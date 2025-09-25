## Lasso



library(tidyverse)
library(readxl)
library(wcfish)
library(sf)
library(zoo)
library(glmnet)


#load catch data

load(here::here("data","fisheries","cali_catch_detrend_2.rda"))

#load weather data
load(here::here("data","environmental","block_beuti.rda"))
load(here::here("data","environmental","block_cuti.rda"))
load(here::here("data","environmental","block_hci.rda"))
load(here::here("data","environmental","block_sst.rda"))
load(here::here("data","environmental","enso_pdo.rda"))
load(here::here('data','fisheries','squid_bio_all.Rdata'))

#load designed fucntions
source(here::here("src","fcn","cw_squid.R"))
source(here::here("src","fcn","ci_fcn.R"))
source(here::here("src","fcn","ut_fcn.R"))
source(here::here("src","fcn","find_m.R"))
source(here::here("src","fcn","inv_ut.R"))


port_cw<-cali_catch %>% 
  filter(species_code=='MSQD') %>% 
  mutate(mt_per_fisher=case_when(mt_per_fisher==Inf~0,
                                 .default=as.numeric(mt_per_fisher)),
         lb_per_fisher=case_when(lb_per_fisher==Inf~0,
                                 .default=as.numeric(lb_per_fisher))) %>% 
  filter(year>=1990) |> 
  nest() %>% 
  mutate(cw_data=pmap(list(spp=species_code,data=data),cw_squid))

###

var_names<-unique(port_cw$cw_data[[1]]$var)
fish_vars<-unique(port_cw$cw_data[[1]]$fish_var)

var_names<-var_names[-which(var_names %in% c('amp_beuti','avg_beuti','f_beuti','s_beuti','w_beuti','amp_cuti','avg_cuti','f_cuti','s_cuti','w_cuti'))]
fish_vars<-fish_vars[which(fish_vars %in% c('mt_per_detrend','rev_per_detrend'))]


combo<-expand_grid(var_names,fish_vars) 

combo_las<-expand_grid(mods=c('rf','lasso'),fish_vars) %>% 
  mutate(mods=as.character(mods))


analysis<-function(index,data,cost,sigma,coverage,controls,m,ra,ut_mod,pred_mod){
  
  
  train<-data[index,]
  
  end<-index[length(index)]+1
  test<-data[end,]
  

    #Make data into a matrix for glmnet
    train_x<-train |> 
      dplyr::select(-c(fish_var,year,fish_value)) |> 
      as.matrix()
    
    train_y<-train |> 
      dplyr::select(fish_value) |> 
      as.matrix()
    
    test_y<-test |> 
      dplyr::select(fish_value) |> 
      as.matrix()
    
    test_x<-test |> 
      dplyr::select(-c(fish_var,year,fish_value)) |> 
      as.matrix()
    
    #Build model
    las_mod<-glmnet(train_x, train_y, alpha = 1,lambda=controls)
    
    #make prediction
    pred_train<- predict(las_mod, newx = train_x)
    
    pred_test<- predict(las_mod, newx = test_x)

    # get payouts as matrix
    payout_train_df<-matrix(NA,nrow=24,ncol=100) |> 
      as.data.frame()
    
    for(i in 1:nrow(payout_train_df)){
      for(j in 1:ncol(payout_train_df)){
        payout_train_df[i,j]<-max(coverage*mean(train_y)-pred_train[i,j],0)
      }
    }
    #get payouts

    
    payout_vec_test<-pred_test |> 
      map_dbl(.f=~max(coverage*mean(train_y)-.x,0))
    
    

  # p_df<-data.frame(x=train$year,fish=train$fish_value,pred=pred_train) |> pivot_longer(cols=c(-x),names_to='mod')
  # ggplot(p_df,aes(x=x,y=value,color=mod))+geom_point()
  # browser()
  residuals <- train$fish_value - pred_train
  
  rmse <- sqrt(colMeans(residuals^2, na.rm = TRUE))
  
  rsq <- 1 - (colSums(residuals^2, na.rm = TRUE) / sum((train$fish_value - mean(train$fish_value))^2, na.rm = TRUE))
  
  prem<-colMeans(payout_train_df,na.rm=TRUE)
  
  
  # return results
  return(tibble(payout_vec_test=list(payout_vec_test),
                rsq=list(rsq),
                prem=list(prem),
                rmse=list(rmse),
                test_value=test$fish_value,
                pred=list(pred_test),
                train_vec=list(pred_train)
  ))
  
}



df<-port_cw$cw_data[[1]] |> 
  filter(fish_var=='mt_per_detrend') |> 
  filter(!var %in% c('amp_beuti','avg_beuti','f_beuti','s_beuti','w_beuti','amp_cuti','avg_cuti','f_cuti','s_cuti','w_cuti','year')) |> 
  pivot_wider(names_from = var, values_from = value) |> 
  drop_na()



tune_map<-function(...,big_data,v,fv,coverage,m,start_year=24,ra,ut_mod,pred_mod){
  input<-list(...)
  controls<-unlist(input)
  
  

    df<-big_data |> 
      filter(fish_var==fv) |> 
      filter(!var %in% c('amp_beuti','avg_beuti','f_beuti','s_beuti','w_beuti','amp_cuti','avg_cuti','f_cuti','s_cuti','w_cuti','year')) |> 
      pivot_wider(names_from = var, values_from = value) |> 
      drop_na()

  
  
  #Create indicies of time splits
  
  
  start_max <- start_year
  end_max   <- nrow(df)-1
  
  vec_list <- map(start_max:end_max, ~ 1:.x)
  
  
  time_out<-vec_list %>% map_df(.f=~analysis(.x,data=df,coverage=coverage,controls=controls,m=m,ut_mod=ut_mod,ra=ra,pred_mod=pred_mod))

  
  
  t_pay<-purrr::transpose(time_out$payout_vec_test)
  t_prem<-purrr::transpose(time_out$prem)
  t_pred<-purrr::transpose(time_out$pred)
  
  #storage df for loops
  rmse_test<-as.data.frame(matrix(NA,ncol=1,nrow=length(controls)))
  u_i_df<-as.data.frame(matrix(NA,ncol=1,nrow=length(controls)))
  
  lr_df<-as.data.frame(matrix(NA,ncol=1,nrow=length(controls)))
  ins_pi_df<-as.data.frame(matrix(NA,ncol=1,nrow=length(controls)))
  
    for( j in 1:nrow(u_i_df)){

      u_i_df[j,1]<-ut_fcn(time_out$test_value,unlist(t_pay[[j]]),unlist(t_prem[[j]]),ra=ra,mod=ut_mod)$ut
      
      rmse_test[j,1]<-sqrt(mean((time_out$test_value-unlist(t_pred[[j]]))^2))
      
     lr_df[j,1]<- sum(unlist(t_pay[[j]]))/sum(unlist(t_prem[[j]]))
     ins_pi_df[j,1]<-sum(unlist(t_prem[[j]])-unlist(t_pay[[j]]))
    }
  

  u_noi<-ut_fcn(time_out$test_value,0,0,ra=ra,mod=ut_mod)$ut
  
  u_rr<-(u_i_df-u_noi)/abs(u_noi)
  
  # get best model and take premium, payouts, training and testing values
  
  min_index<-which(rmse_test==min(rmse_test))
  
  prem_vec<-unlist(t_prem[[min_index]])
  payout_vec=unlist(t_pay[[min_index]])
  test_vec<-unlist(t_pred[[min_index]])
  
  best_train_rsq<-mean(unlist(purrr::transpose(time_out$rsq)[[min_index]]))
  best_train_rmse<-mean(unlist(purrr::transpose(time_out$rmse)[[min_index]]))
  
  return(tibble(lambda=controls,
                u_rr=u_rr$V1,
                ins_pi=ins_pi_df$V1,
                best_train_rmse=best_train_rmse,
                best_train_rsq=best_train_rsq,
                lr=lr_df$V1,
                rmse_test=rmse_test$V1,
                pred_mod=pred_mod,
                prem_vec=list(prem_vec),
                payout_vec=list(payout_vec))
  )
}




time_lasso_df<-tune_map(lambda_grid,v='blank',coverage=1,m=1,fv='mt_per_detrend',big_data = port_cw$cw_data[[1]],ra=0.01,ut_mod='log',pred_mod='lasso')

best_lasso<-time_lasso_df[which.min(time_lasso_df$rmse_test),]

## plot 


max_len <- max(length(df$fish_value), length(best_rf$train_vec[[1]]), length(best_rf$test_vec[[1]]))

# Pad each vector
fish_value <- c(df$fish_value, rep(NA, max_len - length(df$fish_value)))
train <- c(best_rf$train_vec[[1]][[1]], rep(NA, max_len - length(best_rf$train_vec[[1]][[1]])))
test <- c(rep(NA, max_len - length(best_rf$test_vec[[1]][[1]])),best_rf$test_vec[[1]][[1]])



plot_df <- data.frame(fish_value, train, test,year=df$year) |> 
  pivot_longer(!c(year,fish_value),names_to = 'model',values_to = 'value')

ggplot(plot_df)+geom_line(aes(x=year,y=fish_value))+
  geom_point(aes(x=year,y=value,color=model))

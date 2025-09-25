#### Analysis using time series ML methods and burn analsyis for premium




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
  
  
  if(pred_mod=='lm')
  {
    
    mod<-lm(fish_value~value,data=train)
    
    
    tick<-coefficients(mod)[2]
    
    
    
    strike<-((mean(data$fish_value)-coefficients(mod)[1])/tick)*coverage   #coverage is how much to protect 0.65 is way below average, 0.9 is small, 1 is average
    
    pred_train<-predict(mod, newdata = train)
    
    pred_test<-predict(mod, newdata = test)
    
    
    # get payouts
    
    payout_vec_train<-train$value %>% map_dbl(.f=~max(tick*(strike-.x),0))
    payout_vec_test<-test$value %>% map_dbl(.f=~max(tick*(strike-.x),0))
    
  }else if(pred_mod=='lasso'){
    
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
    las_mod<-cv.glmnet(train_x, train_y, alpha = 1,nfolds=5)
    
    #make prediction
    pred_train<- predict(las_mod, newx = train_x, s = "lambda.min")
    
    pred_test<- predict(las_mod, newx = test_x, s = "lambda.min")
    
    
    #get payouts
    payout_vec_train<-pred_train |> 
      map_dbl(.f=~max(coverage*mean(train_y)-.x,0))
    
    payout_vec_test<-pred_test |> 
      map_dbl(.f=~max(coverage*mean(train_y)-.x,0))
    
    
    
  }else if(pred_mod=='rf'){
    
    exclude_vars <- c("year", "fish_var","oni","chci","pdo","sti","relax","sp_cuti")
    
    # Create formula: response ~ all other columns except those in exclude_vars
    predictors <- setdiff(names(train), c("fish_value", exclude_vars))
    form <- reformulate(predictors, response = "fish_value")
    
    rf_mod <- ranger::ranger(formula=form, 
                             data = train, 
                             importance = 'impurity', 
                             num.trees = 1000, 
                             mtry = controls[1], 
                             min.node.size = controls[2],
                             max.depth = controls[3])
    
    pred_train<-predict(rf_mod, data = train)$predictions
    
    pred_test<-predict(rf_mod, data = test)$predictions
    
    payout_vec_train <- pred_train %>% map_dbl(.f=~max(coverage*mean(train$fish_value)-.x,0))
    
    payout_vec_test <- pred_test %>% map_dbl(.f=~max(coverage*mean(train$fish_value)-.x,0))
    
  } else if(pred_mod=='svm'){
    
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
    
    
    svm_fit <- ksvm(
      train_x, train_y,
      type = "eps-svr",       # regression (use "C-svc" for classification)
      kernel = "rbfdot",
      C = controls[1],                  # cost
      kpar = list(sigma = controls[2]) # RBF width parameter
    )
    
    pred_test <- predict(svm_fit, test_x)
    pred_train<-predict(svm_fit,train_x)
    
    payout_vec_train <- pred_train %>% map_dbl(.f=~max(coverage*mean(train$fish_value)-.x,0))
    
    payout_vec_test <- pred_test %>% map_dbl(.f=~max(coverage*mean(train$fish_value)-.x,0))
    
  }
  # p_df<-data.frame(x=train$year,fish=train$fish_value,pred=pred_train) |> pivot_longer(cols=c(-x),names_to='mod')
  # ggplot(p_df,aes(x=x,y=value,color=mod))+geom_point()
  # browser()
  residuals <- train$fish_value - pred_train
  
  rmse <- sqrt(mean(residuals^2, na.rm = TRUE))
  
  rsq <- 1 - (sum(residuals^2, na.rm = TRUE) / sum((train$fish_value - mean(train$fish_value))^2, na.rm = TRUE))
  
  prem<-mean(payout_vec_train,na.rm=TRUE)
  
  
  # return results
  return(tibble(payout_vec_test=payout_vec_test,
                rsq=rsq,
                prem=prem,
                rmse=rmse,
                test_value=test$fish_value,
                pred=as.numeric(pred_test),
                train_vec=list(pred_train)
  ))
  
}



df<-port_cw$cw_data[[1]] |> 
  filter(fish_var=='mt_per_detrend') |> 
  filter(!var %in% c('amp_beuti','avg_beuti','f_beuti','s_beuti','w_beuti','amp_cuti','avg_cuti','f_cuti','s_cuti','w_cuti','year')) |> 
  pivot_wider(names_from = var, values_from = value) |> 
  drop_na()

## Order has to be mtry, min.node, max.depth
mtry <- c(2,3,4,5)
min.node <- seq(5,10)
max.depth<-seq(2,4)

rf_grid <- expand.grid(mtry=mtry,min.node=min.node,max.depth=max.depth)

# Convert to tibble for tune_grid
rf_grid <- as_tibble(rf_grid)

tune_map<-function(...,big_data,v,fv,coverage,m,start_year=24,ra,ut_mod,pred_mod){
  input<-list(...)
  controls<-unlist(input)
  
  
  if(pred_mod=='lm'){
    df<-big_data %>% 
      filter(var==v) %>% 
      filter(fish_var==fv) |> 
      drop_na()
    
  } else {
    df<-big_data |> 
      filter(fish_var==fv) |> 
      filter(!var %in% c('amp_beuti','avg_beuti','f_beuti','s_beuti','w_beuti','amp_cuti','avg_cuti','f_cuti','s_cuti','w_cuti','year')) |> 
      pivot_wider(names_from = var, values_from = value) |> 
      drop_na()
  }
  
  
  #Create indicies of time splits
  
  
  start_max <- start_year
  end_max   <- nrow(df)-1
  
  vec_list <- map(start_max:end_max, ~ 1:.x)
  
  
  time_out<-vec_list %>% map_df(.f=~analysis(.x,data=df,coverage=coverage,controls=controls,m=m,ut_mod=ut_mod,ra=ra,pred_mod=pred_mod))
  
  u_i<-ut_fcn(time_out$test_value,time_out$payout_vec_test,time_out$prem,ra=ra,mod=ut_mod)$ut
  u_noi<-ut_fcn(time_out$test_value,0,0,ra=ra,mod=ut_mod)$ut
  
  u_rr<-(u_i-u_noi)/abs(u_noi)
  
  lr=sum(time_out$payout_vec_test)/sum(time_out$prem)
  ins_pi<-sum(time_out$prem)-sum(time_out$payout_vec_test)
  
  rmse_test<-sqrt(mean((time_out$test_value-time_out$pred)^2))
  
  train_vec=unlist(time_out$train_vec[1])
  
  return(tibble(u_rr=u_rr,
                ins_pi=ins_pi,
                rsq=mean(time_out$rsq,na.rm=TRUE),
                rmse=mean(time_out$rmse,na.rm=TRUE),
                lr=lr,
                rmse_test=rmse_test,
                pred_mod=pred_mod,
                train_vec=list(train_vec),
                test_vec=list(time_out$pred),
                prem_vec=list(time_out$prem),
                payout_vec=list(time_out$payout_vec_test))
  )
}


time_rf_df<-rf_grid %>% 
  mutate(results=pmap(across(everything(.)),~tune_map(...,v='blank',coverage=1,m=1,fv='mt_per_detrend',big_data=port_cw$cw_data[[1]],ra=0.01,ut_mod='log',pred_mod='rf'))) |> 
  unnest_wider(results)



best_rf<-time_rf_df[which.min(time_rf_df$rmse_test),]

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

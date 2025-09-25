### Do Dalhaus 2018 without any regard for data science principles

### Run the linear model assessment on port data

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
  filter(year>=1988) |> 
  nest() %>% 
  mutate(cw_data=pmap(list(spp=species_code,data=data),cw_squid))

###

var_names<-unique(port_cw$cw_data[[1]]$var)
fish_vars<-unique(port_cw$cw_data[[1]]$fish_var)

var_names<-var_names[-which(var_names %in% c('amp_beuti','avg_beuti','f_beuti','s_beuti','w_beuti','amp_cuti','avg_cuti','f_cuti','s_cuti','w_cuti'))]
fish_vars<-fish_vars[which(fish_vars %in% c('mt_detrend','mt_per_detrend','rev_detrend','rev_per_detrend'))]


combo_lm<-expand_grid(var_names,fish_vars) 

combo_las<-expand_grid(mods=c("lasso","rf"),fish_vars)

analysis<-function(v,fv,big_data,coverage,m,ra,ut_mod,pred_mod){
  

  
  if(pred_mod=='lm'){
    

  df<-big_data %>% 
    filter(var==v) %>% 
    filter(fish_var==fv) |> 
    drop_na()
  
  
  
  mod<-lm(fish_value~value,data=df)
  
  
  tick<-coefficients(mod)[2]
  
  
  
  strike<-((mean(df$fish_value)-coefficients(mod)[1])/tick)*coverage   #coverage is how much to protect 0.65 is way below average, 0.9 is small, 1 is average
  
  # assess through rmse
  residuals <- df$fish_value - predict(mod, newx = df)
  
  
  # get premium
  
  payout_vec<-df$value %>% map_dbl(.f=~max(tick*(strike-.x),0))
  
  } else if(pred_mod=='lasso'){
    
    df<-big_data |> 
      filter(fish_var==fv) |> 
      filter(!var %in% c('amp_beuti','avg_beuti','f_beuti','s_beuti','w_beuti','amp_cuti','avg_cuti','f_cuti','s_cuti','w_cuti','year')) |> 
      pivot_wider(names_from = var, values_from = value) |> 
      drop_na()
    
    df_x<-df |> 
      dplyr::select(-c(fish_var,year,fish_value)) |> 
      as.matrix()
    
    df_y<-df |> 
      dplyr::select(fish_value) |> 
      as.matrix()
    
    las_mod<-cv.glmnet(df_x, df_y, alpha = 1,nfolds=5)
    
    pred<- predict(las_mod, newx = df_x, s = "lambda.1se")
    
    payout_vec<-pred |> 
      map_dbl(.f=~max(coverage*mean(df_y)-.x,0))
    
    residuals <- df_y - pred
  } else if(pred_mod=='rf'){
    
    df<-big_data |> 
      filter(fish_var==fv) |> 
      filter(!var %in% c('amp_beuti','avg_beuti','f_beuti','s_beuti','w_beuti','amp_cuti','avg_cuti','f_cuti','s_cuti','w_cuti','year')) |> 
      pivot_wider(names_from = var, values_from = value) |> 
      drop_na()

    exclude_vars <- c("year", "fish_var")
    
    # Create formula: response ~ all other columns except those in exclude_vars
    predictors <- setdiff(names(df), c("fish_value", exclude_vars))
    form <- reformulate(predictors, response = "fish_value")
    
    rf_mod <- ranger::ranger(formula=form, data = df, importance = 'impurity', num.trees = 1000, mtry = 3, min.node.size = 10)
    
    pred<-predict(rf_mod, data = df)$predictions
    
    payout_vec <- pred %>% map_dbl(.f=~max(coverage*mean(df$fish_value)-.x,0))
    
    residuals <- df$fish_value - pred
  } else {
    stop("Invalid prediction model specified.")
  }
  
  rmse <- sqrt(mean(residuals^2, na.rm = TRUE))
  
  rsq <- 1 - (sum(residuals^2, na.rm = TRUE) / sum((df$fish_value - mean(df$fish_value))^2, na.rm = TRUE))
  
  prem<-mean(payout_vec,na.rm=TRUE)
  
  u_out<-ut_fcn(df$fish_value,payout_vec,prem,m=m,mod=ut_mod)
  
  u_i<-u_out$ut
  pi_i<-u_out$profit
  
  ce_i<-inv_ut(u_i,ra=ra,mod=ut_mod)
  
  r_i<-pi_i-ce_i
  
  u_out<-ut_fcn(df$fish_value,0,0,mod=ut_mod)
  u_noi<-u_out$ut
  pi_noi<-u_out$profit
  
  ce_noi<-inv_ut(u_noi,ra=ra,mod=ut_mod)
  
  r_noi<-pi_noi-ce_noi
  
  
  u_rr<-(u_i-u_noi)/abs(u_noi)
  
  # get insurance profits
  ins_pi<-prem*nrow(df)-sum(payout_vec)
  
  # find the m that makes fishers indifferent
  
  m_out <- tryCatch({
    result <- uniroot(find_m,
                      interval = c(0.01, 10),
                      data = df$fish_value,
                      payout_vec = payout_vec,
                      prem = prem,
                      ra = ra,
                      mod = ut_mod)
    result$root  # extract root only if uniroot succeeds
  }, error = function(e) {
    #message("uniroot failed: ", e$message)
    NA  # or another default/fallback value
  })
  
  # If there were no prior payouts then fishers are indifferent
  if(prem==0){
    m_out=1
  }
  

  
  # get risk premium
  risk_p<-(r_i-r_noi)/abs(r_noi)
  
  lr<-sum(payout_vec)/(prem*nrow(df))
  
  # return results
  return(data.frame(u_rr,
                    rsq=rsq,
                    prem=prem,
                    risk_p=risk_p,
                    m_out=m_out,
                    ins_pi=ins_pi,
                    rmse=rmse,
                    lr=lr))
  
}


## split bootstrap



dal_df_lm<-combo_lm %>% 
  mutate(results=map2(.x=var_names,.y=fish_vars,~analysis(.x,.y,big_data=port_cw$cw_data[[1]],coverage=1,m=1,ra=0.01,ut_mod='log',pred_mod='lm'))) |> 
  unnest_wider(results)


dal_df<-combo_las %>%
  mutate(results=map2(.x=mods,.y=fish_vars,~analysis(v="blank",.y,big_data=port_cw$cw_data[[1]],coverage=1,m=1,ra=0.01,ut_mod='log',pred_mod=.x))) |>
  unnest_wider(results)


ggplot(dal_df_lm,aes(y=u_rr,x=var_names))+
  geom_col()+
  scale_y_continuous(labels=scales::percent_format(accuracy=1))+
  labs(x='Weather Variable',y='Utility Improvement')+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  facet_wrap(~fish_vars)
  
ggplot(dal_df,aes(y=u_rr,x=mods))+
  geom_col()+
  scale_y_continuous(labels=scales::percent_format(accuracy=1))+
  labs(x='Model Type',y='Utility Improvement')+
  facet_wrap(~fish_vars)
  
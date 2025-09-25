### Use index modelling to form premium 

library(tidyverse)
library(readxl)
library(wcfish)
library(sf)
library(zoo)
library(glmnet)
library(copula)

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

sim_cop <- function(fitted_copula, orig_data, n_sim = 1000, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  
  # Step 1: Simulate from the fitted copula (uniform margins)
  u_sim <- rCopula(n_sim, fitted_copula)
  
  # Step 2: Apply inverse empirical CDFs
  real_sim <- matrix(NA, nrow = n_sim, ncol = ncol(u_sim))
  colnames(real_sim) <- colnames(orig_data)
  
  for (j in seq_len(ncol(u_sim))) {
    real_sim[, j] <- quantile(orig_data[, j], probs = u_sim[, j], type = 1)
  }
  
  return(as.data.frame(real_sim))
}


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

combo_las<-expand_grid(mods=c('lasso'),fish_vars) %>% 
  mutate(mods=as.character(mods))

analysis<-function(index,data,coverage,m,ra,ut_mod,pred_mod,sim_data){
  
  
  train<-data[index,]
  test<-data[-index,]
  
  
  if(pred_mod=='lm')
  {
    
    mod<-lm(fish_value~value,data=train)
    
    
    tick<-coefficients(mod)[2]
    
    
    
    strike<-((mean(data$fish_value)-coefficients(mod)[1])/tick)*coverage   #coverage is how much to protect 0.65 is way below average, 0.9 is small, 1 is average
    
    pred_train<-predict(mod, newdata = train)
    
    pred_test<-predict(mod, newdata = test)
    
    prem_sim<-predict(mod, newdata = sim_data)
    
    # get payouts
    
    payout_vec_train<-train$value %>% map_dbl(.f=~max(tick*(strike-.x),0))
    payout_vec_test<-test$value %>% map_dbl(.f=~max(tick*(strike-.x),0))
    
    payout_sim<-prem_sim |> 
      map_dbl(.f=~max(coverage*mean(train$fish_value)-.x,0))
    
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
    
    
    prem_sim<-predict(las_mod,newx=as.matrix(sim_data),s="lambda.min")
    #get payouts
    payout_vec_train<-pred_train |> 
      map_dbl(.f=~max(coverage*mean(train_y)-.x,0))
    
    payout_vec_test<-pred_test |> 
      map_dbl(.f=~max(coverage*mean(train_y)-.x,0))
    
    payout_sim<-prem_sim |> 
      map_dbl(.f=~max(coverage*mean(train_y)-.x,0))
    
    
  }else if(pred_mod=='rf'){
    
    exclude_vars <- c("year", "fish_var")
    
    # Create formula: response ~ all other columns except those in exclude_vars
    predictors <- setdiff(names(train), c("fish_value", exclude_vars))
    form <- reformulate(predictors, response = "fish_value")
    
    rf_mod <- ranger::ranger(formula=form, data = train, importance = 'impurity', num.trees = 1000, mtry = 3, min.node.size = 10)
    
    pred_train<-predict(rf_mod, data = train)$predictions
    
    pred_test<-predict(rf_mod, data = test)$predictions
    
    payout_vec_train <- pred_train %>% map_dbl(.f=~max(coverage*mean(train$fish_value)-.x,0))
    
    payout_vec_test <- pred_test %>% map_dbl(.f=~max(coverage*mean(train$fish_value)-.x,0))
    
    prem_sim<-predict(rf_mod,data=sim_data)$predictions
    
    payout_sim<-prem_sim |> 
      map_dbl(.f=~max(coverage*mean(train$fish_value)-.x,0))
  }
  
  
  
  residuals <- train$fish_value - pred_train
  
  rmse <- sqrt(mean(residuals^2, na.rm = TRUE))
  
  rsq <- 1 - (sum(residuals^2, na.rm = TRUE) / sum((train$fish_value - mean(train$fish_value))^2, na.rm = TRUE))
  
  prem<-mean(payout_sim,na.rm=TRUE)
  
  
  res_test<-test$fish_value - pred_test
  
  rmse_test <- sqrt(mean(res_test^2, na.rm = TRUE))
  
  rsq_test <- 1 - (sum(res_test^2, na.rm = TRUE) / sum((test$fish_value - mean(test$fish_value))^2, na.rm = TRUE))
  #Get test utilities and risk premiums
  
  
  u_out<-ut_fcn(test$fish_value,payout_vec_test,prem,mod=ut_mod)
  
  u_i_test<-u_out$ut
  pi_i_test<-u_out$profit
  
  ce_i<-inv_ut(u_i_test,ra=ra,mod=ut_mod)
  
  r_i_test<-pi_i_test-ce_i
  
  u_out<-ut_fcn(test$fish_value,0,0,mod=ut_mod)
  u_noi_test<-u_out$ut
  pi_noi_test<-u_out$profit
  
  ce_noi<-inv_ut(u_noi_test,ra=ra,mod=ut_mod)
  
  r_noi_test<-pi_noi_test-ce_noi
  
  
  test_rr<-(u_i_test-u_noi_test)/abs(u_noi_test)
  
  
  
  # get insurance profits
  ins_pi<-prem*nrow(test)-sum(payout_vec_test)
  
  #find the m that makes fishers indifferent
browser()
  m_out <- tryCatch({
    result <- uniroot(find_m,
                      interval = c(0.01, 10),
                      data = test$fish_value,
                      payout_vec = payout_vec_test,
                      prem_in = prem,
                      ra = ra,
                      mod = ut_mod,
                      u_noi=u_noi_test)
    result$root  # extract root only if uniroot succeeds
  }, error = function(e) {
    #message("uniroot failed: ", e$message)
    NA  # or another default/fallback value
  })
  
  # If there were no prior payouts then fishers are indifferent
  if(prem==0){
    m_out=1
  }
  m_out=1
  
  # get rsq
  
  # get risk premium
  risk_p<-(r_i_test-r_noi_test)/abs(r_noi_test)
  
  lr<-sum(payout_vec_test)/(prem*nrow(test))
  
  if(prem*nrow(test)==0){
    lr=NA
  }
  
  # return results
  return(data.frame(test_rr,
                    rsq=rsq,
                    prem=prem,
                    risk_p=risk_p,
                    m_out=m_out,
                    ins_pi=ins_pi,
                    rmse=rmse,
                    rmse_test=rmse_test,
                    rsq_test=rsq_test,
                    lr=lr
  ))
  
}


## split bootstrap

boot<-function(v,fv,big_data,coverage=1,m=1,split_t=25,ra,ut_mod,pred_mod){
  
  
  if(pred_mod=='lm'){
    df<-big_data %>% 
      filter(var==v) %>% 
      filter(fish_var==fv) |> 
      drop_na()
    
    #Just use normal for now
    v_dist<-fitdistrplus::fitdist(df$value,'norm',method='mle')
    estimated_mean <- v_dist$estimate["mean"]
    estimated_sd <- v_dist$estimate["sd"]
    sim <- rnorm(1000, mean = estimated_mean, sd = estimated_sd)
    
    sim_data <- data.frame(value = sim, var = v, fish_var = fv)
    
  } else if(pred_mod=='lasso'){
    df<-big_data |> 
      filter(fish_var==fv) |> 
      filter(!var %in% c('amp_beuti','avg_beuti','f_beuti','s_beuti','w_beuti','amp_cuti','avg_cuti','f_cuti','s_cuti','w_cuti','year')) |> 
      pivot_wider(names_from = var, values_from = value) |> 
      drop_na()
    
    
    cop_dat<-df |> 
      select(!c(year,fish_var,fish_value)) |> 
      as.matrix()
    
    u <- pobs(cop_dat)  # rank transform to uniform
    
    # Fit different copulas
    fit_t     <- fitCopula(tCopula(dim = ncol(u)), u, method = "ml")
    sim_data<-sim_cop(fit_t@copula,cop_dat,n_sim=1000)
    
  }else if(pred_mod=='rf'){
    df<-big_data |> 
      filter(fish_var==fv) |> 
      filter(!var %in% c('amp_beuti','avg_beuti','f_beuti','s_beuti','w_beuti','amp_cuti','avg_cuti','f_cuti','s_cuti','w_cuti','year')) |> 
      pivot_wider(names_from = var, values_from = value) |> 
      drop_na()
    
    cop_dat<-df |> 
      select(!c(year,fish_var,fish_value)) |> 
      as.matrix()
    
    u <- pobs(cop_dat)  # rank transform to uniform
    
    # Fit different copulas
    fit_t     <- fitCopula(tCopula(dim = ncol(u)), u, method = "ml")
    sim_data<-sim_cop(fit_t@copula,cop_dat,n_sim=1000)
    
  }
  
  
  
  bootstrap_samples <- replicate(
    1000,
    sample(nrow(df), size = split_t, replace = FALSE),
    simplify = FALSE
  )
  
  
  boot_out<-bootstrap_samples %>% map_df(.f=~analysis(.x,data=df,coverage=coverage,m=m,ut_mod=ut_mod,ra=ra,pred_mod=pred_mod,sim_data=sim_data))
  
  
  u_ci<-ci_fcn(boot_out$test_rr,alpha=0.05)
  m_ci<-ci_fcn(boot_out$m_out,alpha=0.05)
  r_ci<-ci_fcn(boot_out$risk_p,alpha=0.05)
  ins_ci<-ci_fcn(boot_out$ins_pi,alpha=0.05)
  rsq_ci<-ci_fcn(boot_out$rsq,alpha=0.05)
  rmse_ci<-ci_fcn(boot_out$rmse,alpha=0.05)
  lr_ci<-ci_fcn(boot_out$lr,alpha=0.05)
  rsq_test_ci<-ci_fcn(boot_out$rsq_test,alpha=0.05)
  rmse_test_ci<-ci_fcn(boot_out$rmse_test,alpha=0.05)
  
  
  
  return(data.frame(u_rr=mean(boot_out$test_rr,na.rm=TRUE),
                    urr_ci_hi=u_ci$hi,
                    urr_ci_lo=u_ci$lo,
                    m_out=mean(boot_out$m_out,na.rm=TRUE),
                    m_out_hi=m_ci$hi,
                    m_out_lo=m_ci$lo,
                    r_rr=mean(boot_out$risk_p,na.rm=TRUE),
                    r_ci_hi=r_ci$hi,
                    r_ci_lo=r_ci$lo,
                    ins_pi=mean(boot_out$ins_pi,na.rm=TRUE),
                    ins_ci_hi=ins_ci$hi,
                    ins_ci_lo=ins_ci$lo,
                    rsq=mean(boot_out$rsq,na.rm=TRUE),
                    rsq_hi=rsq_ci$hi,
                    rsq_lo=rsq_ci$lo,
                    rmse=mean(boot_out$rmse,na.rm=TRUE),
                    rmse_hi=rmse_ci$hi,
                    rmse_lo=rmse_ci$lo,
                    lr=mean(boot_out$lr,na.rm=TRUE),
                    lr_hi=lr_ci$hi,
                    lr_lo=lr_ci$lo,
                    rsq_test=mean(boot_out$rsq_test,na.rm=TRUE),
                    rsq_test_hi=rsq_test_ci$hi,
                    rsq_test_lo=rsq_test_ci$lo,
                    rmse_test=mean(boot_out$rmse_test,na.rm=TRUE),
                    rmse_test_hi=rmse_test_ci$hi,
                    rmse_test_lo=rmse_test_ci$lo))
}


boot_n_df<-combo %>% 
  mutate(results=map2(.x=var_names,.y=fish_vars,~boot(.x,.y,big_data=port_cw$cw_data[[1]],ra=0.01,ut_mod='log',pred_mod='lm'))) |> 
  unnest_wider(results)


boot_las_df<-combo_las %>% 
  mutate(results=map2(.x=mods,.y=fish_vars,~boot(v='blank',.y,big_data=port_cw$cw_data[[1]],ra=0.01,ut_mod='log',pred_mod=.x))) |> 
  unnest_wider(results)


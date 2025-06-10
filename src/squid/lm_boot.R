### Run the linear model assessment on port data

library(tidyverse)
library(readxl)
library(wcfish)
library(sf)
library(zoo)


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

example<-port_cw$cw_data[[1]] %>% 
  filter(var=='sti') %>% 
  filter(fish_var=='landings_mt')

var_names<-unique(port_cw$cw_data[[1]]$var)
fish_vars<-unique(port_cw$cw_data[[1]]$fish_var)

var_names<-var_names[-which(var_names %in% c('amp_beuti','avg_beuti','f_beuti','s_beuti','w_beuti','amp_cuti','avg_cuti','f_cuti','s_cuti','w_cuti'))]
fish_vars<-fish_vars[-which(fish_vars %in% c('landings_lb','lb_per_fisher'))]


combo<-expand_grid(var_names,fish_vars) 

premium<-function(boot_data,strike,tick){
  
  # burn rate
  
  draws<-sample(boot_data,1000,replace=TRUE)
  
  payout<-draws %>% map_dbl(.f=~max(tick*(strike-.x),0))
  
  
  p=mean(payout)
  
  return(p)
}

inv_ut<-function(ra,eu,mod){
  if(mod=='cara'){
    return(-log(1-eu*ra)/ra)
  } else if (mod=='log'){
    return(exp(eu))
  } else if (mod=='power'){
    return(eu*(1-ra)^(1/(1-ra)))
  }
  
}

ut_fcn<-function(value,payout_vec,premium,m=1,ra=0.1,mod='cara'){

  
  profit=payout_vec+value-premium*m
  
  profit=profit/max(value) #scale to make sure everything matches?
  
  if(mod=='cara'){
    ut=-mean((1-exp(-ra*profit))/ra,na.rm=TRUE)
  } else if (mod=='log'){
    profit[which(profit<0)]=0.00001 # Make negative profits really small
    ut<-mean(log(profit),na.rm=TRUE)
  }
  
  return(data.frame(ut=ut,profit=mean(profit)))
}

find_m<-function(m_in,data,payout_vec,prem,ra,mod){
  # m is the multiplier for the premium
  # we want to find the m that maximizes the utility

  u_out<-ut_fcn(data,payout_vec,prem,m=m_in,ra=ra,mod=mod)[1]-ut_fcn(data,0,0,m=m_in,ra=ra,mod=mod)[1]
  
  return(u_out$ut)
}

analysis<-function(index,data,coverage,m,ra,ut_mod){
  train<-data[index,]
  test<-data[-index,]
  
  # run regression on train data
  

  mod<-lm(fish_value~value,data=train)
  
  
  tick<-coefficients(mod)[2]
  

  
  strike<-((mean(data$fish_value)-coefficients(mod)[1])/tick)*coverage   #coverage is how much to protect 0.65 is way below average, 0.9 is small, 1 is average
  
  
  # get premium
  prem<-premium(train$value,strike=strike,tick=tick)
  payout_vec<-train$value %>% map_dbl(.f=~max(tick*(strike-.x),0))
  
  u_out<-ut_fcn(train$fish_value,payout_vec,prem,m=m,mod=ut_mod)
  u_i<-u_out$ut
  pi_i<-u_out$profit
  
  r_i<-pi_i-inv_ut(u_i,ra=ra,mod=ut_mod)
  
  
  
  u_out<-ut_fcn(train$fish_value,0,0,mod=ut_mod)
  u_noi<-u_out$ut
  pi_noi<-u_out$profit
  
  r_noi<-pi_noi-inv_ut(u_noi,ra=ra,mod=ut_mod)
  
  train_rr<-(u_i-u_noi)/abs(u_noi)
  
  #Get test utilities and risk premiums
  
  payout_vec<-test$value %>% map_dbl(.f=~max(tick*(strike-.x),0))
  u_out<-ut_fcn(test$fish_value,payout_vec,prem,m=m,mod=ut_mod)
  
  u_i_test<-u_out$ut
  pi_i_test<-u_out$profit
  
  r_i_test<-pi_i_test-inv_ut(u_i_test,ra=ra,mod=ut_mod)
  
  u_out<-ut_fcn(test$fish_value,0,0,mod=ut_mod)
  u_noi_test<-u_out$ut
  pi_noi_test<-u_out$profit
  
  r_noi_test<-pi_noi_test-inv_ut(u_noi_test,ra=ra,mod=ut_mod)
  
  
  test_rr<-(u_i_test-u_noi_test)/abs(u_noi_test)
  
  risk_p<-(r_i_test-r_noi_test)/abs(r_noi_test)
  
 # find the m that makes fishers indifferent

  m_out <- tryCatch({
    result <- uniroot(find_m,
                      interval = c(0.01, 10),
                      data = test$fish_value,
                      payout_vec = payout_vec,
                      prem = prem,
                      ra = ra,
                      mod = ut_mod)
    result$root  # extract root only if uniroot succeeds
  }, error = function(e) {
    #message("uniroot failed: ", e$message)
    NA  # or another default/fallback value
  })

return(data.frame(train_rr,test_rr,rsq=summary(mod)$r.squared,prem=prem,beta=tick,ris_p=risk_p,r_i=r_i_test,r_noi=r_noi_test,m_out=m_out))
}


## split bootstrap

boot<-function(v,fv,big_data,coverage=1,m=1,split_t=25,ra,ut_mod){
  
  df<-big_data %>% 
    filter(var==v) %>% 
    filter(fish_var==fv)
  
  
  bootstrap_samples <- replicate(
    1000,
    sample(nrow(df), size = split_t, replace = FALSE),
    simplify = FALSE
  )
  
  
  boot_out<-bootstrap_samples %>% map_df(.f=~analysis(.x,data=df,coverage=coverage,m=m,ut_mod=ut_mod,ra=ra))

  return(data.frame(avg_rr=mean(boot_out$test_rr),sd_rr=sd(boot_out$test_rr),avg_rsq=mean(boot_out$rsq),sd_rsq=sd(boot_out$rsq),avg_prem=mean(boot_out$prem),sd_prem=sd(boot_out$prem),avg_beta=mean(boot_out$beta),sd_beta=sd(boot_out$beta),risk_p=mean(boot_out$ris_p),m_out=mean(boot_out$m_out,na.rm=TRUE)))
}

boot_df_2<-combo %>% 
  mutate(results=map2(.x=var_names,.y=fish_vars,~boot(.x,.y,big_data=port_cw$cw_data[[1]],ra=0.01,ut_mod='log')))


boot_df_2<-boot_df_2 %>%
  unnest_wider(results)

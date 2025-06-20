### Run the linear model assessment on port data

library(tidyverse)
library(readxl)
library(wcfish)
library(sf)
library(zoo)


#load catch data

load(here::here("data","fisheries","cali_port_detrend.rda"))

#load weather data
load(here::here("data","environmental","block_beuti.rda"))
load(here::here("data","environmental","block_cuti.rda"))
load(here::here("data","environmental","block_hci.rda"))
load(here::here("data","environmental","block_sst.rda"))
load(here::here("data","environmental","enso_pdo.rda"))
load(here::here('data','fisheries','squid_bio_all.Rdata'))

#load designed fucntions
source(here::here("src","fcn","cw_squid_port.R"))


port_cw<-cali_port_catch %>% 
  filter(spp_code=='MSQD' & port_code=='SBA') %>% 
  mutate(mt_per_fisher=case_when(mt_per_fisher==Inf~0,
                                 .default=as.numeric(mt_per_fisher)),
         lb_per_fisher=case_when(lb_per_fisher==Inf~0,
                                 .default=as.numeric(lb_per_fisher))) %>% 
  filter(year>=1988) |> 
  nest() %>% 
  mutate(cw_data=pmap(list(spp=spp_code,port=port_code,data=data),cw_join_port_squid))

###

example<-port_cw$cw_data[[1]] %>% 
  filter(var=='sti') %>% 
  filter(fish_var=='landings_mt')

var_names<-unique(port_cw$cw_data[[1]]$var)
fish_vars<-unique(port_cw$cw_data[[1]]$fish_var)

var_names<-var_names[-which(var_names %in% c('amp_beuti','avg_beuti','f_beuti','s_beuti','w_beuti','amp_cuti','avg_cuti','f_cuti','s_cuti','w_cuti'))]
fish_vars<-fish_vars[-which(fish_vars %in% c('landings_lb','lb_per_fisher'))]


combo<-expand_grid(var_names,fish_vars)


premium<-function(boot_data,strike,tick,m){

  # burn rate
  
draws<-sample(boot_data,1000,replace=TRUE)

payout<-draws %>% map_dbl(.f=~max(tick*(strike-.x),0))

p=mean(payout)*m  #m is the loading factor
  
  return(p)
}

ut_fcn<-function(data,strike,tick,premium,ra=0.1,mod='cara'){

   payout<-data$value %>% map_dbl(.f=~max(tick*(strike-.x),0))
  
  profit=payout+data$fish_value-premium
  
  if(mod=='cara'){
    ut=-mean((1-exp(-ra*profit))/ra,na.rm=TRUE)
  } else if (mod=='log'){
    profit<-profit+abs(min(profit))+1 # Turn negative values positive, but keep relative order
    ut<-mean(log(profit),na.rm=TRUE)
  }
  
  return(ut)
}

analysis<-function(index,data,coverage,m){
  train<-data[index,]
  test<-data[-index,]
  
  # run regression on train data
  
  
  mod<-lm(fish_value~value,data=train)
  
  
  tick<-coefficients(mod)[2]
  
  strike<-((mean(data$fish_value)-coefficients(mod)[1])/tick)*coverage   #coverage is how much to protect 0.65 is way below average, 0.9 is small, 1 is average
  
  
  # get premium
  prem<-premium(train$value,strike=strike,tick=tick,m=m)
  
  u_i<-ut_fcn(train,strike,tick,prem)
  
  u_noi<-ut_fcn(train,0,0,0)
  
  train_rr<-(u_i-u_noi)/u_noi
  
  u_i_test<-ut_fcn(test,strike,tick,prem,mod='log')
  
  u_noi_test<-ut_fcn(test,0,0,0,mod='log')
  
  test_rr<-(u_i_test-u_noi_test)/u_noi_test
  # think about solving for the m that makes them indifferent
  
  return(data.frame(train_rr,test_rr,rsq=summary(mod)$r.squared,prem=prem,beta=tick))
}


## split bootstrap

boot<-function(v,fv,big_data,coverage=1,m=1,split_t=25){

df<-big_data %>% 
  filter(var==v) %>% 
  filter(fish_var==fv)

index<-sample(nrow(df),split_t)

bootstrap_samples <- replicate(
  1000,
  sample(nrow(df), size = split_t, replace = TRUE),
  simplify = FALSE
)


boot_out<-bootstrap_samples %>% map_df(.f=~analysis(.x,data=df,coverage=coverage,m=m))

return(data.frame(avg_rr=mean(boot_out$test_rr),sd_rr=sd(boot_out$test_rr),avg_rsq=mean(boot_out$rsq),sd_rsq=sd(boot_out$rsq),avg_prem=mean(boot_out$prem),sd_prem=sd(boot_out$prem),avg_beta=mean(boot_out$beta),sd_beta=sd(boot_out$beta)))
}

boot_df<-combo %>% 
  mutate(results=map2(.x=var_names,.y=fish_vars,~boot(.x,.y,big_data=port_cw$cw_data[[1]])))


boot_df<-boot_df %>%
  unnest_wider(results)

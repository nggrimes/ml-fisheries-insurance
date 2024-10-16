### Script to run lasso regression on cali state wide data

library(tidyverse)
library(glmnet)

library(readxl)
library(wcfish)
library(sf)
library(zoo)
library(tidymodels)


#load catch data

load(here::here("data","fisheries","cali_port_detrend.rda"))

#load weather data
load(here::here("data","environmental","block_beuti.rda"))
load(here::here("data","environmental","block_cuti.rda"))
load(here::here("data","environmental","block_hci.rda"))
load(here::here("data","environmental","block_sst.rda"))
load(here::here("data","environmental","enso_pdo.rda"))

#load designed fucntions
source(here::here("src","fcn","port_cw_join.R"))
source(here::here("src","fcn","lasso_fcn_tm.R"))
source(here::here("src","fcn","utility_test.R"))
source(here::here("src","fcn","utility_eval.R"))

port_cw<-cali_port_catch %>% 
  group_by(spp_code,port_code) %>%
  mutate(mt_per_fisher=case_when(mt_per_fisher==Inf~0,
                                 .default=as.numeric(mt_per_fisher)),
         lb_per_fisher=case_when(lb_per_fisher==Inf~0,
                                 .default=as.numeric(lb_per_fisher))) %>% 
  mutate(roll_value_usd=rollmean(revenues_usd,3,fill=NA,align="right",na.rm=TRUE),
         roll_landings=rollmean(landings_mt,3,fill=NA,align='right',na.rm=TRUE),
         roll_n_rev=rollmean(rev_per_fisher,3,fill=NA,align='right',na.rm=TRUE),
         roll_n_mt=rollmean(mt_per_fisher,3,fill=NA,align='right',na.rm=TRUE)) %>%
  filter(year>=1988) |> 
  nest() %>% 
  mutate(cw_data=pmap(list(spp=spp_code,port=port_code,data=data),cw_join_port))


port_mt_lasso<-port_cw %>% 
  mutate(model=map2(.x=cw_data,.y="landings_mt",~lasso_fcn_tm(dep_var=.y,data=.x)))

port_rev_lasso<-port_cw %>% 
  mutate(model=map2(.x=cw_data,.y="revenues_usd",~lasso_fcn_tm(dep_var=.y,data=.x)))

port_per_lasso<-port_cw %>% 
  mutate(model=map2(.x=cw_data,.y="rev_per_fisher",~lasso_fcn_tm(dep_var=.y,data=.x)))


# save output
save(port_mt_lasso,port_rev_lasso,port_per_lasso,file=here::here("data","output","port_lasso_models.rda"))

# Get utility testing improvement

port_mt_lasso_ut<-port_mt_lasso %>% 
  mutate(u_eval=pmap(list(data=cw_data,mod=model,var_name="landings_mt"),utility_eval)) %>% 
  hoist(u_eval,"test_u_rr","prem_vec","l_val") |> 
  select(-model) #save space by dropping model

port_rev_lasso_ut<-port_rev_lasso %>% 
  mutate(u_eval=pmap(list(data=cw_data,mod=model,var_name="revenues_usd"),utility_eval)) %>% 
  hoist(u_eval,"test_u_rr","prem_vec","l_val") |> 
  select(-model) #save space by dropping model

port_per_lasso_ut<-port_per_lasso %>%
  mutate(u_eval=pmap(list(data=cw_data,mod=model,var_name="rev_per_fisher"),utility_eval)) %>% 
  hoist(u_eval,"test_u_rr","prem_vec","l_val") |> 
  select(-model) #save space by dropping model

# save output
save(port_mt_lasso_ut,port_rev_lasso_ut,port_per_lasso_ut,file=here::here("data","output","port_lasso_ut.rda"))


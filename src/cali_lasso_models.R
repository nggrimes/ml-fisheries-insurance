### Script to run lasso regression on cali state wide data

library(tidyverse)
library(glmnet)

library(readxl)
library(wcfish)
library(sf)
library(zoo)
library(tidymodels)


#load catch data

load(here::here("data","fisheries","cali_catch_detrend.rda"))

#load weather data
load(here::here("data","environmental","block_beuti.rda"))
load(here::here("data","environmental","block_cuti.rda"))
load(here::here("data","environmental","block_hci.rda"))
load(here::here("data","environmental","block_sst.rda"))
load(here::here("data","environmental","enso_pdo.rda"))

#load designed fucntions
source(here::here("src","fcn","cw_join_cali.R"))
source(here::here("src","fcn","lasso_fcn_tm.R"))
source(here::here("src","fcn","utility_test.R"))
source(here::here("src","fcn","utility_eval.R"))

cali_cw<-cali_catch %>% 
  group_by(species_code) %>%
  mutate(mt_per_fisher=case_when(mt_per_fisher==Inf~0,
                                 .default=as.numeric(mt_per_fisher)),
         lb_per_fisher=case_when(lb_per_fisher==Inf~0,
                                 .default=as.numeric(lb_per_fisher))) %>% 
  mutate(roll_value_usd=rollmean(value_usd,3,fill=NA,align="right",na.rm=TRUE),
         roll_landings=rollmean(landings_mt,3,fill=NA,align='right',na.rm=TRUE),
         roll_n_rev=rollmean(rev_per_fisher,3,fill=NA,align='right',na.rm=TRUE),
         roll_n_mt=rollmean(mt_per_fisher,3,fill=NA,align='right',na.rm=TRUE)) %>%
  filter(year>=1988) |> 
  nest() %>% 
  mutate(cw_data=map2(.x=species_code,.y=data,~cw_join_cali(.x,.y)))



cali_mt_lasso<-cali_cw %>% 
  mutate(model=map2(.x=cw_data,.y="landings_mt",~lasso_fcn_tm(dep_var=.y,data=.x)))

cali_rev_lasso<-cali_cw %>% 
  mutate(model=map2(.x=cw_data,.y="value_usd",~lasso_fcn_tm(dep_var=.y,data=.x)))

cali_per_fisher_lasso<-cali_cw %>% 
  mutate(model=map2(.x=cw_data,.y="rev_per_fisher",~lasso_fcn_tm(dep_var=.y,data=.x)))

save(cali_mt_lasso,cali_rev_lasso,cali_per_fisher_lasso,file=here::here("data","output","cali_lasso_models.rda"))

cali_mt_lasso_ut<-cali_mt_lasso %>% 
  mutate(u_eval=pmap(list(data=cw_data,mod=model,var_name="landings_mt"),utility_eval)) %>% 
  hoist(u_eval,"test_u_rr","prem_vec","l_val") |> 
  select(-model) #save space by dropping model

cali_rev_lasso_ut<-cali_rev_lasso %>%
  mutate(u_eval=pmap(list(data=cw_data,mod=model,var_name="value_usd"),utility_eval)) %>% 
  hoist(u_eval,"test_u_rr","prem_vec","l_val") |> 
  select(-model) #save space by dropping model

cali_per_fisher_lasso_ut<-cali_per_fisher_lasso %>%
  mutate(u_eval=pmap(list(data=cw_data,mod=model,var_name="fisher_per_rev"),utility_eval)) %>% 
  hoist(u_eval,"test_u_rr","prem_vec","l_val") |> 
  select(-model) #save space by dropping model

# save output
save(cali_mt_lasso_ut,cali_rev_lasso_ut,file=here::here("data","output","cali_lasso_ut.rda"))

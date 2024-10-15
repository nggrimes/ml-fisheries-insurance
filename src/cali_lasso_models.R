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
  mutate(lasso_mod_mt=map2(.x=cw_data,.y="mt_detrend",~lasso_fcn_tm(dep_var=.y,data=.x,ra=1,ut_mod='log'))) |> 
  hoist(lasso_mod_mt,"u_rr","scale","premium")

cali_rev_lasso<-cali_cw %>% 
  mutate(lasso_mod_mt=map2(.x=cw_data,.y="rev_detrend",~lasso_fcn_tm(dep_var=.y,data=.x,ra=1,ut_mod='log'))) |> 
  hoist(lasso_mod_mt,"u_rr","scale","premium")

cali_per_fisher_lasso<-cali_cw %>% 
  mutate(lasso_mod_mt=map2(.x=cw_data,.y="per_detrend",~lasso_fcn_tm(dep_var=.y,data=.x,ra=1,ut_mod='log'))) |> 
  hoist(lasso_mod_mt,"u_rr","scale","premium")

save(cali_mt_lasso,cali_rev_lasso,cali_per_fisher_lasso,file=here::here("data","output","cali_lasso_output_detrend.rda"))


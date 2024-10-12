#### Random Forest Implementation

library(tidyverse)
library(ranger)

library(readxl)
library(wcfish)
library(sf)
library(zoo)
library(tidymodels)



load(here::here("data","fisheries","cali_port.rda"))

#load weather data
load(here::here("data","environmental","block_beuti.rda"))
load(here::here("data","environmental","block_cuti.rda"))
load(here::here("data","environmental","block_hci.rda"))
load(here::here("data","environmental","block_sst.rda"))
load(here::here("data","environmental","enso_pdo.rda"))

#load designed fucntions
source(here::here("src","fcn","port_cw_join.R"))
source(here::here("src","fcn","rf_fcn.R"))
source(here::here("src","fcn","utility_test.R"))

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


port_mt_rf<-port_cw %>% 
  mutate(lasso_mod_mt=map2(.x=cw_data,.y="landings_mt",~rf_fcn(dep_var=.y,data=.x,ra=1,ut_mod='log'))) |> 
  hoist(lasso_mod_mt,"u_rr","coverage")

port_rev_rf<-port_cw %>% 
  mutate(lasso_mod_lb=map2(.x=cw_data,.y="revenues_usd",~rf_fcn(dep_var=.y,data=.x,ra=1,ut_mod='log'))) |> 
  hoist(lasso_mod_lb,"u_rr","coverage")

port_per_rf<-port_cw %>% 
  mutate(lasso_mod_n=map2(.x=cw_data,.y="rev_per_fisher",~rf_fcn(dep_var=.y,data=.x,ra=1,ut_mod='log'))) |> 
  hoist(lasso_mod_n,"u_rr","coverage")

save(port_mt_rf,port_rev_rf,port_per_rf,file=here::here("data","output","port_rf_output.rda"))

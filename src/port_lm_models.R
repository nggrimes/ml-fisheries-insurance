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

#load designed fucntions
source(here::here("src","fcn","port_cw_join.R"))
source(here::here("src","fcn","lm_mod_fcn.R"))
source(here::here("src","fcn","pred_fcn.R"))
source(here::here("src","fcn","ins.R"))
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

port_mt_lm<-port_cw %>% 
  mutate(model=map2(.x=cw_data,.y="landings_mt",~lm_mod_fcn(var_name=.y,data=.x)))

port_rev_lm<-port_cw %>% 
  mutate(model=map2(.x=cw_data,.y="revenues_usd",~lm_mod_fcn(var_name=.y,data=.x)))

port_per_lm<-port_cw %>% 
  mutate(model=map2(.x=cw_data,.y="rev_per_fisher",~lm_mod_fcn(var_name=.y,data=.x)))


# save output
save(port_mt_lm,port_rev_lm,port_per_lm,file=here::here("data","output","port_lm_models.rda"))

# Get utility testing improvement

port_mt_lm_ut<-port_mt_lm %>% 
  mutate(u_eval=pmap(list(data=cw_data,mod=model,var_name="landings_mt"),utility_eval_wtp)) %>% 
  hoist(u_eval,"m",'test_u_i','test_u_noi','test_rmse','train_rmse') |> 
  select(-model) #save space by dropping model

port_rev_lm_ut<-port_rev_lm %>% 
  mutate(u_eval=pmap(list(data=cw_data,mod=model,var_name="revenues_usd"),utility_eval_wtp)) %>% 
  hoist(u_eval,"m",'test_u_i','test_u_noi','test_rmse','train_rmse') |> 
  select(-model) #save space by dropping model

port_per_lm_ut<-port_per_lm %>%
  mutate(u_eval=pmap(list(data=cw_data,mod=model,var_name="rev_per_fisher"),utility_eval)) %>% 
  hoist(u_eval,"test_u_rr","prem_vec","l_val") |> 
  select(-model) #save space by dropping model

# save output
save(port_mt_lm_ut,port_rev_lm_ut,file=here::here("data","output","port_lm_ut_wtp.rda"))

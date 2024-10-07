library(tidyverse)
library(readxl)
library(wcfish)
library(sf)
library(zoo)


#load catch data

load(here::here("data","fisheries","cali_catch.rda"))

#load weather data
load(here::here("data","environmental","block_beuti.rda"))
load(here::here("data","environmental","block_cuti.rda"))
load(here::here("data","environmental","block_hci.rda"))
load(here::here("data","environmental","block_sst.rda"))
load(here::here("data","environmental","enso_pdo.rda"))

#load designed fucntions
source(here::here("src","fcn","cw_join_cali.R"))
source(here::here("src","fcn","lm_mod_fcn.R"))
source(here::here("src","fcn","pred_fcn.R"))
source(here::here("src","fcn","ins.R"))
source(here::here("src","fcn","utility.R"))

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

cali_mt_n_sst_lm<-cali_cw %>% 
  mutate(sst_mt_lm=map(.x=cw_data,~lm_mod_fcn(x="avg_sst",y="mt_per_fisher",data=.x))) |> 
  mutate(sst_mt_pred=map2(.x=cw_data,.y=sst_mt_lm,~pred_fcn(input="avg_sst",output="mt_per_fisher",.x,.y))) |> 
  mutate(pre_ins_pay=map2(.x=data,.y=sst_mt_pred,~pre_ins_pay(.x,.y,roll_val = 'roll_n_mt'))) |> 
  mutate(post_ins_pay=map2(.x=data,.y=sst_mt_pred,~post_ins_pay(.x,.y,roll_val='roll_n_mt'))) |> 
  mutate(avg_ins_pay=map2(.x=data,.y=sst_mt_pred,~avg_ins_pay(.x,.y,fish_var='mt_per_fisher'))) |>
  mutate(premium_pre=map(.x=pre_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(premium_post=map(.x=post_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(premium_avg=map(.x=avg_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(ut_pre=pmap(list(data,pre_ins_pay,premium_pre),utility_ins,model="isoelastic",par=1,val='mt_per_fisher')) |> 
  mutate(ut_noins=map(data,utility,model="isoelastic",par=1,val='mt_per_fisher')) |> 
  mutate(ut_post=pmap(list(data,post_ins_pay,premium_post),utility_ins,model="isoelastic",par=1,val='mt_per_fisher')) |> 
  mutate(ut_avg=pmap(list(data,avg_ins_pay,premium_avg),utility_ins,model="isoelastic",par=1,val='mt_per_fisher')) |> 
  mutate(e_ut_pre=map_dbl(ut_pre,~mean(.x))) |> 
  mutate(e_ut_noins=map_dbl(ut_noins,~mean(.x))) |>
  mutate(e_ut_post=map_dbl(ut_post,~mean(.x))) |>
  mutate(e_ut_avg=map_dbl(ut_avg,~mean(.x))) |> 
  mutate(rr_pre=map2_dbl(ut_pre,ut_noins,~(mean(.x)-mean(.y))/mean(.y))) |> 
  mutate(rr_post=map2_dbl(ut_post,ut_noins,~(mean(.x)-mean(.y))/mean(.y))) |>
  mutate(rr_avg=map2_dbl(ut_avg,ut_noins,~(mean(.x)-mean(.y))/mean(.y)))
  

cali_mt_sst_lm<-cali_cw %>% 
  mutate(sst_mt_lm=map(.x=cw_data,~lm_mod_fcn(x="avg_sst",y="landings_mt",data=.x))) |> 
  mutate(sst_mt_pred=map2(.x=cw_data,.y=sst_mt_lm,~pred_fcn(input="avg_sst",output="landings_mt",.x,.y))) |> 
  mutate(pre_ins_pay=map2(.x=data,.y=sst_mt_pred,~pre_ins_pay(.x,.y,roll_val = 'roll_landings'))) |> 
  mutate(post_ins_pay=map2(.x=data,.y=sst_mt_pred,~post_ins_pay(.x,.y,roll_val='roll_landings'))) |> 
  mutate(avg_ins_pay=map2(.x=data,.y=sst_mt_pred,~avg_ins_pay(.x,.y,fish_var='landings_mt'))) |>
  mutate(premium_pre=map(.x=pre_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(premium_post=map(.x=post_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(premium_avg=map(.x=avg_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(ut_pre=pmap(list(data,pre_ins_pay,premium_pre),utility_ins,model="isoelastic",par=1,val='landings_mt')) |> 
  mutate(ut_noins=map(data,utility,model="isoelastic",par=1,val='landings_mt')) |> 
  mutate(ut_post=pmap(list(data,post_ins_pay,premium_post),utility_ins,model="isoelastic",par=1,val='landings_mt')) |> 
  mutate(ut_avg=pmap(list(data,avg_ins_pay,premium_avg),utility_ins,model="isoelastic",par=1,val='landings_mt')) |> 
  mutate(e_ut_pre=map_dbl(ut_pre,~mean(.x))) |> 
  mutate(e_ut_noins=map_dbl(ut_noins,~mean(.x))) |>
  mutate(e_ut_post=map_dbl(ut_post,~mean(.x))) |>
  mutate(e_ut_avg=map_dbl(ut_avg,~mean(.x))) |> 
  mutate(rr_pre=map2_dbl(ut_pre,ut_noins,~(mean(.x)-mean(.y))/mean(.y))) |> 
  mutate(rr_post=map2_dbl(ut_post,ut_noins,~(mean(.x)-mean(.y))/mean(.y))) |>
  mutate(rr_avg=map2_dbl(ut_avg,ut_noins,~(mean(.x)-mean(.y))/mean(.y)))

### Repeat with hci index

cali_mt_n_hci_lm<-cali_cw %>% 
  mutate(hci_mt_lm=map(.x=cw_data,~lm_mod_fcn(x="chci",y="mt_per_fisher",data=.x))) |> 
  mutate(hci_mt_pred=map2(.x=cw_data,.y=hci_mt_lm,~pred_fcn(input="chci",output="mt_per_fisher",.x,.y))) |> 
  mutate(pre_ins_pay=map2(.x=data,.y=hci_mt_pred,~pre_ins_pay(.x,.y,roll_val = 'roll_n_mt'))) |> 
  mutate(post_ins_pay=map2(.x=data,.y=hci_mt_pred,~post_ins_pay(.x,.y,roll_val='roll_n_mt'))) |> 
  mutate(avg_ins_pay=map2(.x=data,.y=hci_mt_pred,~avg_ins_pay(.x,.y,fish_var='mt_per_fisher'))) |>
  mutate(premium_pre=map(.x=pre_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(premium_post=map(.x=post_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(premium_avg=map(.x=avg_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(ut_pre=pmap(list(data,pre_ins_pay,premium_pre),utility_ins,model="isoelastic",par=1,val='mt_per_fisher')) |> 
  mutate(ut_noins=map(data,utility,model="isoelastic",par=1,val='mt_per_fisher')) |> 
  mutate(ut_post=pmap(list(data,post_ins_pay,premium_post),utility_ins,model="isoelastic",par=1,val='mt_per_fisher')) |> 
  mutate(ut_avg=pmap(list(data,avg_ins_pay,premium_avg),utility_ins,model="isoelastic",par=1,val='mt_per_fisher')) |> 
  mutate(e_ut_pre=map_dbl(ut_pre,~mean(.x))) |> 
  mutate(e_ut_noins=map_dbl(ut_noins,~mean(.x))) |>
  mutate(e_ut_post=map_dbl(ut_post,~mean(.x))) |>
  mutate(e_ut_avg=map_dbl(ut_avg,~mean(.x))) |> 
  mutate(rr_pre=map2_dbl(ut_pre,ut_noins,~(mean(.x)-mean(.y))/mean(.y))) |> 
  mutate(rr_post=map2_dbl(ut_post,ut_noins,~(mean(.x)-mean(.y))/mean(.y))) |>
  mutate(rr_avg=map2_dbl(ut_avg,ut_noins,~(mean(.x)-mean(.y))/mean(.y)))
  

cali_mt_hci_lm<-cali_cw %>%
  mutate(hci_mt_lm=map(.x=cw_data,~lm_mod_fcn(x="chci",y="landings_mt",data=.x))) |> 
  mutate(hci_mt_pred=map2(.x=cw_data,.y=hci_mt_lm,~pred_fcn(input="chci",output="landings_mt",.x,.y))) |> 
  mutate(pre_ins_pay=map2(.x=data,.y=hci_mt_pred,~pre_ins_pay(.x,.y,roll_val = 'roll_landings'))) |> 
  mutate(post_ins_pay=map2(.x=data,.y=hci_mt_pred,~post_ins_pay(.x,.y,roll_val='roll_landings'))) |> 
  mutate(avg_ins_pay=map2(.x=data,.y=hci_mt_pred,~avg_ins_pay(.x,.y,fish_var='landings_mt'))) |>
  mutate(premium_pre=map(.x=pre_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(premium_post=map(.x=post_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(premium_avg=map(.x=avg_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(ut_pre=pmap(list(data,pre_ins_pay,premium_pre),utility_ins,model="isoelastic",par=1,val='landings_mt')) |> 
  mutate(ut_noins=map(data,utility,model="isoelastic",par=1,val='landings_mt')) |> 
  mutate(ut_post=pmap(list(data,post_ins_pay,premium_post),utility_ins,model="isoelastic",par=1,val='landings_mt')) |> 
  mutate(ut_avg=pmap(list(data,avg_ins_pay,premium_avg),utility_ins,model="isoelastic",par=1,val='landings_mt')) |> 
  mutate(e_ut_pre=map_dbl(ut_pre,~mean(.x))) |> 
  mutate(e_ut_noins=map_dbl(ut_noins,~mean(.x))) |>
  mutate(e_ut_post=map_dbl(ut_post,~mean(.x))) |>
  mutate(e_ut_avg=map_dbl(ut_avg,~mean(.x))) |> 
  mutate(rr_pre=map2_dbl(ut_pre,ut_noins,~(mean(.x)-mean(.y))/mean(.y))) |> 
  mutate(rr_post=map2_dbl(ut_post,ut_noins,~(mean(.x)-mean(.y))/mean(.y))) |>
  mutate(rr_avg=map2_dbl(ut_avg,ut_noins,~(mean(.x)-mean(.y))/mean(.y)))


### Repeat for enso index
cali_mt_n_enso_lm<-cali_cw |> 
  mutate(enso_mt_lm=map(.x=cw_data,~lm_mod_fcn(x="enso",y="mt_per_fisher",data=.x))) |> 
  mutate(enso_mt_pred=map2(.x=cw_data,.y=enso_mt_lm,~pred_fcn(input="enso",output="mt_per_fisher",.x,.y))) |> 
  mutate(pre_ins_pay=map2(.x=data,.y=enso_mt_pred,~pre_ins_pay(.x,.y,roll_val = 'roll_n_mt'))) |> 
  mutate(post_ins_pay=map2(.x=data,.y=enso_mt_pred,~post_ins_pay(.x,.y,roll_val='roll_n_mt'))) |> 
  mutate(avg_ins_pay=map2(.x=data,.y=enso_mt_pred,~avg_ins_pay(.x,.y,fish_var='mt_per_fisher'))) |>
  mutate(premium_pre=map(.x=pre_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(premium_post=map(.x=post_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(premium_avg=map(.x=avg_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(ut_pre=pmap(list(data,pre_ins_pay,premium_pre),utility_ins,model="isoelastic",par=1,val='mt_per_fisher')) |> 
  mutate(ut_noins=map(data,utility,model="isoelastic",par=1,val='mt_per_fisher')) |> 
  mutate(ut_post=pmap(list(data,post_ins_pay,premium_post),utility_ins,model="isoelastic",par=1,val='mt_per_fisher')) |> 
  mutate(ut_avg=pmap(list(data,avg_ins_pay,premium_avg),utility_ins,model="isoelastic",par=1,val='mt_per_fisher')) |> 
  mutate(e_ut_pre=map_dbl(ut_pre,~mean(.x))) |> 
  mutate(e_ut_noins=map_dbl(ut_noins,~mean(.x))) |>
  mutate(e_ut_post=map_dbl(ut_post,~mean(.x))) |>
  mutate(e_ut_avg=map_dbl(ut_avg,~mean(.x))) |> 
  mutate(rr_pre=map2_dbl(ut_pre,ut_noins,~(mean(.x)-mean(.y))/mean(.y))) |> 
  mutate(rr_post=map2_dbl(ut_post,ut_noins,~(mean(.x)-mean(.y))/mean(.y))) |>
  mutate(rr_avg=map2_dbl(ut_avg,ut_noins,~(mean(.x)-mean(.y))/mean(.y)))

cali_mt_enso_lm<-cali_cw |> 
  mutate(enso_mt_lm=map(.x=cw_data,~lm_mod_fcn(x="enso",y="landings_mt",data=.x))) |> 
  mutate(enso_mt_pred=map2(.x=cw_data,.y=enso_mt_lm,~pred_fcn(input="enso",output="landings_mt",.x,.y))) |> 
  mutate(pre_ins_pay=map2(.x=data,.y=enso_mt_pred,~pre_ins_pay(.x,.y,roll_val = 'roll_landings'))) |> 
  mutate(post_ins_pay=map2(.x=data,.y=enso_mt_pred,~post_ins_pay(.x,.y,roll_val='roll_landings'))) |> 
  mutate(avg_ins_pay=map2(.x=data,.y=enso_mt_pred,~avg_ins_pay(.x,.y,fish_var='landings_mt'))) |>
  mutate(premium_pre=map(.x=pre_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(premium_post=map(.x=post_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(premium_avg=map(.x=avg_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(ut_pre=pmap(list(data,pre_ins_pay,premium_pre),utility_ins,model="isoelastic",par=1,val='landings_mt')) |> 
  mutate(ut_noins=map(data,utility,model="isoelastic",par=1,val='landings_mt')) |> 
  mutate(ut_post=pmap(list(data,post_ins_pay,premium_post),utility_ins,model="isoelastic",par=1,val='landings_mt')) |> 
  mutate(ut_avg=pmap(list(data,avg_ins_pay,premium_avg),utility_ins,model="isoelastic",par=1,val='landings_mt')) |> 
  mutate(e_ut_pre=map_dbl(ut_pre,~mean(.x))) |> 
  mutate(e_ut_noins=map_dbl(ut_noins,~mean(.x))) |>
  mutate(e_ut_post=map_dbl(ut_post,~mean(.x))) |>
  mutate(e_ut_avg=map_dbl(ut_avg,~mean(.x))) |> 
  mutate(rr_pre=map2_dbl(ut_pre,ut_noins,~(mean(.x)-mean(.y))/mean(.y))) |> 
  mutate(rr_post=map2_dbl(ut_post,ut_noins,~(mean(.x)-mean(.y))/mean(.y))) |>
  mutate(rr_avg=map2_dbl(ut_avg,ut_noins,~(mean(.x)-mean(.y))/mean(.y)))

### Repeat with pdo index

cali_mt_n_pdo_lm<-cali_cw |> 
  mutate(pdo_mt_lm=map(.x=cw_data,~lm_mod_fcn(x="pdo",y="mt_per_fisher",data=.x))) |> 
  mutate(pdo_mt_pred=map2(.x=cw_data,.y=pdo_mt_lm,~pred_fcn(input="pdo",output="mt_per_fisher",.x,.y))) |> 
  mutate(pre_ins_pay=map2(.x=data,.y=pdo_mt_pred,~pre_ins_pay(.x,.y,roll_val = 'roll_n_mt'))) |> 
  mutate(post_ins_pay=map2(.x=data,.y=pdo_mt_pred,~post_ins_pay(.x,.y,roll_val='roll_n_mt'))) |> 
  mutate(avg_ins_pay=map2(.x=data,.y=pdo_mt_pred,~avg_ins_pay(.x,.y,fish_var='mt_per_fisher'))) |>
  mutate(premium_pre=map(.x=pre_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(premium_post=map(.x=post_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(premium_avg=map(.x=avg_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(ut_pre=pmap(list(data,pre_ins_pay,premium_pre),utility_ins,model="isoelastic",par=1,val='mt_per_fisher')) |> 
  mutate(ut_noins=map(data,utility,model="isoelastic",par=1,val='mt_per_fisher')) |> 
  mutate(ut_post=pmap(list(data,post_ins_pay,premium_post),utility_ins,model="isoelastic",par=1,val='mt_per_fisher')) |> 
  mutate(ut_avg=pmap(list(data,avg_ins_pay,premium_avg),utility_ins,model="isoelastic",par=1,val='mt_per_fisher')) |> 
  mutate(e_ut_pre=map_dbl(ut_pre,~mean(.x))) |> 
  mutate(e_ut_noins=map_dbl(ut_noins,~mean(.x))) |>
  mutate(e_ut_post=map_dbl(ut_post,~mean(.x))) |>
  mutate(e_ut_avg=map_dbl(ut_avg,~mean(.x))) |> 
  mutate(rr_pre=map2_dbl(ut_pre,ut_noins,~(mean(.x)-mean(.y))/mean(.y))) |> 
  mutate(rr_post=map2_dbl(ut_post,ut_noins,~(mean(.x)-mean(.y))/mean(.y))) |>
  mutate(rr_avg=map2_dbl(ut_avg,ut_noins,~(mean(.x)-mean(.y))/mean(.y)))

cali_mt_pdo_lm<-cali_cw |> 
  mutate(pdo_mt_lm=map(.x=cw_data,~lm_mod_fcn(x="pdo",y="landings_mt",data=.x))) |> 
  mutate(pdo_mt_pred=map2(.x=cw_data,.y=pdo_mt_lm,~pred_fcn(input="pdo",output="landings_mt",.x,.y))) |> 
  mutate(pre_ins_pay=map2(.x=data,.y=pdo_mt_pred,~pre_ins_pay(.x,.y,roll_val = 'roll_landings'))) |> 
  mutate(post_ins_pay=map2(.x=data,.y=pdo_mt_pred,~post_ins_pay(.x,.y,roll_val='roll_landings'))) |> 
  mutate(avg_ins_pay=map2(.x=data,.y=pdo_mt_pred,~avg_ins_pay(.x,.y,fish_var='landings_mt'))) |>
  mutate(premium_pre=map(.x=pre_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(premium_post=map(.x=post_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(premium_avg=map(.x=avg_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(ut_pre=pmap(list(data,pre_ins_pay,premium_pre),utility_ins,model="isoelastic",par=1,val='landings_mt')) |> 
  mutate(ut_noins=map(data,utility,model="isoelastic",par=1,val='landings_mt')) |> 
  mutate(ut_post=pmap(list(data,post_ins_pay,premium_post),utility_ins,model="isoelastic",par=1,val='landings_mt')) |> 
  mutate(ut_avg=pmap(list(data,avg_ins_pay,premium_avg),utility_ins,model="isoelastic",par=1,val='landings_mt')) |> 
  mutate(e_ut_pre=map_dbl(ut_pre,~mean(.x))) |> 
  mutate(e_ut_noins=map_dbl(ut_noins,~mean(.x))) |>
  mutate(e_ut_post=map_dbl(ut_post,~mean(.x))) |>
  mutate(e_ut_avg=map_dbl(ut_avg,~mean(.x))) |> 
  mutate(rr_pre=map2_dbl(ut_pre,ut_noins,~(mean(.x)-mean(.y))/mean(.y))) |> 
  mutate(rr_post=map2_dbl(ut_post,ut_noins,~(mean(.x)-mean(.y))/mean(.y))) |>
  mutate(rr_avg=map2_dbl(ut_avg,ut_noins,~(mean(.x)-mean(.y))/mean(.y)))
  
### Repeat with beuti index

cali_mt_n_beuti_lm<-cali_cw |> 
  mutate(beuti_mt_lm=map(.x=cw_data,~lm_mod_fcn(x="avg_beuti",y="mt_per_fisher",data=.x))) |> 
  mutate(beuti_mt_pred=map2(.x=cw_data,.y=beuti_mt_lm,~pred_fcn(input="avg_beuti",output="mt_per_fisher",.x,.y))) |> 
  mutate(pre_ins_pay=map2(.x=data,.y=beuti_mt_pred,~pre_ins_pay(.x,.y,roll_val = 'roll_n_mt'))) |> 
  mutate(post_ins_pay=map2(.x=data,.y=beuti_mt_pred,~post_ins_pay(.x,.y,roll_val='roll_n_mt'))) |> 
  mutate(avg_ins_pay=map2(.x=data,.y=beuti_mt_pred,~avg_ins_pay(.x,.y,fish_var='mt_per_fisher'))) |>
  mutate(premium_pre=map(.x=pre_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(premium_post=map(.x=post_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(premium_avg=map(.x=avg_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(ut_pre=pmap(list(data,pre_ins_pay,premium_pre),utility_ins,model="isoelastic",par=1,val='mt_per_fisher')) |> 
  mutate(ut_noins=map(data,utility,model="isoelastic",par=1,val='mt_per_fisher')) |> 
  mutate(ut_post=pmap(list(data,post_ins_pay,premium_post),utility_ins,model="isoelastic",par=1,val='mt_per_fisher')) |> 
  mutate(ut_avg=pmap(list(data,avg_ins_pay,premium_avg),utility_ins,model="isoelastic",par=1,val='mt_per_fisher')) |> 
  mutate(e_ut_pre=map_dbl(ut_pre,~mean(.x))) |> 
  mutate(e_ut_noins=map_dbl(ut_noins,~mean(.x))) |>
  mutate(e_ut_post=map_dbl(ut_post,~mean(.x))) |>
  mutate(e_ut_avg=map_dbl(ut_avg,~mean(.x))) |> 
  mutate(rr_pre=map2_dbl(ut_pre,ut_noins,~(mean(.x)-mean(.y))/mean(.y))) |> 
  mutate(rr_post=map2_dbl(ut_post,ut_noins,~(mean(.x)-mean(.y))/mean(.y))) |>
  mutate(rr_avg=map2_dbl(ut_avg,ut_noins,~(mean(.x)-mean(.y))/mean(.y)))

cali_mt_beuti_lm<-cali_cw |>
  mutate(beuti_mt_lm=map(.x=cw_data,~lm_mod_fcn(x="avg_beuti",y="landings_mt",data=.x))) |> 
  mutate(beuti_mt_pred=map2(.x=cw_data,.y=beuti_mt_lm,~pred_fcn(input="avg_beuti",output="landings_mt",.x,.y))) |> 
  mutate(pre_ins_pay=map2(.x=data,.y=beuti_mt_pred,~pre_ins_pay(.x,.y,roll_val = 'roll_landings'))) |> 
  mutate(post_ins_pay=map2(.x=data,.y=beuti_mt_pred,~post_ins_pay(.x,.y,roll_val='roll_landings'))) |> 
  mutate(avg_ins_pay=map2(.x=data,.y=beuti_mt_pred,~avg_ins_pay(.x,.y,fish_var='landings_mt'))) |>
  mutate(premium_pre=map(.x=pre_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(premium_post=map(.x=post_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(premium_avg=map(.x=avg_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(ut_pre=pmap(list(data,pre_ins_pay,premium_pre),utility_ins,model="isoelastic",par=1,val='landings_mt')) |> 
  mutate(ut_noins=map(data,utility,model="isoelastic",par=1,val='landings_mt')) |> 
  mutate(ut_post=pmap(list(data,post_ins_pay,premium_post),utility_ins,model="isoelastic",par=1,val='landings_mt')) |> 
  mutate(ut_avg=pmap(list(data,avg_ins_pay,premium_avg),utility_ins,model="isoelastic",par=1,val='landings_mt')) |> 
  mutate(e_ut_pre=map_dbl(ut_pre,~mean(.x))) |> 
  mutate(e_ut_noins=map_dbl(ut_noins,~mean(.x))) |>
  mutate(e_ut_post=map_dbl(ut_post,~mean(.x))) |>
  mutate(e_ut_avg=map_dbl(ut_avg,~mean(.x))) |> 
  mutate(rr_pre=map2_dbl(ut_pre,ut_noins,~(mean(.x)-mean(.y))/mean(.y))) |> 
  mutate(rr_post=map2_dbl(ut_post,ut_noins,~(mean(.x)-mean(.y))/mean(.y))) |>
  mutate(rr_avg=map2_dbl(ut_avg,ut_noins,~(mean(.x)-mean(.y))/mean(.y)))

### Repeat with avg_cuti index
cali_mt_n_cuti_lm<-cali_cw |> 
  mutate(cuti_mt_lm=map(.x=cw_data,~lm_mod_fcn(x="avg_cuti",y="mt_per_fisher",data=.x))) |> 
  mutate(cuti_mt_pred=map2(.x=cw_data,.y=cuti_mt_lm,~pred_fcn(input="avg_cuti",output="mt_per_fisher",.x,.y))) |> 
  mutate(pre_ins_pay=map2(.x=data,.y=cuti_mt_pred,~pre_ins_pay(.x,.y,roll_val = 'roll_n_mt'))) |> 
  mutate(post_ins_pay=map2(.x=data,.y=cuti_mt_pred,~post_ins_pay(.x,.y,roll_val='roll_n_mt'))) |> 
  mutate(avg_ins_pay=map2(.x=data,.y=cuti_mt_pred,~avg_ins_pay(.x,.y,fish_var='mt_per_fisher'))) |>
  mutate(premium_pre=map(.x=pre_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(premium_post=map(.x=post_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(premium_avg=map(.x=avg_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(ut_pre=pmap(list(data,pre_ins_pay,premium_pre),utility_ins,model="isoelastic",par=1,val='mt_per_fisher')) |> 
  mutate(ut_noins=map(data,utility,model="isoelastic",par=1,val='mt_per_fisher')) |> 
  mutate(ut_post=pmap(list(data,post_ins_pay,premium_post),utility_ins,model="isoelastic",par=1,val='mt_per_fisher')) |> 
  mutate(ut_avg=pmap(list(data,avg_ins_pay,premium_avg),utility_ins,model="isoelastic",par=1,val='mt_per_fisher')) |> 
  mutate(e_ut_pre=map_dbl(ut_pre,~mean(.x))) |> 
  mutate(e_ut_noins=map_dbl(ut_noins,~mean(.x))) |>
  mutate(e_ut_post=map_dbl(ut_post,~mean(.x))) |>
  mutate(e_ut_avg=map_dbl(ut_avg,~mean(.x))) |> 
  mutate(rr_pre=map2_dbl(ut_pre,ut_noins,~(mean(.x)-mean(.y))/mean(.y))) |> 
  mutate(rr_post=map2_dbl(ut_post,ut_noins,~(mean(.x)-mean(.y))/mean(.y))) |>
  mutate(rr_avg=map2_dbl(ut_avg,ut_noins,~(mean(.x)-mean(.y))/mean(.y)))


### Repeat with avg_cuti index
cali_mt_cuti_lm<-cali_cw |> 
  mutate(ceuti_mt_lm=map(.x=cw_data,~lm_mod_fcn(x="avg_cuti",y="landings_mt",data=.x))) |> 
  mutate(ceuti_mt_pred=map2(.x=cw_data,.y=ceuti_mt_lm,~pred_fcn(input="avg_cuti",output="landings_mt",.x,.y))) |> 
  mutate(pre_ins_pay=map2(.x=data,.y=ceuti_mt_pred,~pre_ins_pay(.x,.y,roll_val = 'roll_landings'))) |> 
  mutate(post_ins_pay=map2(.x=data,.y=ceuti_mt_pred,~post_ins_pay(.x,.y,roll_val='roll_landings'))) |> 
  mutate(avg_ins_pay=map2(.x=data,.y=ceuti_mt_pred,~avg_ins_pay(.x,.y,fish_var='landings_mt'))) |>
  mutate(premium_pre=map(.x=pre_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(premium_post=map(.x=post_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(premium_avg=map(.x=avg_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(ut_pre=pmap(list(data,pre_ins_pay,premium_pre),utility_ins,model="isoelastic",par=1,val='landings_mt')) |> 
  mutate(ut_noins=map(data,utility,model="isoelastic",par=1,val='landings_mt')) |> 
  mutate(ut_post=pmap(list(data,post_ins_pay,premium_post),utility_ins,model="isoelastic",par=1,val='landings_mt')) |> 
  mutate(ut_avg=pmap(list(data,avg_ins_pay,premium_avg),utility_ins,model="isoelastic",par=1,val='landings_mt')) |> 
  mutate(e_ut_pre=map_dbl(ut_pre,~mean(.x))) |> 
  mutate(e_ut_noins=map_dbl(ut_noins,~mean(.x))) |>
  mutate(e_ut_post=map_dbl(ut_post,~mean(.x))) |>
  mutate(e_ut_avg=map_dbl(ut_avg,~mean(.x))) |> 
  mutate(rr_pre=map2_dbl(ut_pre,ut_noins,~(mean(.x)-mean(.y))/mean(.y))) |> 
  mutate(rr_post=map2_dbl(ut_post,ut_noins,~(mean(.x)-mean(.y))/mean(.y))) |>
  mutate(rr_avg=map2_dbl(ut_avg,ut_noins,~(mean(.x)-mean(.y))/mean(.y)))

# compare the means of the relative utility values for each model and payout design
rr_comp<-data.frame(
  model=c("SST","HCI","ENSO","PDO","BEUTI","CUTI"),
  pre=c(mean(cali_mt_sst_lm$rr_pre),mean(cali_mt_hci_lm$rr_pre),mean(cali_mt_enso_lm$rr_pre),mean(cali_mt_pdo_lm$rr_pre),mean(cali_mt_beuti_lm$rr_pre),mean(cali_mt_cuti_lm$rr_pre)),
  post=c(mean(cali_mt_sst_lm$rr_post),mean(cali_mt_hci_lm$rr_post),mean(cali_mt_enso_lm$rr_post),mean(cali_mt_pdo_lm$rr_post),mean(cali_mt_beuti_lm$rr_post),mean(cali_mt_cuti_lm$rr_post)),
  avg=c(mean(cali_mt_sst_lm$rr_avg),mean(cali_mt_hci_lm$rr_avg),mean(cali_mt_enso_lm$rr_avg),mean(cali_mt_pdo_lm$rr_avg),mean(cali_mt_beuti_lm$rr_avg),mean(cali_mt_cuti_lm$rr_avg))
)

rr_n_comp<-data.frame(
  model=c("SST","HCI","ENSO","PDO","BEUTI","CUTI"),
  pre=c(mean(cali_mt_n_sst_lm$rr_pre),mean(cali_mt_n_hci_lm$rr_pre),mean(cali_mt_n_enso_lm$rr_pre),mean(cali_mt_n_pdo_lm$rr_pre),mean(cali_mt_n_beuti_lm$rr_pre),mean(cali_mt_n_cuti_lm$rr_pre)),
  post=c(mean(cali_mt_n_sst_lm$rr_post),mean(cali_mt_n_hci_lm$rr_post),mean(cali_mt_n_enso_lm$rr_post),mean(cali_mt_n_pdo_lm$rr_post),mean(cali_mt_n_beuti_lm$rr_post),mean(cali_mt_n_cuti_lm$rr_post)),
  avg=c(mean(cali_mt_n_sst_lm$rr_avg),mean(cali_mt_n_hci_lm$rr_avg),mean(cali_mt_n_enso_lm$rr_avg),mean(cali_mt_n_pdo_lm$rr_avg),mean(cali_mt_n_beuti_lm$rr_avg),mean(cali_mt_n_cuti_lm$rr_avg))
)


####### All environmental combinations with revenue instead of landings  ######
cali_rev_n_sst_lm<-cali_cw |> 
  mutate(sst_rev_lm=map(.x=cw_data,~lm_mod_fcn(x="avg_sst",y="rev_per_fisher",data=.x))) |> 
  mutate(sst_rev_pred=map2(.x=cw_data,.y=sst_rev_lm,~pred_fcn(input="avg_sst",output="rev_per_fisher",.x,.y))) |> 
  mutate(pre_ins_pay=map2(.x=data,.y=sst_rev_pred,~pre_ins_pay(.x,.y,roll_val = 'roll_n_rev'))) |> 
  mutate(post_ins_pay=map2(.x=data,.y=sst_rev_pred,~post_ins_pay(.x,.y,roll_val='roll_n_rev'))) |> 
  mutate(avg_ins_pay=map2(.x=data,.y=sst_rev_pred,~avg_ins_pay(.x,.y,fish_var='rev_per_fisher'))) |>
  mutate(premium_pre=map(.x=pre_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(premium_post=map(.x=post_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(premium_avg=map(.x=avg_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(ut_pre=pmap(list(data,pre_ins_pay,premium_pre),utility_ins,model="isoelastic",par=1,val='rev_per_fisher')) |> 
  mutate(ut_noins=map(data,utility,model="isoelastic",par=1,val='rev_per_fisher')) |> 
  mutate(ut_post=pmap(list(data,post_ins_pay,premium_post),utility_ins,model="isoelastic",par=1,val='rev_per_fisher')) |> 
  mutate(ut_avg=pmap(list(data,avg_ins_pay,premium_avg),utility_ins,model="isoelastic",par=1,val='rev_per_fisher')) |> 
  mutate(e_ut_pre=map_dbl(ut_pre,~mean(.x))) |> 
  mutate(e_ut_noins=map_dbl(ut_noins,~mean(.x))) |>
  mutate(e_ut_post=map_dbl(ut_post,~mean(.x))) |>
  mutate(e_ut_avg=map_dbl(ut_avg,~mean(.x))) |> 
  mutate(rr_pre=map2_dbl(ut_pre,ut_noins,~(mean(.x)-mean(.y))/mean(.y))) |> 
  mutate(rr_post=map2_dbl(ut_post,ut_noins,~(mean(.x)-mean(.y))/mean(.y))) |>
  mutate(rr_avg=map2_dbl(ut_avg,ut_noins,~(mean(.x)-mean(.y))/mean(.y)))


cali_rev_sst_lm<-cali_cw |> 
  mutate(sst_rev_lm=map(.x=cw_data,~lm_mod_fcn(x="avg_sst",y="value_usd",data=.x))) |> 
  mutate(sst_rev_pred=map2(.x=cw_data,.y=sst_rev_lm,~pred_fcn(input="avg_sst",output="value_usd",.x,.y))) |> 
  mutate(pre_ins_pay=map2(.x=data,.y=sst_rev_pred,~pre_ins_pay(.x,.y,roll_val = 'roll_value_usd'))) |> 
  mutate(post_ins_pay=map2(.x=data,.y=sst_rev_pred,~post_ins_pay(.x,.y,roll_val='roll_value_usd'))) |> 
  mutate(avg_ins_pay=map2(.x=data,.y=sst_rev_pred,~avg_ins_pay(.x,.y,fish_var='value_usd'))) |>
  mutate(premium_pre=map(.x=pre_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(premium_post=map(.x=post_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(premium_avg=map(.x=avg_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(ut_pre=pmap(list(data,pre_ins_pay,premium_pre),utility_ins,model="isoelastic",par=1,val='value_usd')) |> 
  mutate(ut_noins=map(data,utility,model="isoelastic",par=1,val='value_usd')) |> 
  mutate(ut_post=pmap(list(data,post_ins_pay,premium_post),utility_ins,model="isoelastic",par=1,val='value_usd')) |> 
  mutate(ut_avg=pmap(list(data,avg_ins_pay,premium_avg),utility_ins,model="isoelastic",par=1,val='value_usd')) |> 
  mutate(e_ut_pre=map_dbl(ut_pre,~mean(.x))) |> 
  mutate(e_ut_noins=map_dbl(ut_noins,~mean(.x))) |>
  mutate(e_ut_post=map_dbl(ut_post,~mean(.x))) |>
  mutate(e_ut_avg=map_dbl(ut_avg,~mean(.x))) |> 
  mutate(rr_pre=map2_dbl(ut_pre,ut_noins,~(mean(.x)-mean(.y))/mean(.y))) |> 
  mutate(rr_post=map2_dbl(ut_post,ut_noins,~(mean(.x)-mean(.y))/mean(.y))) |>
  mutate(rr_avg=map2_dbl(ut_avg,ut_noins,~(mean(.x)-mean(.y))/mean(.y)))


### Repeat with HCI index

cali_rev_n_hci_lm<-cali_cw |> 
  mutate(hci_rev_lm=map(.x=cw_data,~lm_mod_fcn(x="chci",y="rev_per_fisher",data=.x))) |> 
  mutate(hci_rev_pred=map2(.x=cw_data,.y=hci_rev_lm,~pred_fcn(input="chci",output="rev_per_fisher",.x,.y))) |> 
  mutate(pre_ins_pay=map2(.x=data,.y=hci_rev_pred,~pre_ins_pay(.x,.y,roll_val = 'roll_n_rev'))) |> 
  mutate(post_ins_pay=map2(.x=data,.y=hci_rev_pred,~post_ins_pay(.x,.y,roll_val='roll_n_rev'))) |> 
  mutate(avg_ins_pay=map2(.x=data,.y=hci_rev_pred,~avg_ins_pay(.x,.y,fish_var='rev_per_fisher'))) |>
  mutate(premium_pre=map(.x=pre_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(premium_post=map(.x=post_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(premium_avg=map(.x=avg_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(ut_pre=pmap(list(data,pre_ins_pay,premium_pre),utility_ins,model="isoelastic",par=1,val='rev_per_fisher')) |> 
  mutate(ut_noins=map(data,utility,model="isoelastic",par=1,val='rev_per_fisher')) |> 
  mutate(ut_post=pmap(list(data,post_ins_pay,premium_post),utility_ins,model="isoelastic",par=1,val='rev_per_fisher')) |> 
  mutate(ut_avg=pmap(list(data,avg_ins_pay,premium_avg),utility_ins,model="isoelastic",par=1,val='rev_per_fisher')) |> 
  mutate(e_ut_pre=map_dbl(ut_pre,~mean(.x))) |> 
  mutate(e_ut_noins=map_dbl(ut_noins,~mean(.x))) |>
  mutate(e_ut_post=map_dbl(ut_post,~mean(.x))) |>
  mutate(e_ut_avg=map_dbl(ut_avg,~mean(.x))) |> 
  mutate(rr_pre=map2_dbl(ut_pre,ut_noins,~(mean(.x)-mean(.y))/mean(.y))) |> 
  mutate(rr_post=map2_dbl(ut_post,ut_noins,~(mean(.x)-mean(.y))/mean(.y))) |>
  mutate(rr_avg=map2_dbl(ut_avg,ut_noins,~(mean(.x)-mean(.y))/mean(.y)))
  

cali_rev_hci_lm<-cali_cw |>
  mutate(hci_rev_lm=map(.x=cw_data,~lm_mod_fcn(x="chci",y="value_usd",data=.x))) |> 
  mutate(hci_rev_pred=map2(.x=cw_data,.y=hci_rev_lm,~pred_fcn(input="chci",output="value_usd",.x,.y))) |> 
  mutate(pre_ins_pay=map2(.x=data,.y=hci_rev_pred,~pre_ins_pay(.x,.y,roll_val = 'roll_value_usd'))) |> 
  mutate(post_ins_pay=map2(.x=data,.y=hci_rev_pred,~post_ins_pay(.x,.y,roll_val='roll_value_usd'))) |> 
  mutate(avg_ins_pay=map2(.x=data,.y=hci_rev_pred,~avg_ins_pay(.x,.y,fish_var='value_usd'))) |>
  mutate(premium_pre=map(.x=pre_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(premium_post=map(.x=post_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(premium_avg=map(.x=avg_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(ut_pre=pmap(list(data,pre_ins_pay,premium_pre),utility_ins,model="isoelastic",par=1,val='value_usd')) |> 
  mutate(ut_noins=map(data,utility,model="isoelastic",par=1,val='value_usd')) |> 
  mutate(ut_post=pmap(list(data,post_ins_pay,premium_post),utility_ins,model="isoelastic",par=1,val='value_usd')) |> 
  mutate(ut_avg=pmap(list(data,avg_ins_pay,premium_avg),utility_ins,model="isoelastic",par=1,val='value_usd')) |> 
  mutate(e_ut_pre=map_dbl(ut_pre,~mean(.x))) |> 
  mutate(e_ut_noins=map_dbl(ut_noins,~mean(.x))) |>
  mutate(e_ut_post=map_dbl(ut_post,~mean(.x))) |>
  mutate(e_ut_avg=map_dbl(ut_avg,~mean(.x))) |> 
  mutate(rr_pre=map2_dbl(ut_pre,ut_noins,~(mean(.x)-mean(.y))/mean(.y))) |> 
  mutate(rr_post=map2_dbl(ut_post,ut_noins,~(mean(.x)-mean(.y))/mean(.y))) |>
  mutate(rr_avg=map2_dbl(ut_avg,ut_noins,~(mean(.x)-mean(.y))/mean(.y)))


### repeat with enso

cali_rev_n_enso_lm<-cali_cw |> 
  mutate(enso_rev_lm=map(.x=cw_data,~lm_mod_fcn(x="enso",y="rev_per_fisher",data=.x))) |> 
  mutate(enso_rev_pred=map2(.x=cw_data,.y=enso_rev_lm,~pred_fcn(input="enso",output="rev_per_fisher",.x,.y))) |> 
  mutate(pre_ins_pay=map2(.x=data,.y=enso_rev_pred,~pre_ins_pay(.x,.y,roll_val = 'roll_n_rev'))) |> 
  mutate(post_ins_pay=map2(.x=data,.y=enso_rev_pred,~post_ins_pay(.x,.y,roll_val='roll_n_rev'))) |> 
  mutate(avg_ins_pay=map2(.x=data,.y=enso_rev_pred,~avg_ins_pay(.x,.y,fish_var='rev_per_fisher'))) |>
  mutate(premium_pre=map(.x=pre_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(premium_post=map(.x=post_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(premium_avg=map(.x=avg_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(ut_pre=pmap(list(data,pre_ins_pay,premium_pre),utility_ins,model="isoelastic",par=1,val='rev_per_fisher')) |> 
  mutate(ut_noins=map(data,utility,model="isoelastic",par=1,val='rev_per_fisher')) |> 
  mutate(ut_post=pmap(list(data,post_ins_pay,premium_post),utility_ins,model="isoelastic",par=1,val='rev_per_fisher')) |> 
  mutate(ut_avg=pmap(list(data,avg_ins_pay,premium_avg),utility_ins,model="isoelastic",par=1,val='rev_per_fisher')) |> 
  mutate(e_ut_pre=map_dbl(ut_pre,~mean(.x))) |> 
  mutate(e_ut_noins=map_dbl(ut_noins,~mean(.x))) |>
  mutate(e_ut_post=map_dbl(ut_post,~mean(.x))) |>
  mutate(e_ut_avg=map_dbl(ut_avg,~mean(.x))) |> 
  mutate(rr_pre=map2_dbl(ut_pre,ut_noins,~(mean(.x)-mean(.y))/mean(.y))) |> 
  mutate(rr_post=map2_dbl(ut_post,ut_noins,~(mean(.x)-mean(.y))/mean(.y))) |>
  mutate(rr_avg=map2_dbl(ut_avg,ut_noins,~(mean(.x)-mean(.y))/mean(.y)))

cali_rev_enso_lm<-cali_cw |> 
  mutate(enso_rev_lm=map(.x=cw_data,~lm_mod_fcn(x="enso",y="value_usd",data=.x))) |> 
  mutate(enso_rev_pred=map2(.x=cw_data,.y=enso_rev_lm,~pred_fcn(input="enso",output="value_usd",.x,.y))) |> 
  mutate(pre_ins_pay=map2(.x=data,.y=enso_rev_pred,~pre_ins_pay(.x,.y,roll_val = 'roll_value_usd'))) |> 
  mutate(post_ins_pay=map2(.x=data,.y=enso_rev_pred,~post_ins_pay(.x,.y,roll_val='roll_value_usd'))) |> 
  mutate(avg_ins_pay=map2(.x=data,.y=enso_rev_pred,~avg_ins_pay(.x,.y,fish_var='value_usd'))) |>
  mutate(premium_pre=map(.x=pre_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(premium_post=map(.x=post_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(premium_avg=map(.x=avg_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(ut_pre=pmap(list(data,pre_ins_pay,premium_pre),utility_ins,model="isoelastic",par=1,val='value_usd')) |> 
  mutate(ut_noins=map(data,utility,model="isoelastic",par=1,val='value_usd')) |> 
  mutate(ut_post=pmap(list(data,post_ins_pay,premium_post),utility_ins,model="isoelastic",par=1,val='value_usd')) |> 
  mutate(ut_avg=pmap(list(data,avg_ins_pay,premium_avg),utility_ins,model="isoelastic",par=1,val='value_usd')) |> 
  mutate(e_ut_pre=map_dbl(ut_pre,~mean(.x))) |> 
  mutate(e_ut_noins=map_dbl(ut_noins,~mean(.x))) |>
  mutate(e_ut_post=map_dbl(ut_post,~mean(.x))) |>
  mutate(e_ut_avg=map_dbl(ut_avg,~mean(.x))) |> 
  mutate(rr_pre=map2_dbl(ut_pre,ut_noins,~(mean(.x)-mean(.y))/mean(.y))) |> 
  mutate(rr_post=map2_dbl(ut_post,ut_noins,~(mean(.x)-mean(.y))/mean(.y))) |>
  mutate(rr_avg=map2_dbl(ut_avg,ut_noins,~(mean(.x)-mean(.y))/mean(.y)))
  
### repeat with pdo

cali_rev_n_pdo_lm<-cali_cw |> 
  mutate(pdo_rev_lm=map(.x=cw_data,~lm_mod_fcn(x="pdo",y="rev_per_fisher",data=.x))) |> 
  mutate(pdo_rev_pred=map2(.x=cw_data,.y=pdo_rev_lm,~pred_fcn(input="pdo",output="rev_per_fisher",.x,.y))) |> 
  mutate(pre_ins_pay=map2(.x=data,.y=pdo_rev_pred,~pre_ins_pay(.x,.y,roll_val = 'roll_n_rev'))) |> 
  mutate(post_ins_pay=map2(.x=data,.y=pdo_rev_pred,~post_ins_pay(.x,.y,roll_val='roll_n_rev'))) |> 
  mutate(avg_ins_pay=map2(.x=data,.y=pdo_rev_pred,~avg_ins_pay(.x,.y,fish_var='rev_per_fisher'))) |>
  mutate(premium_pre=map(.x=pre_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(premium_post=map(.x=post_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(premium_avg=map(.x=avg_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(ut_pre=pmap(list(data,pre_ins_pay,premium_pre),utility_ins,model="isoelastic",par=1,val='rev_per_fisher')) |> 
  mutate(ut_noins=map(data,utility,model="isoelastic",par=1,val='rev_per_fisher')) |> 
  mutate(ut_post=pmap(list(data,post_ins_pay,premium_post),utility_ins,model="isoelastic",par=1,val='rev_per_fisher')) |> 
  mutate(ut_avg=pmap(list(data,avg_ins_pay,premium_avg),utility_ins,model="isoelastic",par=1,val='rev_per_fisher')) |> 
  mutate(e_ut_pre=map_dbl(ut_pre,~mean(.x))) |> 
  mutate(e_ut_noins=map_dbl(ut_noins,~mean(.x))) |>
  mutate(e_ut_post=map_dbl(ut_post,~mean(.x))) |>
  mutate(e_ut_avg=map_dbl(ut_avg,~mean(.x))) |> 
  mutate(rr_pre=map2_dbl(ut_pre,ut_noins,~(mean(.x)-mean(.y))/mean(.y))) |> 
  mutate(rr_post=map2_dbl(ut_post,ut_noins,~(mean(.x)-mean(.y))/mean(.y))) |>
  mutate(rr_avg=map2_dbl(ut_avg,ut_noins,~(mean(.x)-mean(.y))/mean(.y)))

cali_rev_pdo_lm<-cali_cw |>
  mutate(pdo_rev_lm=map(.x=cw_data,~lm_mod_fcn(x="pdo",y="value_usd",data=.x))) |> 
  mutate(pdo_rev_pred=map2(.x=cw_data,.y=pdo_rev_lm,~pred_fcn(input="pdo",output="value_usd",.x,.y))) |> 
  mutate(pre_ins_pay=map2(.x=data,.y=pdo_rev_pred,~pre_ins_pay(.x,.y,roll_val = 'roll_value_usd'))) |> 
  mutate(post_ins_pay=map2(.x=data,.y=pdo_rev_pred,~post_ins_pay(.x,.y,roll_val='roll_value_usd'))) |> 
  mutate(avg_ins_pay=map2(.x=data,.y=pdo_rev_pred,~avg_ins_pay(.x,.y,fish_var='value_usd'))) |>
  mutate(premium_pre=map(.x=pre_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(premium_post=map(.x=post_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(premium_avg=map(.x=avg_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(ut_pre=pmap(list(data,pre_ins_pay,premium_pre),utility_ins,model="isoelastic",par=1,val='value_usd')) |> 
  mutate(ut_noins=map(data,utility,model="isoelastic",par=1,val='value_usd')) |> 
  mutate(ut_post=pmap(list(data,post_ins_pay,premium_post),utility_ins,model="isoelastic",par=1,val='value_usd')) |> 
  mutate(ut_avg=pmap(list(data,avg_ins_pay,premium_avg),utility_ins,model="isoelastic",par=1,val='value_usd')) |> 
  mutate(e_ut_pre=map_dbl(ut_pre,~mean(.x))) |> 
  mutate(e_ut_noins=map_dbl(ut_noins,~mean(.x))) |>
  mutate(e_ut_post=map_dbl(ut_post,~mean(.x))) |>
  mutate(e_ut_avg=map_dbl(ut_avg,~mean(.x))) |> 
  mutate(rr_pre=map2_dbl(ut_pre,ut_noins,~(mean(.x)-mean(.y))/mean(.y))) |> 
  mutate(rr_post=map2_dbl(ut_post,ut_noins,~(mean(.x)-mean(.y))/mean(.y))) |>
  mutate(rr_avg=map2_dbl(ut_avg,ut_noins,~(mean(.x)-mean(.y))/mean(.y)))

## Repeat with BEUTI index

cali_rev_n_beuti_lm<-cali_cw |> 
  mutate(beuti_rev_lm=map(.x=cw_data,~lm_mod_fcn(x="avg_beuti",y="rev_per_fisher",data=.x))) |> 
  mutate(beuti_rev_pred=map2(.x=cw_data,.y=beuti_rev_lm,~pred_fcn(input="avg_beuti",output="rev_per_fisher",.x,.y))) |> 
  mutate(pre_ins_pay=map2(.x=data,.y=beuti_rev_pred,~pre_ins_pay(.x,.y,roll_val = 'roll_n_rev'))) |> 
  mutate(post_ins_pay=map2(.x=data,.y=beuti_rev_pred,~post_ins_pay(.x,.y,roll_val='roll_n_rev'))) |> 
  mutate(avg_ins_pay=map2(.x=data,.y=beuti_rev_pred,~avg_ins_pay(.x,.y,fish_var='rev_per_fisher'))) |>
  mutate(premium_pre=map(.x=pre_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(premium_post=map(.x=post_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(premium_avg=map(.x=avg_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(ut_pre=pmap(list(data,pre_ins_pay,premium_pre),utility_ins,model="isoelastic",par=1,val='rev_per_fisher')) |> 
  mutate(ut_noins=map(data,utility,model="isoelastic",par=1,val='rev_per_fisher')) |> 
  mutate(ut_post=pmap(list(data,post_ins_pay,premium_post),utility_ins,model="isoelastic",par=1,val='rev_per_fisher')) |> 
  mutate(ut_avg=pmap(list(data,avg_ins_pay,premium_avg),utility_ins,model="isoelastic",par=1,val='rev_per_fisher')) |> 
  mutate(e_ut_pre=map_dbl(ut_pre,~mean(.x))) |> 
  mutate(e_ut_noins=map_dbl(ut_noins,~mean(.x))) |>
  mutate(e_ut_post=map_dbl(ut_post,~mean(.x))) |>
  mutate(e_ut_avg=map_dbl(ut_avg,~mean(.x))) |> 
  mutate(rr_pre=map2_dbl(ut_pre,ut_noins,~(mean(.x)-mean(.y))/mean(.y))) |> 
  mutate(rr_post=map2_dbl(ut_post,ut_noins,~(mean(.x)-mean(.y))/mean(.y))) |>
  mutate(rr_avg=map2_dbl(ut_avg,ut_noins,~(mean(.x)-mean(.y))/mean(.y)))

cali_rev_beuti_lm<-cali_cw |>
  mutate(beuti_rev_lm=map(.x=cw_data,~lm_mod_fcn(x="avg_beuti",y="value_usd",data=.x))) |> 
  mutate(beuti_rev_pred=map2(.x=cw_data,.y=beuti_rev_lm,~pred_fcn(input="avg_beuti",output="value_usd",.x,.y))) |> 
  mutate(pre_ins_pay=map2(.x=data,.y=beuti_rev_pred,~pre_ins_pay(.x,.y,roll_val = 'roll_value_usd'))) |> 
  mutate(post_ins_pay=map2(.x=data,.y=beuti_rev_pred,~post_ins_pay(.x,.y,roll_val='roll_value_usd'))) |> 
  mutate(avg_ins_pay=map2(.x=data,.y=beuti_rev_pred,~avg_ins_pay(.x,.y,fish_var='value_usd'))) |>
  mutate(premium_pre=map(.x=pre_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(premium_post=map(.x=post_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(premium_avg=map(.x=avg_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(ut_pre=pmap(list(data,pre_ins_pay,premium_pre),utility_ins,model="isoelastic",par=1,val='value_usd')) |> 
  mutate(ut_noins=map(data,utility,model="isoelastic",par=1,val='value_usd')) |> 
  mutate(ut_post=pmap(list(data,post_ins_pay,premium_post),utility_ins,model="isoelastic",par=1,val='value_usd')) |> 
  mutate(ut_avg=pmap(list(data,avg_ins_pay,premium_avg),utility_ins,model="isoelastic",par=1,val='value_usd')) |> 
  mutate(e_ut_pre=map_dbl(ut_pre,~mean(.x))) |> 
  mutate(e_ut_noins=map_dbl(ut_noins,~mean(.x))) |>
  mutate(e_ut_post=map_dbl(ut_post,~mean(.x))) |>
  mutate(e_ut_avg=map_dbl(ut_avg,~mean(.x))) |> 
  mutate(rr_pre=map2_dbl(ut_pre,ut_noins,~(mean(.x)-mean(.y))/mean(.y))) |> 
  mutate(rr_post=map2_dbl(ut_post,ut_noins,~(mean(.x)-mean(.y))/mean(.y))) |>
  mutate(rr_avg=map2_dbl(ut_avg,ut_noins,~(mean(.x)-mean(.y))/mean(.y)))

### repeat with cuti index

cali_rev_n_cuti_lm <- cali_cw |> 
  mutate(cuti_rev_lm=map(.x=cw_data,~lm_mod_fcn(x="avg_cuti",y="rev_per_fisher",data=.x))) |> 
  mutate(cuti_rev_pred=map2(.x=cw_data,.y=cuti_rev_lm,~pred_fcn(input="avg_cuti",output="rev_per_fisher",.x,.y))) |> 
  mutate(pre_ins_pay=map2(.x=data,.y=cuti_rev_pred,~pre_ins_pay(.x,.y,roll_val = 'roll_n_rev'))) |> 
  mutate(post_ins_pay=map2(.x=data,.y=cuti_rev_pred,~post_ins_pay(.x,.y,roll_val='roll_n_rev'))) |> 
  mutate(avg_ins_pay=map2(.x=data,.y=cuti_rev_pred,~avg_ins_pay(.x,.y,fish_var='rev_per_fisher'))) |>
  mutate(premium_pre=map(.x=pre_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(premium_post=map(.x=post_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(premium_avg=map(.x=avg_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(ut_pre=pmap(list(data,pre_ins_pay,premium_pre),utility_ins,model="isoelastic",par=1,val='rev_per_fisher')) |> 
  mutate(ut_noins=map(data,utility,model="isoelastic",par=1,val='rev_per_fisher')) |> 
  mutate(ut_post=pmap(list(data,post_ins_pay,premium_post),utility_ins,model="isoelastic",par=1,val='rev_per_fisher')) |> 
  mutate(ut_avg=pmap(list(data,avg_ins_pay,premium_avg),utility_ins,model="isoelastic",par=1,val='rev_per_fisher')) |> 
  mutate(e_ut_pre=map_dbl(ut_pre,~mean(.x))) |> 
  mutate(e_ut_noins=map_dbl(ut_noins,~mean(.x))) |>
  mutate(e_ut_post=map_dbl(ut_post,~mean(.x))) |>
  mutate(e_ut_avg=map_dbl(ut_avg,~mean(.x))) |> 
  mutate(rr_pre=map2_dbl(ut_pre,ut_noins,~(mean(.x)-mean(.y))/mean(.y))) |> 
  mutate(rr_post=map2_dbl(ut_post,ut_noins,~(mean(.x)-mean(.y))/mean(.y))) |>
  mutate(rr_avg=map2_dbl(ut_avg,ut_noins,~(mean(.x)-mean(.y))/mean(.y)))

cali_rev_cuti_lm<-cali_cw |>
  mutate(cuti_rev_lm=map(.x=cw_data,~lm_mod_fcn(x="avg_cuti",y="value_usd",data=.x))) |> 
  mutate(cuti_rev_pred=map2(.x=cw_data,.y=cuti_rev_lm,~pred_fcn(input="avg_cuti",output="value_usd",.x,.y))) |> 
  mutate(pre_ins_pay=map2(.x=data,.y=cuti_rev_pred,~pre_ins_pay(.x,.y,roll_val = 'roll_value_usd'))) |> 
  mutate(post_ins_pay=map2(.x=data,.y=cuti_rev_pred,~post_ins_pay(.x,.y,roll_val='roll_value_usd'))) |> 
  mutate(avg_ins_pay=map2(.x=data,.y=cuti_rev_pred,~avg_ins_pay(.x,.y,fish_var='value_usd'))) |>
  mutate(premium_pre=map(.x=pre_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(premium_post=map(.x=post_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(premium_avg=map(.x=avg_ins_pay,.f=~sum(.x)/length(.x))) |> 
  mutate(ut_pre=pmap(list(data,pre_ins_pay,premium_pre),utility_ins,model="isoelastic",par=1,val='value_usd')) |> 
  mutate(ut_noins=map(data,utility,model="isoelastic",par=1,val='value_usd')) |> 
  mutate(ut_post=pmap(list(data,post_ins_pay,premium_post),utility_ins,model="isoelastic",par=1,val='value_usd')) |> 
  mutate(ut_avg=pmap(list(data,avg_ins_pay,premium_avg),utility_ins,model="isoelastic",par=1,val='value_usd')) |> 
  mutate(e_ut_pre=map_dbl(ut_pre,~mean(.x))) |> 
  mutate(e_ut_noins=map_dbl(ut_noins,~mean(.x))) |>
  mutate(e_ut_post=map_dbl(ut_post,~mean(.x))) |>
  mutate(e_ut_avg=map_dbl(ut_avg,~mean(.x))) |> 
  mutate(rr_pre=map2_dbl(ut_pre,ut_noins,~(mean(.x)-mean(.y))/mean(.y))) |> 
  mutate(rr_post=map2_dbl(ut_post,ut_noins,~(mean(.x)-mean(.y))/mean(.y))) |>
  mutate(rr_avg=map2_dbl(ut_avg,ut_noins,~(mean(.x)-mean(.y))/mean(.y)))

## compare rr values across all models and payouts

rr_comp_rev<-data.frame(
  model=c("SST","HCI","ENSO","PDO","BEUTI","CUTI"),
  pre=c(mean(cali_rev_sst_lm$rr_pre),mean(cali_rev_hci_lm$rr_pre),mean(cali_rev_enso_lm$rr_pre),mean(cali_rev_pdo_lm$rr_pre),mean(cali_rev_beuti_lm$rr_pre),mean(cali_rev_cuti_lm$rr_pre)),
  post=c(mean(cali_rev_sst_lm$rr_post),mean(cali_rev_hci_lm$rr_post),mean(cali_rev_enso_lm$rr_post),mean(cali_rev_pdo_lm$rr_post),mean(cali_rev_beuti_lm$rr_post),mean(cali_rev_cuti_lm$rr_post)),
  avg=c(mean(cali_rev_sst_lm$rr_avg),mean(cali_rev_hci_lm$rr_avg),mean(cali_rev_enso_lm$rr_avg),mean(cali_rev_pdo_lm$rr_avg),mean(cali_rev_beuti_lm$rr_avg),mean(cali_rev_cuti_lm$rr_avg))
)

gg_fcn<-function(data,pred,y_ax){
  data<-data[[1]]
  
  pred_df<-data.frame(year=seq(1988,2023),pred=pred[[1]])
  data |> 
    ggplot()+
    geom_line(aes(x=year,y=rev_per_fisher,color="Landings"))+
    geom_line(data=pred_df,aes(x=year,y=pred,color="Predicted"))+
    geom_line(aes(x=year,y=roll_value_usd,color="Rolling Mean"))+
    scale_color_manual(values=c("Landings"="black","Predicted"="red","Rolling Mean"="blue"))+
    theme_minimal()
}

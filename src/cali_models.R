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

out<-cali_cw %>% 
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

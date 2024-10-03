library(tidyverse)
library(readxl)
library(wcfish)
library(sf)


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

cali_cw<-cali_catch %>% 
  group_by(species_code) %>% 
  nest() %>% 
  mutate(cw_data=map2(.x=species_code,.y=data,~cw_join_cali(.x,.y)))

cali_cw %>% 
  mutate(sst_mt_lm=pmap(list(x="avg_sst",y="landings_mt",data=cw_data),~lm_mod_fcn(x,y,data)))

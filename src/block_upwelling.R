### cuti and beuit data transformations

#load datasets

library(tidytext)
library(tidyverse)
library(sf)
library(wcfish)

cuti<-read_csv(here::here("data","environmental","CUTI_monthly.csv")) %>% 
  pivot_longer(cols=!c(year,month),names_to="lat",values_to="cuti") %>% 
  mutate(lat=as.numeric(str_replace_all(lat,"N","")))
beuti<-read_csv(here::here("data","environmental","BEUTI_monthly.csv")) %>% 
  pivot_longer(cols=!c(year,month),names_to="lat",values_to="beuti") %>% 
  mutate(lat=as.numeric(str_replace_all(lat,"N","")))


# MAtch block ids to the lat coordinates

b<-blocks %>% 
  mutate(lat=ceiling(block_lat_dd)) %>% 
  filter(block_type=="Inshore") %>% 
  st_drop_geometry()

block_cuti<-b %>% 
  full_join(cuti,by = c("lat")) %>% 
  select(c(block_id,year,month,cuti)) %>% 
  mutate(time=make_datetime(year,month),
         quarter=quarter(time))

cuti_yr<-block_cuti %>% 
  group_by(year,block_id) %>% 
  summarize(avg_cuti=mean(cuti,na.rm=TRUE),
            amp_cuti=max(cuti)-min(cuti)) %>% 
  pivot_longer(cols = c(avg_cuti,amp_cuti),values_to = "value",names_to="var")

cuti_season<-block_cuti %>% 
  group_by(year,quarter,block_id) %>% 
  summarize(value=mean(cuti,na.rm=TRUE)) %>% 
  mutate(var=case_when(quarter==1~paste0("w_cuti",sep=""),
                         quarter==2~paste0("sp_cuti",sep=""),
                         quarter==3~paste0("s_cuti",sep=""),
                         quarter==4~paste0("f_cuti",sep=""))) %>% 
  ungroup() %>% 
  select(year,block_id,var,value)

block_cuti<-rbind(cuti_yr,cuti_season)

block_beuti<-b %>% 
  full_join(beuti,by = c("lat")) %>% 
  select(c(block_id,year,month,beuti)) %>% 
  mutate(time=make_datetime(year,month),
         quarter=quarter(time))

beuti_yr<-block_beuti %>% 
  group_by(year,block_id) %>% 
  summarize(avg_beuti=mean(beuti,na.rm=TRUE),
            amp_beuti=max(beuti)-min(beuti)) %>% 
  pivot_longer(cols = c(avg_beuti,amp_beuti),values_to = "value",names_to="var")

beuti_season<-block_beuti %>% 
  group_by(year,quarter,block_id) %>% 
  summarize(value=mean(beuti,na.rm=TRUE)) %>% 
  mutate(var=case_when(quarter==1~paste0("w_beuti",sep=""),
                       quarter==2~paste0("sp_beuti",sep=""),
                       quarter==3~paste0("s_beuti",sep=""),
                       quarter==4~paste0("f_beuti",sep=""))) %>% 
  ungroup() %>% 
  select(year,block_id,var,value)

block_beuti<-rbind(beuti_yr,beuti_season)

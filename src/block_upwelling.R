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


cuti_d<-read_csv(here::here("data","environmental","CUTI_daily.csv")) %>% 
  mutate(doy=yday(lubridate::make_date(year=year,month=month,day=day))) %>% 
  pivot_longer(cols=!c(year,doy),names_to="lat",values_to="cuti") %>% 
  mutate(lat=as.numeric(str_replace_all(lat,"N",""))) %>% 
  drop_na(lat)

beuti_d<-read_csv(here::here("data","environmental","BEUTI_daily.csv")) %>% 
  mutate(doy=yday(lubridate::make_date(year=year,month=month,day=day))) %>% 
  pivot_longer(cols=!c(year,doy),names_to="lat",values_to="beuti") %>% 
  mutate(lat=as.numeric(str_replace_all(lat,"N",""))) %>% 
  drop_na(lat)



# need to create a juilian day

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



### BEUTI STI

sti<-beuti_d %>% 
  group_by(year,lat) %>% 
  mutate(int=cumsum(beuti)) %>% 
  summarize(sti=which.min(int))

### relaxation
relax_fcn<-function(data){
 
  day_count<-1
  row_count<-0
  excess_count<-0
  event_count<-0
  while(day_count<364){


    if(excess_count == 1 & data$cuti[day_count]< 0.5 ){
      row_count<-row_count+1
    } else if(row_count>=3){
      event_count<-event_count+1
      row_count<-0
      excess_count<-0
    
    } else{
      row_count<-0
      excess_count<-0
      
    }
  
    if(data$cuti[day_count]>1){
      excess_count<-1
    }

    day_count<-day_count+1
    
    
  }
  
  return(event_count)
}

relax<-cuti_d %>% 
  filter(year>1980 & year <2024) %>% 
  group_by(year,lat) %>% 
  nest() %>% 
  mutate(relax=map_dbl(.x=data,~relax_fcn(.x)))


#### frequency

freq<-cuti_d %>% 
  group_by(year,lat) %>% 
  mutate(mod=ifelse(cuti < 2 & cuti > 0.5,1,0)) %>% 
  summarize(freq=sum(mod)/n())

# Match with blocks
block_sti<-b %>% 
  full_join(sti,by = c("lat"),relationship = 'many-to-many') %>% 
  select(c(block_id,year,sti)) %>% 
  pivot_longer(cols=sti,values_to='value',names_to='var')

block_relax<-b %>% 
  full_join(relax,by = c("lat"),relationship = 'many-to-many') %>% 
  select(c(block_id,year,relax))%>% 
  pivot_longer(cols=relax,values_to='value',names_to='var')

block_freq<-b %>% 
  full_join(freq,by = c("lat"),relationship = 'many-to-many') %>% 
  select(c(block_id,year,freq))%>% 
  pivot_longer(cols=freq,values_to='value',names_to='var')

block_beuti<-rbind(block_beuti,block_sti) %>% 
  drop_na()

block_cuti<-rbind(block_cuti,block_relax,block_freq) %>% 
  drop_na()


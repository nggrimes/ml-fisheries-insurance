library(tidyverse)
library(rerddap)
library(lubridate)

rock<-tabledap('FED_Rockfish_Catch','time>=1980-01-01','time<=2024-12-31')

rock_sq<-rock %>% 
  filter(common_name=='MARKET SQUID') %>% 
  mutate(time=as.Date(time)) %>% 
  mutate(month=month(time),
         year=year(time)) %>% 
  group_by(year) %>% 
  summarize(value=sum(catch))

krill<-rock %>% filter(common_name=='KRILL, TOTAL') %>% 
  mutate(time=as.Date(time)) %>% 
  mutate(month=month(time),
         year=year(time))




cps<-tabledap('FRDCPSTrawlLHHaulCatch','time>=1980-01-01','time<=2024-12-31')

cps_squid<-cps %>% 
  filter(scientific_name=='Doryteuthis opalescens') %>% 
  mutate(time=as.Date(time)) %>% 
  mutate(month=month(time),
         year=year(time)) %>% 
  group_by(year) %>% 
  summarize(value=)

calcofi_invert<-tabledap('erdCalCOFIinvcnt','time>=1980-01-01','time<=2024-12-31')

calcofi_sq<-calcofi_invert %>% 
  filter(common_name=='Market squid') %>% 
  mutate(time=as.Date(time)) %>% 
  mutate(month=month(time),
         year=year(time)) 

mt_sq<-calcofi_sq %>% 
  filter(tow_type=='MT') %>% 
  group_by(year) %>% 
  summarize(value=sum(inverts_100m3),
            type='manta_tow')

cb_sq<-calcofi_sq %>% 
  filter(tow_type=='CB') %>% 
  group_by(year) %>% 
  summarize(value=sum(inverts_10m2),
            type='bongo_tow')


calcofi_larvae<-tabledap('erdCalCOFIlrvstg','time>=1980-01-01','time<=2024-12-31')




# CB= 'Bongo Tow' MT='Manta Tow 
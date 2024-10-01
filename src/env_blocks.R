## Merge environmental block data with fisheries catch data

library(readxl)
library(tidyverse)
library(stringr)
library(wcfish)
library(sf)
library(lubridate)

## Get environmental data

load(here::here("data","environmental","sst_cali_sf.rda"))

one_year<-sst_cali_sf %>% 
  filter(mask==0)

# Make weather summarized for the whole year to reduce data load

merge<-st_join(one_year,blocks)

sst_blocks<-merge %>% 
  drop_na(block_id) %>% 
  select(t,mask,temp,sea_surface_temperature_anomaly,block_id,block_type) %>% 
  st_drop_geometry()

sst_blocks<-sst_blocks %>% 
  mutate(time=ymd(t)) %>% 
  mutate(year=year(t),
         month=month(t))
head(sst_blocks)

sst_blocks<-sst_blocks %>% 
  group_by(year,month,block_id) %>% 
  summarize(sst=mean(temp,na.rm=TRUE),
            sst_anomaly=mean(sea_surface_temperature_anomaly,na.rm=TRUE),
            block_type=unique(block_type)) 

### Need to add 2021-2023 from data download

save(sst_blocks,file=here::here("data","environmental","sst_blocks.rda"))

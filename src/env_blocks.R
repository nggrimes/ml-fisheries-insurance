## Merge environmental block data with fisheries catch data

library(readxl)
library(tidyverse)
library(stringr)
library(wcfish)
library(sf)
## will be passed in as purrr dataframe
load(here::here("data","fisheries","cali_catch.rda"))
temp<-cali_catch %>% 
  filter(species_code=="CBZ1")

###

name<-tolower(temp$species_code[1])
catch_data<-readxl::read_xlsx(here::here("data","blocks","state",paste0(name,".xlsx"))) %>% 
  janitor::clean_names() %>% 
  drop_na(total_pounds) %>% 
  filter(total_pounds>0) %>% 
  filter(block_id!="Total") %>% 
  mutate(block_id=as.numeric(block_id))

## construct sf blocks for intersection
block_data_sf<-catch_data %>% 
  inner_join(blocks,by="block_id") %>% 
  mutate(pct_catch=total_pounds/sum(total_pounds))

## Get environmental data

load(here::here("data","environmental","sst_cali_sf.rda"))

one_year<-sst_cali_sf %>% 
  filter(t=="1986-01-16" & mask==0)

# Make weather summarized for the whole year to reduce data load

merge<-st_join(one_year,blocks)

a<-merge %>% drop_na(block_id)

plot(a)

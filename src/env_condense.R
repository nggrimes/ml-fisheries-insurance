# create sea surface temperature data for all three states
load(here::here("data","environmental","sst_noaa_dhw_1985-2020.Rdata"))
load(here::here("data","sf_borders","cali_sf.Rdata"))
load(here::here("data","sf_borders","or_sf.Rdata"))
load(here::here("data","sf_borders","wa_sf.Rdata"))
library(tidyverse)
library(sf)
library(rerddap)
library(lubridate)
library(tsibble)


file_id<-rerddap::info(datasetid = "esrlIcoads1ge_LonPM180", url = "https://coastwatch.pfeg.noaa.gov/erddap/")

start="1985-01-01"

end="2023-12-31"
wind<-griddap(datasetx=file_id,
              time=c(start,end),
              latitude=c(30,42),
              longitude=c(-115,-130))$data %>% 
  mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>% 
  select(time,longitude,latitude,sst,wspd) %>%
  drop_na()

sst_sf<-st_as_sf(wind,coords=c('longitude','latitude'),crs=4326)

# intersect out the california points
ca_sst_sf<-st_intersection(sst_sf,cali_sf) %>% 
  mutate(lon=st_coordinates(.)[,1],
         lat=st_coordinates(.)[,2]) %>% 
  st_drop_geometry() |> 
  mutate(time=as.Date(time)) |> 
  mutate(time=ymd(time))

sb_sst_sf<-st_intersection(sst_sf,sb_sf) %>% 
  mutate(lon=st_coordinates(.)[,1],
         lat=st_coordinates(.)[,2]) %>% 
  st_drop_geometry() |> 
  mutate(time=as.Date(time)) |> 
  mutate(time=ymd(time))

# intersect out the oregon points
or_sst_sf<-st_intersection(sst_sf,oreg_sf) %>% 
  mutate(lon=st_coordinates(.)[,1],
         lat=st_coordinates(.)[,2]) %>% 
  st_drop_geometry() |> 
  mutate(time=as.Date(time)) |> 
  mutate(time=ymd(time))

# intersect out the washington points
wa_sst_sf<-st_intersection(sst_sf,wash_sf) %>% 
  mutate(lon=st_coordinates(.)[,1],
         lat=st_coordinates(.)[,2]) %>% 
  st_drop_geometry() |> 
  mutate(time=as.Date(time)) |> 
  mutate(time=ymd(time))

### Make tsibbles of environmental data  #####

ca_sst_ts<-ca_sst_sf |> 
  mutate(date=tsibble::yearmonth(time)) |> 
  as_tsibble(key=c(lat,lon),index=date)

# create oregon tsibble
or_sst_ts<-or_sst_sf |> 
  mutate(date=tsibble::yearmonth(time)) |> 
  as_tsibble(key=c(lat,lon),index=date)

# create washington tsibble
wa_sst_ts<-wa_sst_sf |> 
  mutate(date=tsibble::yearmonth(time)) |> 
  as_tsibble(key=c(lat,lon),index=date)


### Quarterly and Year average in California

ca_year_temp<-ca_sst_ts %>% 
index_by(year=~year(.)) |>
  summarise(sst=mean(sst,na.rm=TRUE),
            wspd=mean(wspd,na.rm=TRUE)) |> 
  mutate(sst_t1=lag(sst,1),
         sst_t2=lag(sst,2),
         sst_t3=lag(sst,3),
         sst_t4=lag(sst,4),
         wspd_t1=lag(wspd,1),
         wspd_t2=lag(wspd,2),
         wspd_t3=lag(wspd,3),
         wspd_t4=lag(wspd,4))


ca_q_temp<-ca_sst_ts %>% 
  index_by(quarter=~yearquarter(.)) |>
  summarise(sst=mean(sst,na.rm=TRUE),
            wspd=mean(wspd,na.rm=TRUE)) |> 
  ungroup()

# for each year subtract quarter 3 from quarter 1 in ca_q_temp to make a seasonal amplitdue variable
ca_amp<-ca_q_temp %>%
  mutate(q=quarter(quarter),
         year=year(quarter)) |> 
  filter(q %in% c(1,3)) |>
  mutate(lag_sst=lag(sst),
         lag_wspd=lag(wspd)) |> 
  filter(q==3) |> 
  mutate(amp_sst=sst-lag_sst,
         amp_wspd=wspd-lag_wspd) |> 
  as.data.frame() |> 
  select(year,amp_sst,amp_wspd) |> 
  mutate(ampt_t1=lag(amp_sst,1),
         ampt_t2=lag(amp_sst,2),
         ampt_t3=lag(amp_sst,3),
         ampt_t4=lag(amp_sst,4),
         ampw_t1=lag(amp_wspd,1),
         ampw_t2=lag(amp_wspd,2),
         ampw_t3=lag(amp_wspd,3),
         ampw_t4=lag(amp_wspd,4))

# repeat with oregon
or_year_temp<-or_sst_ts %>% 
  index_by(year=~year(.)) |>
  summarise(sst=mean(sst,na.rm=TRUE),
            wspd=mean(wspd,na.rm=TRUE)) |> 
  mutate(sst_t1=lag(sst,1),
         sst_t2=lag(sst,2),
         sst_t3=lag(sst,3),
         sst_t4=lag(sst,4),
         wspd_t1=lag(wspd,1),
         wspd_t2=lag(wspd,2),
         wspd_t3=lag(wspd,3),
         wspd_t4=lag(wspd,4))


or_q_temp<-or_sst_ts %>%
  index_by(quarter=~yearquarter(.)) |>
  summarise(sst=mean(sst,na.rm=TRUE),
            wspd=mean(wspd,na.rm=TRUE)) |> 
  ungroup()

or_amp<-or_q_temp %>%
  mutate(q=quarter(quarter),
         year=year(quarter)) |> 
  filter(q %in% c(1,3)) |>
  mutate(lag_sst=lag(sst),
         lag_wspd=lag(wspd)) |> 
  filter(q==3) |> 
  mutate(amp_sst=sst-lag_sst,
         amp_wspd=wspd-lag_wspd) |> 
  as.data.frame() |> 
  select(year,amp_sst,amp_wspd) |> 
  mutate(ampt_t1=lag(amp_sst,1),
         ampt_t2=lag(amp_sst,2),
         ampt_t3=lag(amp_sst,3),
         ampt_t4=lag(amp_sst,4),
         ampw_t1=lag(amp_wspd,1),
         ampw_t2=lag(amp_wspd,2),
         ampw_t3=lag(amp_wspd,3),
         ampw_t4=lag(amp_wspd,4))

# repeat with washington

wa_year_temp<-wa_sst_ts %>% 
  index_by(year=~year(.)) |>
  summarise(sst=mean(sst,na.rm=TRUE),
            wspd=mean(wspd,na.rm=TRUE)) |> 
  mutate(sst_t1=lag(sst,1),
         sst_t2=lag(sst,2),
         sst_t3=lag(sst,3),
         sst_t4=lag(sst,4),
         wspd_t1=lag(wspd,1),
         wspd_t2=lag(wspd,2),
         wspd_t3=lag(wspd,3),
         wspd_t4=lag(wspd,4))


wa_q_temp<-wa_sst_ts %>%
  index_by(quarter=~yearquarter(.)) |>
  summarise(sst=mean(sst,na.rm=TRUE),
            wspd=mean(wspd,na.rm=TRUE)) |> 
  ungroup()

wa_amp<-wa_q_temp %>%
  mutate(q=quarter(quarter),
         year=year(quarter)) |> 
  filter(q %in% c(1,3)) |>
  mutate(lag_sst=lag(sst),
         lag_wspd=lag(wspd)) |> 
  filter(q==3) |> 
  mutate(amp_sst=sst-lag_sst,
         amp_wspd=wspd-lag_wspd) |> 
  as.data.frame() |> 
  select(year,amp_sst,amp_wspd) |> 
  mutate(ampt_t1=lag(amp_sst,1),
         ampt_t2=lag(amp_sst,2),
         ampt_t3=lag(amp_sst,3),
         ampt_t4=lag(amp_sst,4),
         ampw_t1=lag(amp_wspd,1),
         ampw_t2=lag(amp_wspd,2),
         ampw_t3=lag(amp_wspd,3),
         ampw_t4=lag(amp_wspd,4))



### Get PDO and NINO amplitude data
load("~/ml-fisheries-insurance/data/environmental/enso_pdo.rda")

enso_ts<-enso |> 
  as_tsibble(key=c(enso),index=date) %>%
  filter_index("1981-01-01"~"2021-12-31") %>%
  index_by(year=~year(.)) %>%
  summarise(enso=mean(enso,na.rm=TRUE))

pdo_ts<-pdo |> 
  as_tsibble(key=c(pdo),index=date) %>%
  filter_index("1981-01-01"~"2021-12-31") %>%
  index_by(year=~year(.)) %>%
  summarise(pdo=mean(pdo,na.rm=TRUE))

  

######## Get Anonmaly Data ############

file_id<-rerddap::info(datasetid = "nceiErsstv5_LonPM180", url = "https://coastwatch.pfeg.noaa.gov/erddap/")

anonmaly<-griddap(datasetx=file_id,
              time=c(start,end),
              latitude=c(30,49.2),
              longitude=c(-115,-130))$data %>% 
  mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>% 
  select(time,longitude,latitude,ssta) %>%
  drop_na()

anon_sf<-st_as_sf(anonmaly,coords=c('longitude','latitude'),crs=4326)
# intersect out the california points
ca_anon_sf<-st_intersection(anon_sf,cali_sf) %>% 
  mutate(lon=st_coordinates(.)[,1],
         lat=st_coordinates(.)[,2]) %>% 
  st_drop_geometry() |> 
  mutate(time=as.Date(time)) |> 
  mutate(time=ymd(time))

ca_anon_ts<-ca_anon_sf |> 
  mutate(date=tsibble::yearmonth(time)) |> 
  as_tsibble(key=c(lat,lon),index=date)


ca_q_anon<-ca_anon_ts %>% 
  index_by(quarter=~yearquarter(time)) |>
  summarise(anon=mean(ssta,na.rm=TRUE)) |> 
  ungroup()

#### Save all variables ####
# no anomaly data 
save(ca_year_temp,ca_amp,or_year_temp,or_amp,wa_year_temp,wa_amp,enso_ts,pdo_ts,file="~/ml-fisheries-insurance/data/environmental/env_ts.rda")

  
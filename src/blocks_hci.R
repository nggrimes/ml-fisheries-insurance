### clean the habitat compression index values

# load all and merge into file

library(lubridate)
library(tsibble)

hci_48n<-read_csv(here::here("data","environmental","hci_48n.csv"),skip=1) %>% 
  rename(time=UTC,hci="fraction below monthly threshold") %>% 
  distinct() %>% 
  mutate(time=lubridate::as_date(time),
         year=year(time),
         hci=as.numeric(hci)) %>%
  filter(year>=1988 & year <2024) %>% 
  group_by(year) %>% 
  summarize(chci=sum(hci),
            region=48)

hci_43n<-read_csv(here::here("data","environmental","hci_43n.csv"),skip=1) %>% 
  rename(time=UTC,hci="fraction below monthly threshold") %>% 
  distinct() %>% 
  mutate(time=lubridate::as_date(time),
         year=year(time),
         hci=as.numeric(hci)) %>%
  filter(year>=1988 & year <2024) %>% 
  group_by(year) %>% 
  summarize(chci=sum(hci),
            region=43.5)

hci_40n<-read_csv(here::here("data","environmental","hci_40n.csv"),skip=1) %>% 
  rename(time=UTC,hci="fraction below monthly threshold") %>% 
  distinct() %>% 
  mutate(time=lubridate::as_date(time),
         year=year(time),
         hci=as.numeric(hci)) %>%
  filter(year>=1988 & year <2024) %>% 
  group_by(year) %>% 
  summarize(chci=sum(hci),
            region=40)

hci_35n<-read_csv(here::here("data","environmental","hci_35n.csv"),skip=1) %>% 
  rename(time=UTC,hci="fraction below monthly threshold") %>% 
  distinct() %>% 
  mutate(time=lubridate::as_date(time),
         year=year(time),
         hci=as.numeric(hci)) %>%
  filter(year>=1988 & year <2024) %>% 
  group_by(year) %>% 
  summarize(chci=sum(hci),
            region=35.5)




hci<-rbind(hci_48n,hci_43n,hci_40n,hci_35n)

c<-blocks %>% 
  st_drop_geometry() %>% 
  mutate(lat=ceiling(block_lat_dd)) %>% 
  mutate(region=if_else(lat<=35,35,
                        if_else(lat<=40,40,
                                if_else(lat<=43.5,43.5,48)))) %>% 
  filter(block_type=="Inshore")

block_hci<-c %>% 
  full_join(hci,by="region") %>% 
  select(year,region,chci,block_id)

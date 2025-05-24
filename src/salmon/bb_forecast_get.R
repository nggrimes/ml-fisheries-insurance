library(tidyverse)
forecast<-read.table(here::here("data","fisheries","preseasonForecast.dat"),header=FALSE,skip=1) |> 
  rename(agency=V1,forecaster=V2,return_yr=V3,dist=V4,system_id=V5,fw_age=V6,o_age=V7,return=V8)

bb_sum<-forecast |> 
  group_by(return_yr) |> 
  summarize(sum_return=sum(return,na.rm=TRUE))


manual<-data.frame(return_yr=c(2020,2021,2022,2023,2024,2025),sum_return=1000*c(48.95,50,75.27,49.1,44.95,54.07))

bb_sum<-rbind(bb_sum,manual)

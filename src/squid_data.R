library(tidyverse)
library(rerddap)
library(lubridate)

rock<-tabledap('FED_Rockfish_Catch','time>=1980-01-01','time<=2024-12-31')

rock_sq<-rock %>% 
  filter(common_name=='MARKET SQUID') %>% 
  filter(station_latitude<34.5) %>% 
  mutate(time=as.Date(time)) %>% 
  mutate(year=year(time)) 

krill<-rock %>% filter(common_name=='KRILL, TOTAL') %>% 
  filter(station_latitude<34.5) %>% 
  mutate(time=as.Date(time)) %>% 
  mutate(year=year(time)) 


### attempt at delta glm

krill$presence<-as.numeric(krill$catch>0)
presence_model<-glm(presence~as.factor(year)+as.factor(station),data=krill,family=binomial)

positive_data<-subset(krill,catch>0)
positive_model<-glm(catch~as.factor(year)+as.factor(station),data=positive_data,family=Gamma(link='log'))

# Predict probability of presence
p_presence <- predict(presence_model, newdata = krill, type = "response")

# Predict expected positive catch
expected_catch <- predict(positive_model, newdata = krill, type = "response")

# Combine into delta-GLM prediction
delta_glm_prediction <- p_presence * expected_catch

krill$predict<-delta_glm_prediction

krill_df<-krill %>% group_by(year) %>% 
  summarize(cpue=mean(log(predict)))  # I could spatially match the stations to fishing blocks, but I think the region estimate as less variance.


### Now for squid glm

rock_sq$presence<-as.numeric(rock_sq$catch>0)
presence_model<-glm(presence~as.factor(year)+as.factor(station),data=rock_sq,family=binomial)

positive_data<-subset(rock_sq,catch>0)
positive_model<-glm(catch~as.factor(year)+as.factor(station),data=positive_data,family=Gamma(link='log'))

# Predict probability of presence
p_presence <- predict(presence_model, newdata = rock_sq, type = "response")

# Predict expected positive catch
expected_catch <- predict(positive_model, newdata = rock_sq, type = "response")

# Combine into delta-GLM prediction
squid_glm_pred <- p_presence * expected_catch

rock_sq$predict<-squid_glm_pred


squid_df<-rock_sq %>% 
  group_by(year) %>% 
  summarize(cpue=mean(log(predict)))

save(squid_df,krill_df,file=here::here('data','fisheries','squid_bio.Rdata'))

###### CPS Not used #######

cps<-tabledap('FRDCPSTrawlLHHaulCatch','time>=1980-01-01','time<=2024-12-31')

# cps_squid<-cps %>% 
#   filter(scientific_name=='Doryteuthis opalescens') %>% 
#   mutate(time=as.Date(time)) %>% 
#   mutate(month=month(time),
#          year=year(time)) %>% 
#   group_by(year) %>% 
#   summarize(value=)


### Calcofi  ######
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

# The correlation between manta and bongo are 93%. Get full data coverage by builidng a model for the early years

t_join<-mt_sq %>% full_join(cb_sq,by='year')

mod<-lm(value.y~value.x,data=t_join %>% drop_na())


out<-predict(mod,t_join)

squid_larvae<-data.frame(value=out[3:15],year=seq(1984,1996)) %>% 
  rbind(cb_sq %>% select(-type)) 

save(squid_larvae,file=here::here('data','fisheries','squid_larvae.Rdata'))

calcofi_larvae<-tabledap('erdCalCOFIlrvstg','time>=1980-01-01','time<=2024-12-31')




# CB= 'Bongo Tow' MT='Manta Tow 
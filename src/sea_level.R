# spring transition
library(tidyverse)
sl_cc<-read_csv(here::here('data','environmental','sea_level_cc.csv'),na = "-32767")

colnames(sl_cc)<-c("year","month","day","sea_level")

day_fcn<-function(data){
  
  day_count<-1
  row_count<-1
  while(row_count<7 & day_count<364){
    
    #browser()
    if(data$anonmaly[day_count]< -100 & !is.na(data$anonmaly[day_count])){
      row_count<-row_count+1
    } else{
      row_count<-1
    }
    day_count<-day_count+1
    

  }
  
  return(day_count)
}

sp_trans<-sl_cc %>% 
  filter(year>1980) %>% 
  group_by(year) %>% 
  mutate(yr_avg=mean(sea_level,na.rm=TRUE)) %>% 
  mutate(anonmaly=sea_level-yr_avg) %>% 
  mutate(doy = row_number()) %>% 
  nest() %>% 
  mutate(sp_day=map_dbl(.x=data,~day_fcn(.x)))


# Assume daily data, sampling frequency = 1 (1/day)
fs <- 1           # sampling frequency
fc <- 1/90        # cutoff frequency in cycles/day
Wn <- fc / (fs/2) # normalize by Nyquist frequency

# 4th-order Butterworth filter
bf <- butter(n = 4, W = Wn, type = "low")

# Apply filter
filtered_ts <- filtfilt(bf, sp_trans$data[[1]]$anonmaly)

filtered_ts <- stats::filter(sp_trans$data[[3]]$anonmaly, rep(1/90, 90), sides = 2)

plot(filtered_ts, type = 'l', main = "90-Day Moving Average")

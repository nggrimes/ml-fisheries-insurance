### Calculate the performance of an insurance contract that triggers when the bristol bay
### preseason forecast drops below a coverage level i.e. 50%, 33%, 25%, and 10% over the historical average

# calculate 20 year moving average for the bristol bay forecast

library(dplyr)

bb_for<-bb_sum |> 
  mutate(moving_avg_20 = zoo::rollmean(sum_return, k = 20, fill = NA, align = "right")) |> 
  mutate(moving_avg_10= zoo::rollmean(sum_return, k = 10, fill = NA, align = "right")) |> 
  # I'll get this to work later
  # mutate(
  #   quintile_20yr = map2_dbl(return_yr, sum_return, ~ {
  #     # Define the 20-year window
  #     window_data <- df %>%
  #       filter(return_yr <= .x & return_yr > (.x - 20)) %>%
  #       pull(sum_return)
  #     
  #     # Compute quintile within that window
  #     ntile(c(window_data), 5)[length(window_data)]  # Get the quantile of the current value (last in vector)
  #   })
  # )
  pivot_longer(cols = c(moving_avg_20, moving_avg_10), names_to = "window", values_to = "moving_avg") |>
  filter(!is.na(moving_avg)) |> 
  mutate(pct=sum_return/moving_avg)


payout_fcn<-function(coverage,scale,forecast, avg){
  #scale is the value offer
  # Need to adjust for the amount of forecast
  
  payout<-ifelse(forecast<coverage*avg,scale*(coverage*avg-forecast),0)
  
  return(payout)
}

pay<-bb_for |> 
  mutate(coverage_90=payout_fcn(0.9,1, sum_return, moving_avg),
         coverage_75=payout_fcn(0.75,1, sum_return, moving_avg),
         coverage_60=payout_fcn(0.6,1, sum_return, moving_avg)) |> 
  pivot_longer(cols = c(coverage_90, coverage_75, coverage_60), names_to = "coverage", values_to = "payout")



utility_fcn<-function(year,win,cov,m,scale,mod='cara',ra=0.08,df){
  
  
f_df<-df |> 
  mutate(moving_avg = zoo::rollmean(sum_return, k = win, fill = NA, align = "right")) |> 
  filter(!is.na(moving_avg)) |>
  mutate(payout=payout_fcn(cov,scale,sum_return, moving_avg))

p_df<-f_df |> 
  filter(return_yr>2001 & return_yr<year)
  

premium<-mean(p_df$payout)*m

payout=f_df$payout[which(f_df$return_yr==year)]
fish=f_df$sum_return[which(f_df$return_yr==year)]
  # utility function
  # cara utility function
  if(mod=='cara'){
    utility<-(1-exp(-ra*(payout-premium+fish)))/ra
  }
  
  # exponential utility function
  if(mod=='log'){
    utility<-log(payout-premium+fish)
  }
  
  # mean-variance utility function
  if(mod=='mv'){
    utility<-mean(p_df$payout)-premium+0.5*var(p_df$payout)
  }
  
return(utility)
}

  yr_vec<-2015:2025
  
  map_dbl(.x=yr_vec,~utility_fcn(.x,win=20,cov=0,m=0,scale=1,mod='log',df=bb_sum))
          
  
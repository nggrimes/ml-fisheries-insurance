## Insurance payout functions

# payouts occur at the beginning of year if model prediction is below the 3 year rolling average
pre_ins_pay<-function(data,pred,cl=1){
  #browser()
  #unlist from purrr
  if(length(which(is.na(data$roll_value_usd)))>0){
    pred<-pred[-which(is.na(data$roll_value_usd))]
  }
  
  
  fill_data<-data |> 
    filter(year>=1988) |> 
    drop_na(roll_value_usd)
  
  # set up payout vector
  payout=rep(0,length(pred))
  
  # evaluate payouts
  for(i in 1: length(pred)){
    if(fill_data$roll_value_usd[i]*cl>pred[i]){
      payout[i]<-fill_data$roll_value_usd[i]-pred[i]
    }else{
      payout[i]<-0
    }
  }
  return(payout)
}
# payouts occur at the end of the year if model prediction is above the actual landings

post_ins_pay<-function(data,pred,cl=1){
  #browser()
  #unlist from purrr
  if(length(which(is.na(data$roll_value_usd)))>0){
    pred<-pred[-which(is.na(data$roll_value_usd))]
  }
  
  
  fill_data<-data |> 
    filter(year>=1988) |> 
    drop_na(roll_value_usd)
  
  # set up payout vector
  payout=rep(0,length(pred))
  
  # evaluate payouts
  for(i in 1: length(pred)){
    if(fill_data$value_usd[i]<pred[i]){
      payout[i]<-pred[i]-fill_data$value_usd[i]
    }else{
      payout[i]<-0
    }
  }
  return(payout)
}

avg_ins_pay<-function(data,pred,cl=1){
 #browser()
  #unlist from purrr
  if(length(which(is.na(data$value_usd)))>0){
    pred<-pred[-which(is.na(data$value_usd))]
  }
  
  
  fill_data<-data |> 
    filter(year>=1988) |> 
    drop_na(value_usd)
  
  # set up payout vector
  payout=rep(0,length(pred))
  
  # evaluate payouts
  for(i in 1: length(pred)){
    if(mean(fill_data$value_usd)*cl>pred[i]){
      payout[i]<-mean(fill_data$value_usd)-pred[i]
    }else{
      payout[i]<-0
    }
  }
  return(payout)
}

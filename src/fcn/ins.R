## Insurance payout functions

# payouts occur at the beginning of year if model prediction is below the 3 year rolling average
pre_ins_pay<-function(data,pred,cl=1,roll_val){
  #browser()
  #unlist from purrr
  if(length(which(is.na(data[[roll_val]])))>0){
    pred<-pred[-which(is.na(data[[roll_val]]))]
  }
  
  
  fill_data<-data |> 
    filter(year>=1988) %>% 
    drop_na(.data[[roll_val]])
  
  # set up payout vector
  payout=rep(0,length(pred))
  
  # evaluate payouts
  for(i in 1: length(pred)){
    if(fill_data[[roll_val]][i]*cl>pred[i]){
      payout[i]<-fill_data[[roll_val]][i]-pred[i]
    }else{
      payout[i]<-0
    }
  }
  return(payout)
}


# payouts occur at the end of the year if model prediction is above the actual landings

post_ins_pay<-function(data,pred,cl=1,roll_val){
  #browser()
  #unlist from purrr
  if(length(which(is.na(data[[roll_val]])))>0){
    pred<-pred[-which(is.na(data[[roll_val]]))]
  }
  
  fill_data<-data |> 
    filter(year>=1988) %>% 
    drop_na(.data[[roll_val]])
  
  # set up payout vector
  payout=rep(0,length(pred))
  
  # evaluate payouts
  for(i in 1: length(pred)){
    if(fill_data[[roll_val]][i]<pred[i]){
      payout[i]<-pred[i]-fill_data[[roll_val]][i]
    }else{
      payout[i]<-0
    }
  }
  return(payout)
}

avg_ins_pay<-function(data,pred,cl=1,fish_var){
 #browser()
  #unlist from purrr
  if(length(which(is.na(data[[fish_var]])))>0){
    pred<-pred[-which(is.na(data[[fish_var]]))]
  }
  
  
  fill_data<-data |> 
    filter(year>=1988) |> 
    drop_na()
  
  # set up payout vector
  payout=rep(0,length(pred))
  
  # evaluate payouts
  for(i in 1: length(pred)){
    if(mean(fill_data[[fish_var]])*cl>pred[i]){
      payout[i]<-mean(fill_data[[fish_var]])-pred[i]
    }else{
      payout[i]<-0
    }
  }
  return(payout)
}

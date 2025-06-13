ut_fcn<-function(value,payout_vec,premium,m=1,ra=0.1,mod='cara'){
  
  
  profit=payout_vec+value-premium*m
  
  profit_scale=profit/max(value) #scale to make sure everything matches?
  
  if(mod=='cara'){
    ut=mean((1-exp(-ra*profit_scale))/ra,na.rm=TRUE)
  } else if (mod=='log'){
    profit[which(profit_scale<0)]=0.00001 # Make negative profits really small
    ut<-mean(log(profit_scale),na.rm=TRUE)
  }
  
  return(data.frame(ut=ut,profit=mean(profit)))
}
ut_fcn<-function(value,payout_vec,premium,ra=0.1,mod='cara'){

  profit=payout_vec+value-premium
  
  profit_scale=profit#/max(value) #scale to make sure everything matches?
  
  if(mod=='cara'){
    ut=mean((1-exp(-ra*profit_scale))/ra,na.rm=TRUE)
  
    } else if (mod=='power' & ra==1){
    profit_scale[which(profit_scale<0)]=0.00001 # Make negative profits really small
    ut<-mean(log(profit_scale),na.rm=TRUE)
  
    } else if (mod=='power' & ra!=1){
    profit_scale[which(profit_scale<0)]=0.00001 # Make negative profits really small
    ut=mean((profit_scale^(1-ra)-1)/(1-ra),na.rm=TRUE)
  
    }  else if (mod=='negexp'){
    
    ut<-mean((-1/ra)*exp(-ra*profit_scale))
  }
  profit_out=profit_scale#*max(value)
  return(data.frame(ut=ut,profit=mean(profit_out)))
}

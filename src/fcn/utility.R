## utility models

utility_ins<-function(data,payouts,premium,model="isoelastic",par=3){
  profit<-(data$value_usd+payouts-premium)/1000000
  
  profit[which(profit<=0)]<-0.00001
  
  if(model=="isoelastic"){
    if(par==1){
      ut<-log(profit)
    }else{
      ut<-(profit)^(1-par)/(1-par)
    }
  }
  
  if(model=="cara"){
    ut<-1-exp(-par*(profit))
  }
  return(ut)
}

utility<-function(data,model="isoelastic",par=3){
  #browser()
  profit<-data$value_usd/1000000
  
  profit[which(profit<=0)]<-0.00001
  
  if(model=="isoelastic"){
    if(par==1){
      ut<-log(profit)
    }else{
      ut<-((profit)^(1-par)-1)/(1-par)
    }
  }
  
  if(model=="cara"){
    ut<-1-exp(-par*(profit))
  }
  return(ut)
}

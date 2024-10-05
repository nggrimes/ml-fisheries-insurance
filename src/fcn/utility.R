## utility models

utility_ins<-function(data,payouts,premium,model="isoelastic",par=3,val){
  profit<-(data[[val]]+payouts-premium)
  
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

utility<-function(data,model="isoelastic",par=3,val){
  #browser()
  profit<-data[[val]]
  
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

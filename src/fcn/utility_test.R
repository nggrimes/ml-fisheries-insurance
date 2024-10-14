utility_test<-function(l,data,a=ra,ut_mod=ut_mod,m=1){
  #This function calculates the expected utility to pass into the optim function
  #so that fishers may choose their level of coverage
  #browser()
  profit=data$fish_value+data$raw_pay*l-mean(data$raw_pay*l)*m
  
  if(ut_mod=="log"){
    
    profit[which(profit<=0)]<-0.00001
    ut<-mean(log(profit),na.rm=TRUE)
    return(-ut)
  } else if(ut_mod=="cara"){
    ut<-mean((1-exp(-a*profit))/a,na.rm=TRUE)
    return(-ut)
  }else{
    ut<-mean((profit-1)^(1-a)/(1-a),na.rm=TRUE)
    return(-ut)
  }
  
  
}

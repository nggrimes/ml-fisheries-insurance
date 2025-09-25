lr_find<-function(m,payout,premium,target){
  
  lr<-sum(payout)/sum(premium*m)
  
  out<-lr-target
  return(out)
  
}

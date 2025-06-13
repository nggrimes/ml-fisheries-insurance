ci_fcn<-function(data,alpha){
  
  if(any(is.na(data))){
  clean=data[-which(is.na(data))]
  }else{
    clean=data
  }
  

  sample.n <- length(clean)
  sample.sd <- sd(clean)
  sample.se <- sample.sd/sqrt(sample.n)
  
  degrees.freedom = sample.n - 1
  t.score = qt(p=alpha/2, df=degrees.freedom,lower.tail=F)
  
  margin.error <- t.score * sample.se
  
  expected<-mean(clean)

  
  hi<- expected + margin.error
  lo<- expected - margin.error
  
  return(data.frame(hi=hi,lo=lo))
}

lasso_fcn<-function(data,dep_var,var_list='all',ra=1,ut_mod='log'){
  
  if(var_list=='all'){
    filter_data<-data %>% 
      filter(fish_var==dep_var) |> 
      pivot_wider(
        names_from=var,
        values_from=value
      ) |> 
      drop_na() |> 
      select(-year,-fish_var)
  }else{
    filter_data<-data %>% 
      filter(var %in% var_list & fish_var==dep_var)
    pivot_wider(
      names_from=var,
      values_from=value
    ) |> 
      drop_na() |> 
      select(-year,-fish_var)
  }
  
  #glmnet lasso doesn't handle df's so we need to convert to matrix
  ind<-filter_data |> 
    select(-fish_value)
  
  dep<-filter_data$fish_value
  
  fit<-glmnet(x=ind,y=dep,alpha=1)
  
  cvfit<-cv.glmnet(x=as.matrix(ind),y=as.matrix(dep),alpha=1)
  
  #predict with original data
  pred=as.data.frame(predict(cvfit,s='lambda.min',newx=as.matrix(ind))) |> 
    rename(pred=1)
  
  fit_data<-cbind(dep,pred)
  
  pay_data<-fit_data |> 
    drop_na() |> 
    mutate(raw_pay=mean(dep)-pred) |> 
    mutate(raw_pay=case_when(raw_pay<0~0,
                             TRUE~raw_pay)) |> 
    mutate(raw_pay=raw_pay/max(dep),
           dep=dep/max(dep)
           )
  
  utility_test<-function(l,data,a=ra,ut_mod=ut_mod){
    #browser()
    profit=data$dep+data$raw_pay*l-mean(data$raw_pay*l)
    
    if(ut_mod=="log"){
      
      profit[which(profit<=0)]<-0.0001
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
  #browser()
  opt_out<-optim(par=.1,utility_test,lower=0,method="L-BFGS-B",data=pay_data,a=0.1,ut_mod=ut_mod)
  u_i=-opt_out$value
  u_noi=-utility_test(0,pay_data,a=ra,ut_mod=ut_mod)
  
  u_rr=(u_i-u_noi)/abs(u_noi)*100
  
  return(list(final_mod=cvfit,coverage=opt_out$par,u_rr=u_rr))
  
}  

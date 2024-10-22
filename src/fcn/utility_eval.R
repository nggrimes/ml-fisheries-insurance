#### model prediction and utility calculating on test data
utility_eval<-function(data,mod,var_name,ut_mod='cara',m=1,ra=0.08){

  dat<-data |> 
    filter(fish_var==var_name) |> 
    drop_na()
  
  
  
  if(class(mod$final_mod)=='workflow'){
    
    mod<-mod$final_mod

    
    yr<-max(dat$year)
    
    l_val<-rep(0,yr-2012)
    u_i_val<-rep(0,yr-2012)
    u_noi_val<-rep(0,yr-2012)
    prem_vec<-rep(0,yr-2012)
    
    for(i in 1:(yr-2012)){
      
      temp<-dat |> 
        filter(year<(2013+i)) |> 
        pivot_wider(
          names_from=var,
          values_from=value
        ) |> 
        drop_na() |> 
        select(-year,-fish_var)
      
      pred<-predict(mod |> fit(temp),temp) |> 
        rename(pred=1)
      
      t_yr<-dat |> 
        filter(year<(2013+i)) |> 
        pivot_wider(
          names_from=var,
          values_from=value
        ) |> 
        drop_na()
      
      pay_data<-t_yr |> 
        select(year,fish_value) |> 
        cbind(pred) |> 
        mutate(raw_pay=mean(fish_value)-pred) |> 
        mutate(raw_pay=case_when(raw_pay<0~0,
                                 TRUE~raw_pay)) |> 
        mutate(scale_pay=raw_pay/max(fish_value),
               fish_value=fish_value/max(fish_value)  # This step 'normalizes' differences in payouts to compare utility across all fisheries
        )
      
      t_opt<-optim(par=.1,utility_test,lower=0,upper=1.5,method="L-BFGS-B",data=pay_data,a=0.08,ut_mod='cara',m=1)
      l_val[i]<-t_opt$par
      
      
      profit<-pay_data$fish_value[nrow(temp)]+pay_data$scale_pay[nrow(temp)]*l_val[i]-mean(pay_data$scale_pay*l_val[i])*m
      
      if(ut_mod=="log"){
        profit[which(profit<=0)]<-0.00001
        u_i_val[i]<-mean(log(profit),na.rm=TRUE)
      } else if(ut_mod=="cara"){
        u_i_val[i]<-mean((1-exp(-ra*profit))/ra,na.rm=TRUE)
      }else{
        u_i_val[i]<-mean((profit-1)^(1-ra)/(1-ra),na.rm=TRUE)
      }
      
      if(ut_mod=="log"){
        
        u_noi_val[i]<-mean(pay_data$fish_value[nrow(pay_data)],na.rm=TRUE)
        
      } else if(ut_mod=="cara"){
        u_noi_val[i]<-mean((1-exp(-ra*pay_data$fish_value[nrow(pay_data)]))/ra,na.rm=TRUE)
      }else{
        ut_noi_val[i]<-mean((pay_data$fish_value[nrow(pay_data)]-1)^(1-ra)/(1-ra),na.rm=TRUE)
        
      }
      
      
      prem_vec[i]<-mean(pay_data$raw_pay*l_val[i])*m
      
    }
    
    
    
    
  } else if(class(mod$final_mod)=='ranger'){
    mod<-mod$final_mod
    
    mtry=mod$mtry
    n_node=mod$min.node.size
    
    yr<-max(dat$year)
    
    l_val<-rep(0,yr-2012)
    u_i_val<-rep(0,yr-2012)
    u_noi_val<-rep(0,yr-2012)
    prem_vec<-rep(0,yr-2012)
    
    for(i in 1:(yr-2012)){
     
      temp<-dat |> 
        filter(year<(2013+i)) |> 
        pivot_wider(
          names_from=var,
          values_from=value
        ) |> 
        drop_na() |> 
        select(-year,-fish_var)
      
      temp_mod<-ranger(fish_value~.,data=temp,mtry=mtry,min.node.size=n_node)
      
      pred<-predict(temp_mod,temp)$predictions |> 
        as.data.frame() |> 
        rename(pred=1)
      
      t_yr<-dat |> 
        filter(year<(2013+i)) |> 
        pivot_wider(
          names_from=var,
          values_from=value
        ) |> 
        drop_na()
      
      pay_data<-t_yr |> 
        select(year,fish_value) |> 
        cbind(pred) |> 
        mutate(raw_pay=mean(fish_value)-pred) |> 
        mutate(raw_pay=case_when(raw_pay<0~0,
                                 TRUE~raw_pay)) |> 
        mutate(scale_pay=raw_pay/max(fish_value),
               fish_value=fish_value/max(fish_value)  # This step 'normalizes' differences in payouts to compare utility across all fisheries
        )
      
      t_opt<-optim(par=.1,utility_test,lower=0,upper=1.5,method="L-BFGS-B",data=pay_data,a=0.08,ut_mod='cara',m=1)
      l_val[i]<-t_opt$par
      
      
      profit<-pay_data$fish_value[nrow(temp)]+pay_data$scale_pay[nrow(temp)]*l_val[i]-mean(pay_data$scale_pay*l_val[i])*m
      
      if(ut_mod=="log"){
        profit[which(profit<=0)]<-0.00001
        u_i_val[i]<-mean(log(profit),na.rm=TRUE)
      } else if(ut_mod=="cara"){
        u_i_val[i]<-mean((1-exp(-ra*profit))/ra,na.rm=TRUE)
      }else{
        u_i_val[i]<-mean((profit-1)^(1-ra)/(1-ra),na.rm=TRUE)
      }
      
      if(ut_mod=="log"){
        
        u_noi_val[i]<-mean(pay_data$fish_value[nrow(pay_data)],na.rm=TRUE)
        
      } else if(ut_mod=="cara"){
        u_noi_val[i]<-mean((1-exp(-ra*pay_data$fish_value[nrow(pay_data)]))/ra,na.rm=TRUE)
      }else{
        ut_noi_val[i]<-mean((pay_data$fish_value[nrow(pay_data)]-1)^(1-ra)/(1-ra),na.rm=TRUE)
        
      }
      
      
      prem_vec[i]<-mean(pay_data$raw_pay*l_val[i])*m
      
    }
    
    
    
  }else{
    filt_var<-mod$best_rmse$var
    
    
    mod<-mod$final_mod
    
    yr<-max(dat$year)
    
    l_val<-rep(0,yr-2012)
    u_i_val<-rep(0,yr-2012)
    u_noi_val<-rep(0,yr-2012)
    prem_vec<-rep(0,yr-2012)
    
    for(i in 1:(yr-2012)){
      #browser()
      temp<-dat |> 
        filter(year<(2013+i)) |>
        filter(var==filt_var) |>
        pivot_wider(
          names_from=var,
          values_from=value
        ) |> 
        drop_na() |> 
        select(-year,-fish_var)
      
      temp_mod<-lm(fish_value~.,data=temp)
      
      pred<-predict(temp_mod,temp) |> 
        as.data.frame() |> 
        rename(pred=1)
      
      t_yr<-dat |> 
        filter(year<(2013+i)) |> 
        pivot_wider(
          names_from=var,
          values_from=value
        ) |> 
        drop_na(!!as.symbol(filt_var))
      
      pay_data<-t_yr |> 
        select(year,fish_value) |> 
        cbind(pred) |> 
        mutate(raw_pay=mean(fish_value)-pred) |> 
        mutate(raw_pay=case_when(raw_pay<0~0,
                                 TRUE~raw_pay)) |> 
        mutate(scale_pay=raw_pay/max(fish_value),
               fish_value=fish_value/max(fish_value)  # This step 'normalizes' differences in payouts to compare utility across all fisheries
        )
      
      t_opt<-optim(par=.1,utility_test,lower=0,upper=1.5,method="L-BFGS-B",data=pay_data,a=0.08,ut_mod='cara',m=1)
      l_val[i]<-t_opt$par
      
      
      profit<-pay_data$fish_value[nrow(temp)]+pay_data$scale_pay[nrow(temp)]*l_val[i]-mean(pay_data$scale_pay*l_val[i])*m
      
      if(ut_mod=="log"){
        profit[which(profit<=0)]<-0.00001
        u_i_val[i]<-mean(log(profit),na.rm=TRUE)
      } else if(ut_mod=="cara"){
        u_i_val[i]<-mean((1-exp(-ra*profit))/ra,na.rm=TRUE)
      }else{
        u_i_val[i]<-mean((profit-1)^(1-ra)/(1-ra),na.rm=TRUE)
      }
      
      if(ut_mod=="log"){
        
        u_noi_val[i]<-mean(pay_data$fish_value[nrow(pay_data)],na.rm=TRUE)
        
      } else if(ut_mod=="cara"){
        u_noi_val[i]<-mean((1-exp(-ra*pay_data$fish_value[nrow(pay_data)]))/ra,na.rm=TRUE)
      }else{
        ut_noi_val[i]<-mean((pay_data$fish_value[nrow(pay_data)]-1)^(1-ra)/(1-ra),na.rm=TRUE)
        
      }
      
      
      prem_vec[i]<-mean(pay_data$raw_pay*l_val[i])*m
      
    }
  }
  
  
  # pay_data<-rbind(train,test) |> 
  #   select(year,fish_value) |> 
  #   cbind(pred) |> 
  #   mutate(raw_pay=mean(fish_value)-pred) |> 
  #   mutate(raw_pay=case_when(raw_pay<0~0,
  #                            TRUE~raw_pay)) |> 
  #   mutate(scale_pay=raw_pay/max(fish_value),
  #          fish_value=fish_value/max(fish_value)  # This step 'normalizes' differences in payouts to compare utility across all fisheries
  #   )
  # 
  # 
  # 
  # l_val<-rep(0,nrow(test_data))
  # u_i_val<-rep(0,nrow(test_data))
  # u_noi_val<-rep(0,nrow(test_data))
  # prem_vec<-rep(0,nrow(test_data))
  # 
  # for(i in 1:nrow(test_data)){
  #   
  #   temp<-pay_data |> 
  #     filter(year<(2013+i))
  #   
  #   t_opt<-optim(par=.1,utility_test,lower=0,upper=1.5,method="L-BFGS-B",data=temp,a=0.08,ut_mod='cara',m=1)
  #   l_val[i]<-t_opt$par
  #   
  #   
  #   profit<-temp$fish_value[nrow(temp)]+temp$scale_pay[nrow(temp)]*l_val[i]-mean(temp$scale_pay*l_val[i])*m
  #   
  #   if(ut_mod=="log"){
  #     profit[which(profit<=0)]<-0.00001
  #     u_i_val[i]<-mean(log(profit),na.rm=TRUE)
  #   } else if(ut_mod=="cara"){
  #     u_i_val[i]<-mean((1-exp(-ra*profit))/ra,na.rm=TRUE)
  #   }else{
  #     u_i_val[i]<-mean((profit-1)^(1-ra)/(1-ra),na.rm=TRUE)
  #   }
  #   
  #   if(ut_mod=="log"){
  #     
  #     u_noi_val[i]<-mean(temp$fish_value[nrow(temp)],na.rm=TRUE)
  #     
  #   } else if(ut_mod=="cara"){
  #     u_noi_val[i]<-mean((1-exp(-ra*temp$fish_value[nrow(temp)]))/ra,na.rm=TRUE)
  #   }else{
  #     ut_noi_val[i]<-mean((temp$fish_value[nrow(temp)]-1)^(1-ra)/(1-ra),na.rm=TRUE)
  #     
  #   }
  #   
  #   
  #   prem_vec[i]<-mean(temp$raw_pay*l_val[i])*m
  #   
  # }
  
  test_u_i=mean(u_i_val)
  test_u_noi=mean(u_noi_val)
  
  test_u_rr<-(test_u_i-test_u_noi)/abs(test_u_noi)
  
  if(test_u_rr<0){
    test_u_rr<-0
    prem_vec<-rep(0,length(prem_vec))
    l_val<-rep(0,length(l_val))
  }
  
  return(list(test_u_rr=test_u_rr,test_u_i=test_u_i,test_u_noi=test_u_noi,prem_vec=prem_vec,l_val=l_val,u_i_val=u_i_val,u_noi_val=u_noi_val))
}

temp_lm_fcn<-function(var_name,data){
  
  
  ## Helper functions for purrr map
  
  lm_purr<-function(d,ut_mod='cara',ra=0.08,m=1){
    
    
    size<-floor(0.75*nrow(d))
    
    train_ind<-sample(seq_len(nrow(d)),size=size)
    
    mod=lm(fish_value~value,data=d[train_ind,])
    
    pred=predict(mod,d[train_ind,])
    train_rmse<-sqrt(mean(pred-d[train_ind,]$fish_value)^2)
    
    pred=predict(mod,d[-train_ind,])
    
    test_rmse<-sqrt(mean(pred-d[-train_ind,]$fish_value)^2)
    
    pred<-predict(mod,d[train_ind,]) |> 
      as.data.frame() |> 
      rename(pred=1)
    
    pay_data<-d[train_ind,] |> 
      select(year,fish_value) |> 
      cbind(pred) |> 
      mutate(raw_pay=mean(fish_value)-pred) |> 
      mutate(raw_pay=case_when(raw_pay<0~0,
                               TRUE~raw_pay)) |> 
      mutate(scale_pay=raw_pay/max(d$fish_value),
             fish_value=fish_value/max(d$fish_value)  # This step 'normalizes' differences in payouts to compare utility across all fisheries
      )
    
    t_opt<-optim(par=.1,utility_test,lower=0,upper=1.5,method="L-BFGS-B",data=pay_data,a=0.08,ut_mod='cara',m=1)
    l_val<-t_opt$par
    
    
    profit<-pay_data$fish_value+pay_data$scale_pay*l_val-mean(pay_data$scale_pay*l_val)*m
    
    if(ut_mod=="log"){
      profit[which(profit<=0)]<-0.00001
      train_u_i<-mean(log(profit),na.rm=TRUE)
    } else if(ut_mod=="cara"){
      train_u_i<-mean((1-exp(-ra*profit))/ra,na.rm=TRUE)
    }else{
      train_u_i<-mean((profit-1)^(1-ra)/(1-ra),na.rm=TRUE)
    }
    
    if(ut_mod=="log"){
      
      train_u_noi<-mean(pay_data$fish_value,na.rm=TRUE)
      
    } else if(ut_mod=="cara"){
      train_u_noi<-mean((1-exp(-ra*pay_data$fish_value))/ra,na.rm=TRUE)
    }else{
      train_ut_noi<-mean((pay_data$fish_value-1)^(1-ra)/(1-ra),na.rm=TRUE)
      
    }
    
    pred<-predict(mod,d[-train_ind,]) |> 
      as.data.frame() |> 
      rename(pred=1)
    
    test_pay_data<-d[-train_ind,] |> 
      select(year,fish_value) |> 
      cbind(pred) |> 
      mutate(raw_pay=mean(fish_value)-pred) |> 
      mutate(raw_pay=case_when(raw_pay<0~0,
                               TRUE~raw_pay)) |> 
      mutate(scale_pay=raw_pay/max(d$fish_value),
             fish_value=fish_value/max(d$fish_value)  # This step 'normalizes' differences in payouts to compare utility across all fisheries
      )
    
    profit<-test_pay_data$fish_value+test_pay_data$scale_pay*l_val-mean(pay_data$scale_pay*l_val)*m
    
    if(ut_mod=="log"){
      profit[which(profit<=0)]<-0.00001
      test_u_i<-mean(log(profit),na.rm=TRUE)
    } else if(ut_mod=="cara"){
      test_u_i<-mean((1-exp(-ra*profit))/ra,na.rm=TRUE)
    }else{
      test_u_i<-mean((profit-1)^(1-ra)/(1-ra),na.rm=TRUE)
    }
    
    if(ut_mod=="log"){
      
      test_u_noi<-mean(test_pay_data$fish_value,na.rm=TRUE)
      
    } else if(ut_mod=="cara"){
      test_u_noi<-mean((1-exp(-ra*test_pay_data$fish_value))/ra,na.rm=TRUE)
    }else{
      test_ut_noi<-mean((test_pay_data$fish_value-1)^(1-ra)/(1-ra),na.rm=TRUE)
      
    }
    
    
    prem_vec<-mean(pay_data$raw_pay*l_val)*m
    
    train_u_rr=(train_u_i-train_u_noi)/abs(train_u_noi)
    test_u_rr=(test_u_i-test_u_noi)/abs(test_u_noi)
    
    return(list(mod=mod,train_rmse,test_rmse,test_u_rr=test_u_rr,train_u_rr=train_u_rr))
  }
  
  
  #make an empty dataframe for storage with 3 column names: var, rmse, fold
  
  rmse_store<-data.frame(var=character(),test_u_rr=numeric(),fold=character())
  
  for(i in 1:100){
    
    
    
    split<-data |> 
      filter(fish_var==var_name) |> 
      drop_na()
    
    md_df<-split |> 
      group_by(var) |> 
      nest() |> 
      mutate(mod=map(data,~lm_purr(d=.x)))
    
    
    clean_rmse<-md_df |> 
      hoist(mod,"test_u_rr")|>
      select(var,test_u_rr) |> 
      mutate(fold=paste0("fold",i))
    
    rmse_store<-rbind(rmse_store,clean_rmse)
    
  }
  
  best_rmse<- rmse_store |> 
    group_by(var) |>
    summarize(mean_rmse=mean(test_u_rr),
              sd_u_rr=sd(test_u_rr)) |> 
    filter(mean_rmse==max(mean_rmse))
  
  # run final model on training period data set, testing down with utility_eval
  
  final_mod<-data |> 
    filter(fish_var==var_name & var==best_rmse$var) |>
    drop_na() |>
    filter(year<2013) |> 
    (\(x){lm(fish_value~value,data=x)})()
  
  ## Run utility analysis
  print("running")
  
  return(list(best_rmse=best_rmse,final_mod=final_mod))
} 

cali_mt_lm<-cali_cw %>% 
  mutate(model=map2(.x=cw_data,.y="mt_detrend",~temp_lm_fcn(var_name=.y,data=.x),.progress = TRUE))

save(cali_mt_lm,file=here::here("data","output","cali_mt_lm_bootstrap"))

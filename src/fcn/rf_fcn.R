rf_fcn<-function(data,var_list='all',dep_var,ra=1,ut_mod='log'){
  
  if(var_list=='all'){
    filter_data<-data %>% 
      filter(fish_var==dep_var) |> 
      pivot_wider(
        names_from=var,
        values_from=value
      ) |> 
      drop_na() |> 
      select(-fish_var)
  }else{
    filter_data<-data %>% 
      filter(var %in% var_list & fish_var==dep_var)
    pivot_wider(
      names_from=var,
      values_from=value
    ) |> 
      drop_na() |> 
      select(-fish_var)
  }
  
 
  rmse<-function(x,y){
  
    
    out=sqrt(mean(x-y)^2) 
    
    return(out)
  }
  
  # create grid of possible hyperparameter combinations
  par_grid<-expand_grid(m=seq(2,16,by=2),
               n=seq(2,8)) |> 
    mutate(rmse=NA)
  
  n_fold=10
  
  
  for(j in 1:nrow(par_grid)){
    rmse_store<-(data.frame(rmse=numeric(),fold=character()))
  
    for(i in 1:n_fold){
      #browser()
      #partition into training and testing
      
      train<-filter_data |> 
        filter(year <=(2009+i)) |> #2009 is hard coded to the break point
        drop_na() |> 
        select(-year)
      
      test<-filter_data |>
        filter(year>(2009+i)) |> 
        drop_na() |> 
        select(-year)
      
      set.seed(123)
      md<-ranger(fish_value~.,data=train,num.trees=1000,mtry=par_grid$m[j],min.node.size=par_grid$n[j])
      
      out<-predict(md,test)
      
      clean_rmse<-rmse(out$predictions,test$fish_value)
      
      clean_rmse<-data.frame(rmse=clean_rmse,fold=paste0("fold",i))
      
      rmse_store<-rbind(rmse_store,clean_rmse)
      
    }
  
  par_grid$rmse[j]<- mean(rmse_store$rmse)
  }  
  # run final model on full data set
  
  
  r_index<-which.min(par_grid$rmse)
  
  final_mod<-filter_data |> 
    select(-year) |> 
    (\(x){ranger(fish_value~.,data=x,num.trees=1000,mtry=par_grid$m[r_index],min.node.size=par_grid$n[r_index])})()
  

  
  pred<-predict(final_mod,filter_data) |> 
    pluck("predictions") |> 
    as.data.frame() |> 
    rename(pred=1)
  
  fit_data=filter_data |> 
    select(fish_value) |> 
    cbind(pred)
  
  pay_data<-fit_data |> 
    drop_na() |> 
    mutate(raw_pay=mean(fish_value)-pred) |> 
    mutate(raw_pay=case_when(raw_pay<0~0,
                             TRUE~raw_pay)) |> 
    mutate(raw_pay=raw_pay/max(fish_value),
           fish_value=fish_value/max(fish_value)
    )
  
  
  
  opt_out<-optim(par=.1,utility_test,lower=0,method="L-BFGS-B",data=pay_data,a=0.1,ut_mod=ut_mod)
  u_i=-opt_out$value
  u_noi=-utility_test(0,pay_data,a=ra,ut_mod=ut_mod)
  
  u_rr=(u_i-u_noi)/abs(u_noi)*100
  
  return(list(final_mod=final_mod,coverage=opt_out$par,u_rr=u_rr))
}

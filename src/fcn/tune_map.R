tune_map<-function(...,big_data,v,fv,coverage,m,start_year=24,ra,ut_mod,pred_mod){
  input<-list(...)
  controls<-unlist(input)
  
  if(pred_mod=='lm'){
    df<-big_data |> 
      filter(var==v) |> 
      filter(fish_var==fv) |> 
      drop_na()
    
  } else {
    df<-big_data |> 
      filter(fish_var==fv) |> 
      filter(!var %in% c('amp_beuti','avg_beuti','f_beuti','s_beuti','w_beuti','amp_cuti','avg_cuti','f_cuti','s_cuti','w_cuti','year')) |> 
      pivot_wider(names_from = var, values_from = value) |> 
      drop_na()
  }
  
  
  #Create indicies of time splits
  
  
  start_max <- start_year
  end_max   <- nrow(df)-1
  
  vec_list <- map(start_max:end_max, ~ 1:.x)
  
  # Run analysis for each type of model
  
  if(pred_mod=='lm'){
    
    time_out<-vec_list |> 
      map_df(.f=~analysis_lm(.x,data=df,coverage=coverage,m=m,ut_mod=ut_mod,ra=ra))
    
  } else if (pred_mod=='rf'){
    
    time_out<-vec_list |> 
      map_df(.f=~analysis_rf(.x,data=df,coverage=coverage,controls=controls,m=m,ut_mod=ut_mod,ra=ra))
    
  } else if (pred_mod=='grrf'){
    
    time_out<-vec_list |> 
      map_df(.f=~analysis_grrf(.x,data=df,coverage=coverage,controls=controls,m=m,ut_mod=ut_mod,ra=ra))
    
  } else if(pred_mod=='svm'){
    
    time_out<-vec_list |> 
      map_df(.f=~analysis_svm(.x,data=df,coverage=coverage,controls=controls,m=m,ut_mod=ut_mod,ra=ra))
  }

  u_i<-ut_fcn(time_out$test_value,time_out$payout_vec_test,time_out$prem,ra=ra,mod=ut_mod)$ut
  u_noi<-ut_fcn(time_out$test_value,0,0,ra=ra,mod=ut_mod)$ut
  
  u_rr<-(u_i-u_noi)/abs(u_noi)
  
  lr=sum(time_out$payout_vec_test)/sum(time_out$prem)

  
  rmse_test<-sqrt(mean((time_out$test_value-time_out$pred)^2))
  
  train_vec=unlist(time_out$train_vec[1])
  
  return(tibble(u_rr=u_rr,
                rsq=mean(time_out$rsq,na.rm=TRUE),
                rmse=mean(time_out$rmse,na.rm=TRUE),
                lr=lr,
                rmse_test=rmse_test,
                pred_mod=pred_mod,
                train_vec=list(train_vec),
                test_vec=list(time_out$pred),
                prem_vec=list(time_out$prem),
                payout_vec=list(time_out$payout_vec_test),
                mods=list(time_out$mods),
                pay_threshold_vec=list(time_out$pay_threshold)
  ))
}

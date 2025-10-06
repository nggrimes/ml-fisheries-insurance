tune_lasso<-function(...,big_data,v,fv,coverage,m,start_year=24,ra,ut_mod,pred_mod){
  input<-list(...)
  controls<-unlist(input)
  
  
  df<-big_data |> 
    filter(fish_var==fv) |> 
    filter(!var %in% c('amp_beuti','avg_beuti','f_beuti','s_beuti','w_beuti','amp_cuti','avg_cuti','f_cuti','s_cuti','w_cuti','year')) |> 
    pivot_wider(names_from = var, values_from = value) |> 
    drop_na()
  
  
  
  #Create indicies of time splits
  
  
  start_max <- start_year
  end_max   <- nrow(df)-1
  
  vec_list <- map(start_max:end_max, ~ 1:.x)
  
  
  time_out<-vec_list %>% map_df(.f=~analysis_lasso(.x,
                                                   data=df,
                                                   coverage=coverage,
                                                   controls=controls,
                                                   m=m,
                                                   ut_mod=ut_mod,
                                                   ra=ra,
                                                   pred_mod=pred_mod))
  
  
  
  t_pay<-purrr::transpose(time_out$payout_vec_test)
  t_prem<-purrr::transpose(time_out$prem)
  t_pred<-purrr::transpose(time_out$pred)
  
  #storage df for loops
  rmse_test<-as.data.frame(matrix(NA,ncol=1,nrow=length(controls)))
  u_i_df<-as.data.frame(matrix(NA,ncol=1,nrow=length(controls)))
  
  lr_df<-as.data.frame(matrix(NA,ncol=1,nrow=length(controls)))
  ins_pi_df<-as.data.frame(matrix(NA,ncol=1,nrow=length(controls)))
  
  for( j in 1:nrow(u_i_df)){
    
    u_i_df[j,1]<-ut_fcn(time_out$test_value,unlist(t_pay[[j]]),unlist(t_prem[[j]]),ra=ra,mod=ut_mod)$ut
    
    rmse_test[j,1]<-sqrt(mean((time_out$test_value-unlist(t_pred[[j]]))^2))
    
    lr_df[j,1]<- sum(unlist(t_pay[[j]]))/sum(unlist(t_prem[[j]]))
    ins_pi_df[j,1]<-sum(unlist(t_prem[[j]])-unlist(t_pay[[j]]))
  }
  
  
  u_noi<-ut_fcn(time_out$test_value,0,0,ra=ra,mod=ut_mod)$ut
  
  u_rr<-(u_i_df-u_noi)/abs(u_noi)
  
  # get best model and take premium, payouts, training and testing values

  min_index<-which(rmse_test==min(rmse_test))[1]
 
  
  prem_vec<-unlist(t_prem[[min_index]])
  payout_vec=unlist(t_pay[[min_index]])
  test_vec<-unlist(t_pred[[min_index]])

  
  best_train_rsq<-mean(unlist(purrr::transpose(time_out$rsq)[[min_index]]))
  best_train_rmse<-mean(unlist(purrr::transpose(time_out$rmse)[[min_index]]))
 
  return(tibble(lambda=controls,
                u_rr=u_rr$V1,
                rmse=best_train_rmse,
                rsq=best_train_rsq,
                lr=lr_df$V1,
                rmse_test=rmse_test$V1,
                pred_mod=pred_mod,
                train_vec=list(time_out$train_vec[[1]][,min_index]),
                test_vec=list(test_vec),
                prem_vec=list(prem_vec),
                payout_vec=list(payout_vec),
                mods=list(time_out$mods))
                
  )
}

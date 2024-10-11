# Single linear model purrr function


lm_mod_fcn<-function(var_name,data,ra=1,ut_mod="log"){
  
  
  ## Helper functions for purrr map
  
  purr_predict<-function(x,y){
    
    new_data<-test |> 
      filter(var==y)
    
    return(predict(x,new_data))
    
    
  }
  
  rmse<-function(x,y){
    # filters the variable of interest from the test set and compares the prediction with test data
    raw<-test |> 
      filter(var==y)
    
    out=sqrt(mean(x-raw$fish_value)^2) 
    
    return(out)
  }
  
  
  #make an empty dataframe for storage with 3 column names: var, rmse, fold
  
  rmse_store<-data.frame(var=character(),rmse=numeric(),fold=character())
  
  for(i in 1:10){
    #browser()
    #partition into training and testing

    train<-data |> 
      filter(year <=(2009+i)) |> #2009 is hard coded to the break point
      filter(fish_var==var_name) |> 
      drop_na()
    
    test<-data |>
      filter(year>(2009+i)) |> 
      filter(fish_var==var_name) |> 
      drop_na()
    
    md_df<-train |> 
      group_by(var) |> 
      nest() |> 
      mutate(mod=map(data,~lm(fish_value~value,data=.x))) |> 
      mutate(pred=map2(mod,var,~purr_predict(.x,.y))) |> 
      mutate(rmse=map2_dbl(pred,var,~rmse(.x,.y)))
    
    clean_rmse<-md_df |> 
      select(var,rmse) |> 
      mutate(fold=paste0("fold",i))
    
    rmse_store<-rbind(rmse_store,clean_rmse)
    
  }
  
 best_rmse<- rmse_store |> 
    group_by(var) |>
    summarize(mean_rmse=mean(rmse)) |> 
    filter(mean_rmse==min(mean_rmse))
 
 # run final model on full data set
 
 final_mod<-data |> 
   filter(fish_var==var_name & var==best_rmse$var) |>
   drop_na() |>
   (\(x){lm(fish_value~value,data=x)})()
 
## Run utility analysis
 filt_data<- data |> 
   filter(fish_var==var_name & var==best_rmse$var)
   
  pred<-filt_data |>
    drop_na() |> 
    (\(x){predict(final_mod,x)})() 
  
  pay_data<-filt_data |> 
    drop_na() |> 
    mutate(raw_pay=mean(fish_value)-pred) |> 
    mutate(raw_pay=case_when(raw_pay<0~0,
                               TRUE~raw_pay)) |> 
    mutate(raw_pay=raw_pay/max(fish_value),
           fish_value=fish_value/max(fish_value)  # This step 'normalizes' differences in payouts to compare utility across all fisheries
           )  
  
  
  
  opt_out<-optim(par=.1,utility_test,lower=0,method="L-BFGS-B",data=pay_data,a=0.1,ut_mod=ut_mod)
  u_i=-opt_out$value
  u_noi=-utility_test(0,pay_data,a=ra,ut_mod=ut_mod)
  
  u_rr=(u_i-u_noi)/abs(u_noi)*100
  
  return(list(best_rmse=best_rmse,final_mod=final_mod,coverage=opt_out$par,u_rr=u_rr))
}  


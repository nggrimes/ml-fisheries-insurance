# Single linear model purrr function


lm_mod_fcn<-function(var_name,data){

  
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
 
 # run final model on training period data set, testing down with utility_eval
 
 final_mod<-data |> 
   filter(fish_var==var_name & var==best_rmse$var) |>
   drop_na() |>
   filter(year<2013) |> 
   (\(x){lm(fish_value~value,data=x)})()
 
## Run utility analysis

  
  return(list(best_rmse=best_rmse,final_mod=final_mod))
}  


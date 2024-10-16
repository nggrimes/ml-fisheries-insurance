rf_fcn<-function(data,var_list='all',dep_var){
  
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
  #Run final model on full training data, test down with utility_eval
  final_mod<-filter_data |> 
    filter(year<2013) |> 
    select(-year) |> 
    (\(x){ranger(fish_value~.,data=x,num.trees=1000,mtry=par_grid$m[r_index],min.node.size=par_grid$n[r_index])})()
  

  
  
  
  return(list(final_mod=final_mod))
}

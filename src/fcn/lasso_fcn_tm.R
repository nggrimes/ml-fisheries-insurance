lasso_fcn_tm<-function(data,var_list='all',dep_var){
  #Uses a tidymodels workflow to calculate the best lasso model for a fishery
  # Better performance with the boostraps then cv.glmnet
  # output is a list of the final workflow that needs to be fitted with data before use
  # optimal choice of coverage and the relative improvement in utility
  
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
  
  
  #browser()
  # split data (may have to do manually later)
  set.seed(123)
  data_split<-initial_time_split(filter_data)
  data_train<-training(data_split)
  data_test<-testing(data_split)
  
  # create receipe
  rec<-recipe(fish_value~.,data=data_train) %>%
    step_normalize(all_predictors()) %>%
    step_zv(all_predictors()) 
  
  # prep receipe
  
  rec_prep<-rec |> 
    prep()
  
  # create model
  
  # lasso<-linear_reg(mode="regression",penalty=0.1, mixture=1) %>%
  #   set_engine("glmnet")
  
  # create workflow
  wf<-workflow()%>%
    add_recipe(rec)
  
  data_boot<-bootstraps(data_train, times=200)
  
  tune_spec<-linear_reg(penalty=tune(),mixture=1)%>%
    set_engine("glmnet")
  
  lambda_grid<-grid_regular(penalty(),levels=50)
  
  lasso_grid<-tune_grid(
    wf |> add_model(tune_spec),
    resamples=data_boot,
    grid=lambda_grid
  )
  
  lowest_rmse<-lasso_grid |> 
    select_best('rmse')
  
  final_lasso<-finalize_workflow(
    wf |> add_model(tune_spec),
    lowest_rmse
  )
  
  
  return(list(final_mod=final_lasso))
  
}

rf_fcn_tm<-function(data,var_list='all',dep_var,ra=1,ut_mod='log'){
  
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
  
  browser()
  # split data (may have to do manually later)
  set.seed(123)
  data_split<-initial_time_split(filter_data)
  data_train<-training(data_split)
  data_test<-testing(data_split)
  
  # create receipe
  rec<-recipe(fish_value~.,data=data_train)
  # prep receipe
  
  rec_prep<-rec |> 
    prep()
  
  # create model
  
  tune_spec<-rand_forest(mtry=tune(),trees=1000,min_n=tune())%>%
    set_mode("regression")%>%
    set_engine("ranger")
  
  # create workflow
  wf<-workflow()%>%
    add_recipe(rec) %>%
    add_model(tune_spec)
  
  #vfold_cv for data
  set.seed(123)
  data_folds<-vfold_cv(data_train,v=10)  
  
  # Set up parallel processing
  
  #doParallel::registerDoParallel()
  
  
  rf_grid<-tune_grid(
    wf,
    resamples=data_folds,
    grid=20
  )
  
  lowest_rmse<-rf_grid |> 
    select_best('rmse')
  
  final_rf<-finalize_workflow(
    wf,
    lowest_rmse
  )
  
  
  pred<-predict(final_rf |> fit(filter_data),filter_data) |> 
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
  
  
  #browser()
  opt_out<-optim(par=.1,utility_test,lower=0,method="L-BFGS-B",data=pay_data,a=ra,ut_mod=ut_mod)
  u_i=-opt_out$value
  u_noi=-utility_test(0,pay_data,a=ra,ut_mod=ut_mod)
  
  u_rr=(u_i-u_noi)/abs(u_noi)*100
  
  return(list(final_mod=final_rf,coverage=opt_out$par,u_rr=u_rr))
  
}

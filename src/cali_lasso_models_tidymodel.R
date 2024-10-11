### Script to run lasso regression on cali state wide data

library(tidyverse)
library(glmnet)

library(readxl)
library(wcfish)
library(sf)
library(zoo)
library(tidymodels)


#load catch data

load(here::here("data","fisheries","cali_catch.rda"))

#load weather data
load(here::here("data","environmental","block_beuti.rda"))
load(here::here("data","environmental","block_cuti.rda"))
load(here::here("data","environmental","block_hci.rda"))
load(here::here("data","environmental","block_sst.rda"))
load(here::here("data","environmental","enso_pdo.rda"))

#load designed fucntions
source(here::here("src","fcn","cw_join_cali.R"))
source(here::here("src","fcn","lm_mod_fcn.R"))
source(here::here("src","fcn","pred_fcn.R"))
source(here::here("src","fcn","ins.R"))
source(here::here("src","fcn","utility.R"))

cali_cw<-cali_catch %>% 
  group_by(species_code) %>%
  mutate(mt_per_fisher=case_when(mt_per_fisher==Inf~0,
                                 .default=as.numeric(mt_per_fisher)),
         lb_per_fisher=case_when(lb_per_fisher==Inf~0,
                                 .default=as.numeric(lb_per_fisher))) %>% 
  mutate(roll_value_usd=rollmean(value_usd,3,fill=NA,align="right",na.rm=TRUE),
         roll_landings=rollmean(landings_mt,3,fill=NA,align='right',na.rm=TRUE),
         roll_n_rev=rollmean(rev_per_fisher,3,fill=NA,align='right',na.rm=TRUE),
         roll_n_mt=rollmean(mt_per_fisher,3,fill=NA,align='right',na.rm=TRUE)) %>%
  filter(year>=1988) |> 
  nest() %>% 
  mutate(cw_data=map2(.x=species_code,.y=data,~cw_join_cali(.x,.y)))

### Use tidymodels to build a function that continas a lasso workflow to apply to each data set

lasso_fcn<-function(data,var_list,dep_var){
  
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
  
  
  
  # split data (may have to do manually later)
  set.seed(123)
  data_split<-initial_split(filter_data)
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
  
  data_boot<-bootstraps(data_train)
  
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
  
  last_fit(final_lasso,
           data_split) |> collect_metrics()
  
  lasso_fit<-wf |> 
    add_model(lasso) |> 
    fit(data=data_train)
  
  
  library(vip)
  
  final_lasso %>%
    fit(data_train) %>%
    pull_workflow_fit() %>%
    vi(lambda = lowest_rmse$penalty) %>%
    mutate(
      Importance = abs(Importance),
      Variable = fct_reorder(Variable, Importance)
    ) %>%
    ggplot(aes(x = Importance, y = Variable, fill = Sign)) +
    geom_col() +
    scale_x_continuous(expand = c(0, 0)) +
    labs(y = NULL)
  
  
}
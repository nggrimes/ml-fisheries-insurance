## Main Script


## Load packages


library(tidyverse)
library(readxl)
library(wcfish)
library(sf)
library(zoo)
library(glmnet)
library(kernlab)
library(ranger)
library(RRF)

#load catch data

load(here::here("data","fisheries","cali_catch_detrend_2.rda"))

#load weather data
load(here::here("data","environmental","block_beuti.rda"))
load(here::here("data","environmental","block_cuti.rda"))
load(here::here("data","environmental","block_hci.rda"))
load(here::here("data","environmental","block_sst.rda"))
load(here::here("data","environmental","enso_pdo.rda"))
load(here::here('data','fisheries','squid_bio_all.Rdata'))

#load designed functions
source(here::here("src","fcn","cw_squid.R"))
source(here::here("src","fcn","ut_fcn.R"))
source(here::here("src","fcn","find_m.R"))
source(here::here("src","fcn","market.R"))

source(here::here("src","fcn","analysis_lm.R"))
source(here::here("src","fcn","analysis_rf.R"))
source(here::here("src","fcn","analysis_grrf.R"))
source(here::here("src","fcn","analysis_svm.R"))
source(here::here("src","fcn","analysis_lasso.R"))
source(here::here("src","fcn","tune_map.R"))
source(here::here("src","fcn","tune_lasso.R"))

# Parameters
ra=0.008  #if using log, ra is not necessary, but for 'cara' or 'negexp' it is risk aversion
ut_mod='negexp'  #options so far include 'cara', 'power', and 'negexp'
cov=0.9   # what proportion of mean catch to insure against
m=1     # actuarilly fair premium. Can be adjusted 


### Merge catch and environmental data

port_cw<-cali_catch %>% 
  filter(species_code=='MSQD') %>% 
  mutate(mt_per_detrend=case_when(mt_per_detrend==Inf~0,
                                 .default=as.numeric(mt_per_detrend)),
         lb_per_fisher=case_when(lb_per_fisher==Inf~0,
                                 .default=as.numeric(lb_per_fisher))) %>% 
  filter(year>=1990) |> 
  nest() %>% 
  mutate(cw_data=pmap(list(spp=species_code,data=data),cw_squid))


## Isolate features and measures of interest


var_names<-unique(port_cw$cw_data[[1]]$var)
fish_vars<-unique(port_cw$cw_data[[1]]$fish_var)

var_names<-var_names[-which(var_names %in% 
                              c('amp_beuti',
                                'avg_beuti',
                                'f_beuti',
                                's_beuti',
                                'w_beuti',
                                'amp_cuti',
                                'avg_cuti',
                                'f_cuti',
                                's_cuti',
                                'w_cuti'))]
fish_vars<-fish_vars[which(fish_vars %in% c('mt_per_detrend'))]


#### Linear Models #####

# Create dataframe to loop over all variable names

combo<-expand_grid(var_names,fish_vars) 

# Use tune_map and lm_analysis to run all linear models

time_lm_df<-combo |> 
  mutate(results=map2(.x=var_names,.y=fish_vars,~tune_map('blank',v=.x,fv=.y,
                                                          big_data=port_cw$cw_data[[1]],
                                                          ra=ra,
                                                          ut_mod=ut_mod,
                                                          pred_mod='lm',
                                                          m=m,
                                                          start_year=24,
                                                          coverage=cov))) |> 
  unnest_wider(results)



#### Random forests #####

# create tuning grid

## Order has to be mtry, min.node, max.depth
mtry <- c(2,3,4,5)
min.node <- seq(5,10)
max.depth<-seq(2,4)

rf_grid <- expand.grid(mtry=mtry,min.node=min.node,max.depth=max.depth)

# Convert to tibble for tune_grid
rf_grid <- as_tibble(rf_grid)


# Use tune_map and rf_analysis to select and run all random forest models

time_rf_df<-rf_grid %>% 
  mutate(results=pmap(across(everything(.)),~tune_map(...,v='blank',
                                                      coverage=cov,m=m,
                                                      fv='mt_per_detrend',
                                                      big_data=port_cw$cw_data[[1]],
                                                      ra=ra,
                                                      ut_mod=ut_mod,
                                                      pred_mod='rf'))) |> 
  unnest_wider(results)



best_rf<-time_rf_df[which.min(time_rf_df$rmse_test),]

#### GRRF #####

## Order has to be mtry, min.node, max.depth, gamma
mtry <- c(2,3,4)
nodesize <- seq(2,8,by=2)
maxnodes<-seq(2,5,by=2)
gamma<-seq(0.1,.9,by=.2)

grrf_grid <- expand.grid(mtry=mtry,nodesize=nodesize,maxnodes=maxnodes,gamma=gamma)

# Convert to tibble for tune_grid
grrf_grid <- as_tibble(grrf_grid)

time_grrf_df<-grrf_grid %>% 
  mutate(results=pmap(across(everything(.)),~tune_map(...,v='blank',
                                                      coverage=cov,
                                                      m=m,
                                                      fv='mt_per_detrend',
                                                      big_data=port_cw$cw_data[[1]],
                                                      ra=ra,
                                                      ut_mod=ut_mod,
                                                      pred_mod='grrf'))) |> 
  unnest_wider(results)

best_grrf<-time_grrf_df[which.min(time_grrf_df$rmse_test),]

#### SVM #####


# create tuning grid use sigest to ID good range for hyperparameters

df<-port_cw$cw_data[[1]] |> 
  filter(fish_var=='mt_per_detrend') |> 
  filter(!var %in% c('amp_beuti','avg_beuti','f_beuti','s_beuti','w_beuti','amp_cuti','avg_cuti','f_cuti','s_cuti','w_cuti','year')) |> 
  pivot_wider(names_from = var, values_from = value) |> 
  drop_na()

train_for_sigest <- df %>% slice(1:25) %>% dplyr::select(-year, -fish_value,-fish_var)

# kernlab::sigest expects a matrix (unscaled OK; we'll estimate on raw predictors)
sigest_vals <- sigest(as.matrix(train_for_sigest))

# sigest returns 3 values; use the middle or the [1] and [3] as low/high

sigma_low  <- as.numeric(sigest_vals[1])   # lower
sigma_med  <- as.numeric(sigest_vals[2])   # median-ish
sigma_high <- as.numeric(sigest_vals[3])   # upper

# Build ranges around sigest but clipped to safe bounds:
rbf_min <- max(sigma_low * 0.5, 1e-4)      # don't let it go to 0
rbf_max <- min(sigma_high * 2.0, 1.0)      # cap at 1.0 to avoid flat kernels

# If sigest gave ridiculous numbers, fallback to defaults:
if(!is.finite(rbf_min) | !is.finite(rbf_max) | rbf_min >= rbf_max) {
  rbf_min <- 0.01
  rbf_max <- 0.5
}

cost_vals <- c(0.1, 0.5, 1, 5, 10, 50, 100)
sigma_vals <- exp(seq(log(rbf_min), log(rbf_max), length.out = 6))

svm_grid <- expand.grid(cost = cost_vals, rbf_sigma = sigma_vals)

# Convert to tibble for tune_grid
svm_grid <- as_tibble(svm_grid)


time_svm_df<-svm_grid %>% 
  mutate(results=pmap(across(everything(.)),~tune_map(...,v='blank',
                                                      coverage=cov,
                                                      m=m,
                                                      fv='mt_per_detrend',
                                                      big_data=port_cw$cw_data[[1]],
                                                      ra=ra,
                                                      ut_mod=ut_mod,
                                                      pred_mod='svm'))) |> 
  unnest_wider(results)



best_svm<-time_svm_df[which.min(time_svm_df$rmse_test),]



#### Lasso ####

#Lasso needs it own tuning wrapper because of the way glmnet handles lambda hyperparameter
lambda_grid <- 10^seq(3, -3, length.out = 100)

time_lasso_df<-tune_lasso(lambda_grid,v='blank',
                          coverage=cov,
                          m=m,
                          fv='mt_per_detrend',
                          big_data = port_cw$cw_data[[1]],
                          ra=ra,
                          ut_mod=ut_mod,
                          pred_mod='lasso')

best_lasso<-time_lasso_df[which.min(time_lasso_df$rmse_test),]


#### Combine best models ####

clean_lm<-time_lm_df |> 
  mutate(pred_mod=var_names) |> 
  dplyr::select(u_rr:pay_threshold_vec) 

clean_rf<-best_rf |> 
  dplyr::select(u_rr:pay_threshold_vec)

clean_grrf<-best_grrf |>
  dplyr::select(u_rr:pay_threshold_vec)

clean_svm<-best_svm |>
  dplyr::select(u_rr:pay_threshold_vec)

clean_lasso<-best_lasso |>
  dplyr::select(u_rr:mods)

all_models<-bind_rows(clean_lm,
                      clean_rf,
                      clean_grrf,
                      clean_svm,
                      clean_lasso)
models<-all_models |> 
  mutate(m_eq=map2(.x=payout_vec,.y=prem_vec,~market(.x,.y,
                                                       fish_value=df$fish_value[25:34],
                                                       ra=ra,
                                                       ut_mod=ut_mod))) |> 
  unnest_wider(m_eq)


save(models,file=here::here("data","output","squid_cara10-5_c90.rda"))

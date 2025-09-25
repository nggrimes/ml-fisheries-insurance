### Squid Lasso

library(tidyverse)
library(readxl)
library(wcfish)
library(sf)
library(zoo)
library(glmnet)


#load catch data

load(here::here("data","fisheries","cali_catch_detrend_2.rda"))

#load weather data
load(here::here("data","environmental","block_beuti.rda"))
load(here::here("data","environmental","block_cuti.rda"))
load(here::here("data","environmental","block_hci.rda"))
load(here::here("data","environmental","block_sst.rda"))
load(here::here("data","environmental","enso_pdo.rda"))
load(here::here('data','fisheries','squid_bio_all.Rdata'))

#load designed fucntions
source(here::here("src","fcn","cw_squid.R"))
source(here::here("src","fcn","ci_fcn.R"))
source(here::here("src","fcn","ut_fcn.R"))
source(here::here("src","fcn","find_m.R"))
source(here::here("src","fcn","inv_ut.R"))

port_cw<-cali_catch %>% 
  filter(species_code=='MSQD') %>% 
  mutate(mt_per_fisher=case_when(mt_per_fisher==Inf~0,
                                 .default=as.numeric(mt_per_fisher)),
         lb_per_fisher=case_when(lb_per_fisher==Inf~0,
                                 .default=as.numeric(lb_per_fisher))) %>% 
  filter(year>=1988) |> 
  nest() %>% 
  mutate(cw_data=pmap(list(spp=species_code,data=data),cw_squid))


# Practice with one metric

combo<-port_cw$cw_data[[1]] %>% 
  filter(!var %in% c('amp_beuti','avg_beuti','f_beuti','s_beuti','w_beuti','amp_cuti','avg_cuti','f_cuti','s_cuti','w_cuti')) |> 
  filter(year>=1990)

fish_vars<-unique(port_cw$cw_data[[1]]$fish_var)

fish_vars<-fish_vars[-which(fish_vars %in% c('landings_lb','lb_per_fisher'))]


lasso_analysis<-function(index,data,coverage,m,ra,ut_mod){
 
  
   train<-data[index,]
  test<-data[-index,]
  
lasso_x<-train |> 
  dplyr::select(-c(fish_var,year,fish_value)) |> 
  as.matrix()

lasso_y<-train |>
  dplyr::select(fish_value) |> 
  as.matrix()

lasso_fit <- cv.glmnet(lasso_x, lasso_y, alpha = 1,nfolds=8)

payout_vec<-predict(lasso_fit, newx = lasso_x, s = "lambda.1se") |> 
  map_dbl(.f=~max(coverage*mean(lasso_y)-.x,0))

# Get fitted RMSE and R-squared

residuals <- lasso_y - predict(lasso_fit, newx = lasso_x, s = "lambda.1se")

rmse <- sqrt(mean(residuals^2, na.rm = TRUE))

rsq <- 1 - (sum(residuals^2, na.rm = TRUE) / sum((lasso_y - mean(lasso_y))^2, na.rm = TRUE))

prem<-mean(payout_vec)*m


lasso_x_test<-test |> 
  dplyr::select(-c(fish_var,year,fish_value)) |> 
  as.matrix()

lasso_y_test<-test |>
  dplyr::select(fish_value) |> 
  as.matrix()

payout_out_test<-predict(lasso_fit, newx = lasso_x_test, s = "lambda.1se")|> 
  map_dbl(.f=~max(coverage*mean(lasso_y)-.x,0))



u_out_i<-ut_fcn(value=lasso_y_test,payout_vec=payout_out_test,premium=prem,ra=0.1,mod=ut_mod)
u_out_noi<-ut_fcn(value=lasso_y_test,payout_vec=0,premium=0,ra=0.1,mod=ut_mod)

u_noi_test<-u_out_noi$ut
pi_noi_test<-u_out_noi$profit

u_i_test<-u_out_i$ut
pi_i_test<-u_out_i$profit

r_i_test<-mean(pi_i_test)-inv_ut(u_i_test,ra=ra,mod=ut_mod)
r_noi_test<-mean(pi_noi_test)-inv_ut(u_noi_test,ra=ra,mod=ut_mod)

u_rr<-(u_i_test-u_noi_test)/abs(u_noi_test)

r_rr<-(r_i_test-r_noi_test)/abs(r_noi_test)

#get average insurance profits

ins_pi<-prem*nrow(test)-sum(payout_out_test)

m_out <- tryCatch({
  result <- uniroot(find_m,
                    interval = c(0.01, 10),
                    data = test$fish_value,
                    payout_vec = payout_out_test,
                    prem = prem,
                    ra = ra,
                    mod = ut_mod)
  result$root  # extract root only if uniroot succeeds
}, error = function(e) {
  #message("uniroot failed: ", e$message)
  NA  # or another default/fallback value
})

return(data.frame(
    u_rr=u_rr,
    m_out=m_out,
    u_i=u_i_test,
    u_noi=u_noi_test,
    r_rr=r_rr,
    ins_pi=ins_pi,
    rmse=rmse,
    rsq=rsq
  ))
}

boot_lasso<-function(fv,big_data,coverage=1,m=1,split_t=25,ra,ut_mod){
  
  df<-big_data |> 
    filter(fish_var==fv) |> 
    pivot_wider(names_from = var, values_from = value)
  
  bootstrap_samples <- replicate(
    1000,
    sample(nrow(df), size = split_t, replace = FALSE),
    simplify = FALSE
  )
  
  
  boot_out<-bootstrap_samples %>% map_df(.f=~lasso_analysis(.x,data=df,coverage=coverage,m=m,ut_mod=ut_mod,ra=ra))

  # Get CI for each metric
  
  
  u_ci<-ci_fcn(boot_out$u_rr,alpha=0.05)
  m_ci<-ci_fcn(boot_out$m_out,alpha=0.05)
  r_ci<-ci_fcn(boot_out$r_rr,alpha=0.05)
  ins_ci<-ci_fcn(boot_out$ins_pi,alpha=0.05)
  rmse_ci<-ci_fcn(boot_out$rmse,alpha=0.05)
  rsq_ci<-ci_fcn(boot_out$rsq,alpha=0.05)
  
  
  return(data.frame(
    ut_i=mean(boot_out$u_i,na.rm=TRUE),
    ut_noi=mean(boot_out$u_noi,na.rm=TRUE),
    u_rr=mean(boot_out$u_rr,na.rm=TRUE),
    m_out=mean(boot_out$m_out,na.rm=TRUE),
    urr_ci_hi=u_ci$hi,
    urr_ci_lo=u_ci$lo,
    m_out_hi=m_ci$hi,
    m_out_lo=m_ci$lo,
    r_rr=mean(boot_out$r_rr,na.rm=TRUE),
    r_ci_hi=r_ci$hi,
    r_ci_lo=r_ci$lo,
    ins_pi=mean(boot_out$ins_pi,na.rm=TRUE),
    ins_ci_hi=ins_ci$hi,
    ins_ci_lo=ins_ci$lo,
    rmse=mean(boot_out$rmse,na.rm=TRUE),
    rmse_hi=rmse_ci$hi,
    rmse_lo=rmse_ci$lo,
    rsq=mean(boot_out$rsq,na.rm=TRUE),
    rsq_hi=rsq_ci$hi,
    rsq_lo=rsq_ci$lo
  ))
  
}

lasso_df<-as.data.frame(fish_vars) %>% 
  mutate(results=map(.x=fish_vars,~boot_lasso(.x,big_data=combo,ra=2,ut_mod='cara')))


lasso_df<-lasso_df %>%
  unnest_wider(results)

save(lasso_df,file=here::here("data","output","lasso_df_6-7_cara.Rdata"))

### Ranger implementation 

library(ranger)

rf_model <- ranger(fish_value ~ ., data = example, keep.inbag = TRUE, oob.error = TRUE)

# inbag counts
inbag_matrix <- rf_model$inbag.counts

# OOB matrix (TRUE if not in-bag)
oob_matrix <- lapply(inbag_matrix, function(x) x == 0)

# OOB predictions



rf_analysis<-function(oob_data,rf_model,data,m,ra,ut_mod){
oob_index<-which(oob_data==TRUE)
inbag_index<-which(oob_data==FALSE)

payout_vec<-predict(rf_model,data=data[inbag_index,])$predictions |> 
  map_dbl(.f=~max(coverage*mean(data$fish_value[inbag_index])-.x,0))

prem<-mean(payout_vec)

residuals <- data$fish_value[inbag_index] - predict(rf_model, data = data[inbag_index,])$predictions

rmse <- sqrt(mean(residuals^2, na.rm = TRUE))

rsq <- 1 - (sum(residuals^2, na.rm = TRUE) / sum((data$fish_value[inbag_index] - mean(data$fish_value))^2, na.rm = TRUE))


oob_pay<-predict(rf_model,data=data[oob_index,])$predictions |> 
  map_dbl(.f=~max(coverage*mean(data$fish_value[inbag_index])-.x,0))


u_out_i<-ut_fcn(data$fish_value[oob_index],oob_pay,prem,m=1,ra=ra,ut_mod)

u_out_noi<-ut_fcn(data$fish_value[oob_index],0,0,m=1,ra=ra,ut_mod)

u_noi_test<-u_out_noi$ut
pi_noi_test<-u_out_noi$profit

u_i_test<-u_out_i$ut
pi_i_test<-u_out_i$profit

r_i_test<-mean(pi_i_test)-inv_ut(u_i_test,ra=ra,mod=ut_mod)
r_noi_test<-mean(pi_noi_test)-inv_ut(u_noi_test,ra=ra,mod=ut_mod)

u_rr<-(u_i_test-u_noi_test)/abs(u_noi_test)

r_rr<-(r_i_test-r_noi_test)/abs(r_noi_test)

#get average insurance profits

ins_pi<-prem*length(oob_index)-sum(oob_pay)

m_out <- tryCatch({
  result <- uniroot(find_m,
                    interval = c(0.01, 10),
                    data = data$fish_value[oob_index],
                    payout_vec = oob_pay,
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

boot_rf<-function(fv,big_data,coverage=1,m=1,ra,ut_mod){

  df<-big_data |> 
    filter(fish_var==fv) |> 
    pivot_wider(names_from = var, values_from = value)
  
  rf_model <- ranger(fish_value ~ ., data = df, keep.inbag = TRUE, oob.error = TRUE)
  
  # inbag counts
  inbag_matrix <- rf_model$inbag.counts
  
  # OOB matrix (TRUE if not in-bag)
  oob_matrix <- lapply(inbag_matrix, function(x) x == 0)  
  

boot_out<-oob_matrix |> map_df(.f=~rf_analysis(oob_data=.x,data=df,rf_model=rf_model,m=1,ra=1,ut_mod='log'))
 
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

rf_df<-as.data.frame(fish_vars) %>% 
  mutate(results=map(.x=fish_vars,~boot_rf(.x,big_data=combo,ra=2,ut_mod='log')))

rf_df<-rf_df %>%
  unnest_wider(results)

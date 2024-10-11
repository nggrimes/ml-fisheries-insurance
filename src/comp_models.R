### Compare model outputs

# load the model outputs

load(here::here("data","output","port_lm_output.rda"))
load(here::here("data","output","port_lasso_output.rda"))
load(here::here("data","output","cali_lm_output.rda"))
load(here::here("data","output","cali_lasso_output.rda"))


port_lass_comp<-port_mt_lasso$u_rr-port_mt_lm$u_rr
cali_lass_comp<-cali_mt_lasso$u_rr-cali_mt_lm$u_rr

cali_lass_rev_comp<-cali_rev_lasso$u_rr-cali_rev_lm$u_rr
cali_lass_per_comp<-cali_per_fisher_lasso$u_rr-cali_per_lm$u_rr

port_lass__rev_comp<-port_rev_lasso$u_rr-port_rev_lm$u_rr
port_lass__per_comp<-port_per_fisher_lasso$u_rr-port_per_lm$u_rr


mean(cali_lass_comp)
mean(cali_lass_rev_comp)
mean(cali_lass_per_comp)
mean(port_lass_comp)
mean(port_lass__rev_comp)
mean(port_lass__per_comp)

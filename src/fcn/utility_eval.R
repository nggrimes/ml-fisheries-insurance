#### model prediction and utility calculating on test data
utility_eval<-function(data,mod,var_name,ut_mod='cara',m=1,ra=0.08){
  
dat<-data |> 
  filter(fish_var==var_name) |> 
  drop_na()



if(class(mod$final_mod)=='workflow'){
  train<-dat |> 
    filter(year<2013) |> 
    pivot_wider(
      names_from=var,
      values_from=value
    ) |> 
    drop_na() |> 
    select(-year,-fish_var)
  
  test<-dat |> 
    filter(year>=2013) |>
    pivot_wider(
      names_from=var,
      values_from=value
    ) |> 
    drop_na() |> 
    select(-year,-fish_var)
  
  mod<-mod$final_mod
  
  train_pred<-predict(mod |> fit(train),train) |> 
    rename(pred=1)
  
  test_pred<-predict(mod |> fit(train),test) |> 
    rename(pred=1)
  
  pred<-rbind(train_pred,test_pred)
  
  train<-dat |> 
    filter(year<2013) |> 
    pivot_wider(
      names_from=var,
      values_from=value
    ) |> 
    drop_na() 
  
  test<-dat |> 
    filter(year>=2013) |>
    pivot_wider(
      names_from=var,
      values_from=value
    ) |> 
    drop_na()
  
} else if(class(mod$final_mod)=='ranger'){
  mod<-mod$final_mod
  
  train<-dat |> 
    filter(year<2013) |> 
    pivot_wider(
      names_from=var,
      values_from=value
    ) |> 
    drop_na() |> 
    select(-year,-fish_var)
  
  test<-dat |> 
    filter(year>=2013) |>
    pivot_wider(
      names_from=var,
      values_from=value
    ) |> 
    drop_na() |> 
    select(-year,-fish_var)
  
  train_pred<-predict(mod,train)$predictions |> 
    as.data.frame() |> 
    rename(pred=1)
  
  test_pred<-predict(mod,test)$predictions |>
    as.data.frame() |> 
    rename(pred=1)
  
  pred<-rbind(train_pred,test_pred)
  
  train<-dat |> 
    filter(year<2013) |> 
    pivot_wider(
      names_from=var,
      values_from=value
    ) |> 
    drop_na()
  
  test<-dat |> 
    filter(year>=2013) |>
    pivot_wider(
      names_from=var,
      values_from=value
    ) |> 
    drop_na()
}else{
  filt_var<-mod$best_rmse$var
  mod<-mod$final_mod
  
  train<-dat |> 
    filter(year<2013) |> 
    filter(var==filt_var) |> 
    drop_na()
  
  train_pred<-predict(mod,train) |> 
    as.data.frame() |> 
    rename(pred=1)
  
  test<-dat |> 
    filter(year>=2013) |> 
    filter(var==filt_var) |> 
    drop_na()
  
  
  test_pred<-predict(mod,test) |>
    as.data.frame() |> 
    rename(pred=1)
  
  pred<-rbind(train_pred,test_pred)
}


pay_data<-rbind(train,test) |> 
  select(year,fish_value) |> 
  cbind(pred) |> 
  mutate(raw_pay=mean(fish_value)-pred) |> 
  mutate(raw_pay=case_when(raw_pay<0~0,
                           TRUE~raw_pay)) |> 
  mutate(scale_pay=raw_pay/max(fish_value),
         fish_value=fish_value/max(fish_value)  # This step 'normalizes' differences in payouts to compare utility across all fisheries
  )


premium<-pay_data |> 
  filter(year<2013) 

test_data<-pay_data |> 
  filter(year>=2013)


l_val<-rep(0,nrow(test_data))
u_i_val<-rep(0,nrow(test_data))
u_noi_val<-rep(0,nrow(test_data))
prem_vec<-rep(0,nrow(test_data))

for(i in 1:nrow(test_data)){
  
  temp<-pay_data |> 
    filter(year<(2013+i))
  
  t_opt<-optim(par=.1,utility_test,lower=0,upper=1.5,method="L-BFGS-B",data=temp,a=0.08,ut_mod='cara',m=1)
l_val[i]<-t_opt$par


profit<-temp$fish_value[nrow(temp)]+temp$scale_pay[nrow(temp)]*l_val[i]-mean(temp$scale_pay*l_val[i])*m

if(ut_mod=="log"){
  profit[which(profit<=0)]<-0.00001
  u_i_val[i]<-mean(log(profit),na.rm=TRUE)
} else if(ut_mod=="cara"){
  u_i_val[i]<-mean((1-exp(-ra*profit))/ra,na.rm=TRUE)
}else{
  u_i_val[i]<-mean((profit-1)^(1-ra)/(1-ra),na.rm=TRUE)
}

if(ut_mod=="log"){
  
  u_noi_val[i]<-mean(temp$fish_value[nrow(temp)],na.rm=TRUE)
  
} else if(ut_mod=="cara"){
  u_noi_val[i]<-mean((1-exp(-ra*temp$fish_value[nrow(temp)]))/ra,na.rm=TRUE)
}else{
  ut_noi_val[i]<-mean((temp$fish_value[nrow(temp)]-1)^(1-ra)/(1-ra),na.rm=TRUE)

}


prem_vec[i]<-mean(temp$raw_pay*l_val[i])*m
  
  }

test_u_i=mean(u_i_val)
test_u_noi=mean(u_noi_val)

test_u_rr<-(test_u_i-test_u_noi)/abs(test_u_noi)

if(test_u_rr<0){
  test_u_rr<-0
  prem_vec<-rep(0,length(prem_vec))
  l_val<-rep(0,length(l_val))
  }

return(list(test_u_rr=test_u_rr,test_u_i=test_u_i,test_u_noi=test_u_noi,prem_vec=prem_vec,l_val=l_val,u_i_val=u_i_val,u_noi_val=u_noi_val))
}

### Time series RF
prac<-combo |> 
  filter(fish_var=='mt_per_detrend') |> 
  pivot_wider(names_from = var, values_from = value)

store_df<-data.frame(
  year=integer(),
  premium=numeric(),
  pay_test=numeric(),
  test_pred=numeric(),
  residual=numeric(),
  value=numeric()
)

t_year=2013
end_yr=max(prac$year)

for(i in t_year:end_yr){

  train<-prac |> 
    filter(year < i) |> 
    select(-year, -fish_var)
  
  test<-prac |>
    filter(year == i) |> 
    select(-year, -fish_var)
  
  rf_model<-ranger(fish_value~.,
                   data=train)
  
  pay_train<-predict(rf_model, data=train)$predictions |> 
    map_dbl(.f=~max(coverage*mean(train$fish_value)-.x,0))
  
  prem<-mean(pay_train)*m
  
  pay_test<-predict(rf_model, data=test)$predictions |> 
    map_dbl(.f=~max(coverage*mean(train$fish_value)-.x,0))
  
  test_pred<-predict(rf_model, data=test)$predictions 
  
  residual<- test_pred - test$fish_value
  
  temp_df<-data.frame(
    year=i,
    premium=prem,
    pay_test=pay_test,
    test_pred=test_pred,
    residual=residual,
    value=test$fish_value
  )
  
  store_df<-bind_rows(store_df, temp_df)
}

rmse <- sqrt(mean(store_df$residual^2, na.rm = TRUE))

rsq <- 1 - (sum(store_df$residual^2, na.rm = TRUE) / sum((store_df$value - mean(store_df$value))^2, na.rm = TRUE))

u_i<-ut_fcn(store_df$value,store_df$pay_test,store_df$premium,m=1,ra=1,'log')$ut
u_noi<-ut_fcn(store_df$value,0,0,m=1,ra=1,'log')$ut

u_rr<- (u_i - u_noi)/abs(u_noi)

#### Try with lasso

store_df<-data.frame(
  year=integer(),
  premium=numeric(),
  pay_test=numeric(),
  test_pred=numeric(),
  residual=numeric(),
  value=numeric()
)

t_year=2013
end_yr=max(prac$year)

for(i in t_year:end_yr){
 
  train<-prac |> 
    filter(year < i) |> 
    select(-year, -fish_var)
  
  train_x<-train |> 
    select(-fish_value) |> 
    as.matrix()
  
  train_y<-train$fish_value
  
  test<-prac |>
    filter(year == i) |> 
    select(-year, -fish_var)
  
  test_x<-test |>
    select(-fish_value) |> 
    as.matrix()
  
  test_y<-test$fish_value
  
  model<-cv.glmnet(train_x, train_y, alpha = 1,nfolds=7)
  
  pay_train<-predict(model, newx=train_x,s='lambda.1se') |> 
    map_dbl(.f=~max(coverage*mean(train_y)-.x,0))
  
  prem<-mean(pay_train)*m
  
  pay_test<-predict(model, newx=test_x,s='lambda.1se')|> 
    map_dbl(.f=~max(coverage*mean(train_y)-.x,0))
  
  test_pred<-as.double(predict(model, newx=test_x,s='lambda.1se'))
  
  residual<- test_pred - test$fish_value
  
  temp_df<-data.frame(
    year=i,
    premium=prem,
    pay_test=pay_test,
    test_pred=test_pred,
    residual=residual,
    value=test$fish_value
  )
  
  store_df<-bind_rows(store_df, temp_df)
}

rmse <- sqrt(mean(store_df$residual^2, na.rm = TRUE))

rsq <- 1 - (sum(store_df$residual^2, na.rm = TRUE) / sum((store_df$value - mean(store_df$value))^2, na.rm = TRUE))


#### Do it with regression on just sp_beuti

store_df<-data.frame(
  year=integer(),
  premium=numeric(),
  pay_test=numeric(),
  test_pred=numeric(),
  residual=numeric(),
  value=numeric()
)

t_year=2013
end_yr=max(prac$year)

for(i in t_year:end_yr){

  train<-prac |> 
    filter(year < i) |> 
    select(-year, -fish_var)
  
  test<-prac |>
    filter(year == i) |> 
    select(-year, -fish_var)
  
  rf_model<-lm(fish_value~sp_beuti,
                   data=train)
  
  pay_train<-predict(rf_model, data=train)|> 
    map_dbl(.f=~max(coverage*mean(train$fish_value)-.x,0))
  
  prem<-mean(pay_train)*m
  
  beta<-coef(rf_model)[2]
  y_int= coef(rf_model)[1]
  pay_test<-(test$sp_beuti*beta+y_int) |> 
    map_dbl(.f=~max(coverage*mean(train$fish_value)-.x,0))
  
  test_pred<-test$sp_beuti*beta+y_int
  
  residual<- test_pred - test$fish_value
  
  temp_df<-data.frame(
    year=i,
    premium=prem,
    pay_test=pay_test,
    test_pred=test_pred,
    residual=residual,
    value=test$fish_value
  )
  
  store_df<-bind_rows(store_df, temp_df)
}

### analysis for linear models on a single fold of data

analysis_lm<-function(index,data,coverage,m,ra,ut_mod,pred_mod){

  
  train<-data[index,]
  
  end<-index[length(index)]+1
  test<-data[end,]
  
    
  mod<-lm(fish_value~value,data=train)
  
  
  tick<-coefficients(mod)[2]
  
  
  
  strike<-((mean(data$fish_value)-coefficients(mod)[1])/tick)*coverage   #coverage is how much to protect 0.65 is way below average, 0.9 is small, 1 is average
  
  pred_train<-predict(mod, newdata = train)
  
  pred_test<-predict(mod, newdata = test)
  
  
  # get payouts
  
  payout_vec_train<-train$value |> 
    map_dbl(.f=~max(tick*(strike-.x),0))
  
  payout_vec_test<-test$value |> 
    map_dbl(.f=~max(tick*(strike-.x),0))
  

  residuals <- train$fish_value - pred_train
  
  rmse <- sqrt(mean(residuals^2, na.rm = TRUE))
  
  rsq <- 1 - (sum(residuals^2, na.rm = TRUE) / sum((train$fish_value - mean(train$fish_value))^2, na.rm = TRUE))
  
  prem<-mean(payout_vec_train,na.rm=TRUE)
  
  
  pay_threshold<-coverage*mean(train$fish_value)
  
  
  # return results
  return(tibble(payout_vec_test=payout_vec_test,
                rsq=rsq,
                prem=prem,
                rmse=rmse,
                test_value=test$fish_value,
                pred=as.numeric(pred_test),
                train_vec=list(pred_train),
                mods=list(mod),
                pay_threshold=pay_threshold,
  ))
  
}

analysis_rf<-function(index,data,coverage,controls,m,ra,ut_mod,pred_mod){
  
  data <- data |>
    dplyr::select(-c(,"pdo","oni","chci","sti","relax","sp_cuti"))

  train<-data[index,]
  
  end<-index[length(index)]+1
  test<-data[end,]
    
  exclude_vars <- c("year", "fish_var")
  
  # Create formula: response ~ all other columns except those in exclude_vars
  predictors <- setdiff(names(train), c("fish_value", exclude_vars))
  form <- reformulate(predictors, response = "fish_value")
  
  set.seed(123)
  rf_mod <- ranger::ranger(formula=form, 
                           data = train, 
                           importance = 'impurity', 
                           num.trees = 1000, 
                           mtry = controls[1], 
                           min.node.size = controls[2],
                           max.depth = controls[3])
  
  pred_train<-predict(rf_mod, data = train)$predictions
  
  pred_test<-predict(rf_mod, data = test)$predictions
  
  payout_vec_train <- pred_train |> 
    map_dbl(.f=~max(coverage*mean(train$fish_value)-.x,0))
  
  payout_vec_test <- pred_test |> 
    map_dbl(.f=~max(coverage*mean(train$fish_value)-.x,0))

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
                mods=list(rf_mod),
                pay_threshold=pay_threshold,
  ))
  
}


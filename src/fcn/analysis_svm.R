analysis_svm<-function(index,data,coverage,controls,m,ra,ut_mod,pred_mod){
  
  # exclude sti, relax and sp_cuti due to high correlations and low predictability
  data <- data |>
    dplyr::select(-c(,"pdo","oni","chci","sti","relax","sp_cuti"))
  
  train<-data[index,]
  
  end<-index[length(index)]+1
  test<-data[end,]

  #Make data into a matrix for ksvm
  train_x<-train |> 
    dplyr::select(-c(fish_var,year,fish_value)) |> 
    as.matrix()
  
  train_y<-train |> 
    dplyr::select(fish_value) |> 
    as.matrix()
  
  test_y<-test |> 
    dplyr::select(fish_value) |> 
    as.matrix()
  
  test_x<-test |> 
    dplyr::select(-c(fish_var,year,fish_value)) |> 
    as.matrix()
  
  set.seed(123)
  svm_fit <- ksvm(
    train_x, train_y,
    type = "eps-svr",       # regression (use "C-svc" for classification)
    kernel = "rbfdot",
    C = controls[1],                  # cost
    kpar = list(sigma = controls[2]) # RBF width parameter
  )
  
  pred_test <- predict(svm_fit, test_x)
  pred_train<-predict(svm_fit,train_x)
  
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
                mods=list(svm_fit),
                pay_threshold=pay_threshold,
  ))
  
}


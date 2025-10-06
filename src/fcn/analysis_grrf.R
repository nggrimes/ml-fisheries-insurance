analysis_grrf<-function(index,data,coverage,controls,m,ra,ut_mod,pred_mod){
  
  data <- data |> 
    dplyr::select(-c("sti","relax","sp_cuti"))
  
  train<-data[index,]
  
  end<-index[length(index)]+1
  test<-data[end,]
  
  
  y<-train$fish_value
  x<-train |> 
    dplyr::select(-c("year","fish_var","fish_value"))
  
  y_test<-test$fish_value
  x_test<-test |> 
    dplyr::select(-c("year","fish_var","fish_value"))
  
  base_rf<-RRF(x,y,
               flagReg=0,
               mtry=controls[1],
               nodesize=controls[2],
               maxnodes=controls[3])
  
  imp<-base_rf$importance[,1]
  
  imp<-imp+1e-6 #avoid zeros
  
  coefReg<-(imp/max(imp))^controls[4]  #controls four has to be gamma the regularization parameter
  
  grrf<-RRF(x,y,
            flagReg=1,
            coefReg=coefReg,
            ntree=1000,
            mtry=controls[1],
            nodesize=controls[2],
            maxnodes=controls[3]
  )
  
  
  pred_train<-predict(grrf,x)
  
  pred_test<-predict(grrf,x_test)
  
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
                mods=list(grrf),
                pay_threshold=pay_threshold
  ))
  
}

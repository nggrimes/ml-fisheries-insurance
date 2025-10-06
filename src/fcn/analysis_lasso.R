analysis_lasso<-function(index,data,coverage,controls,m,ra,ut_mod,pred_mod){
  # exclude sti, relax and sp_cuti due to high correlations and low predictability

  
  train<-data[index,]
  
  end<-index[length(index)]+1
  test<-data[end,]
  
  
  #Make data into a matrix for glmnet
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
  
  #Build model
  set.seed(123)
  las_mod<-glmnet(train_x, train_y, alpha = 1,lambda=controls,standardize=TRUE)
  
  #make prediction
  pred_train<- predict(las_mod, newx = train_x)
  
  pred_test<- predict(las_mod, newx = test_x)
  
  # get payouts as matrix
  payout_train_df<-matrix(NA,nrow=nrow(train_x),ncol=length(controls)) |> 
    as.data.frame()
  
  for(i in 1:nrow(payout_train_df)){
    for(j in 1:ncol(payout_train_df)){
      payout_train_df[i,j]<-max(coverage*mean(train_y)-pred_train[i,j],0)
    }
  }
  #get payouts
  
  
  payout_vec_test<-pred_test |> 
    map_dbl(.f=~max(coverage*mean(train_y)-.x,0))
  
  
  
  # p_df<-data.frame(x=train$year,fish=train$fish_value,pred=pred_train) |> pivot_longer(cols=c(-x),names_to='mod')
  # ggplot(p_df,aes(x=x,y=value,color=mod))+geom_point()
  # browser()
  residuals <- train$fish_value - pred_train
  
  rmse <- sqrt(colMeans(residuals^2, na.rm = TRUE))
  
  rsq <- 1 - (colSums(residuals^2, na.rm = TRUE) / sum((train$fish_value - mean(train$fish_value))^2, na.rm = TRUE))
  
  prem<-colMeans(payout_train_df,na.rm=TRUE)
  
  
  # return results
  return(tibble(payout_vec_test=list(payout_vec_test),
                rsq=list(rsq),
                prem=list(prem),
                rmse=list(rmse),
                test_value=test$fish_value,
                pred=list(pred_test),
                train_vec=list(pred_train),
                mods=list(las_mod)
  ))
  
}
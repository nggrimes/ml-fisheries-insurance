pred_fcn<-function(input,output,data,model){
  filtered_data=data |> 
    filter(fish_var==output & var==input)

    out<-predict(model,newdata=filtered_data)
  
    out[which(out<0)]<-0
  return(out)
}

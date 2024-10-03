# Single linear model purrr function


lm_mod_fcn<-function(x,y,data){
  browser()
  filter_data<-data[[1]] %>% 
    filter(fish_var==y & var==x)
  
  
  lm_mod<-lm(fish_value~value,data=filter_data)
  
  return(lm_mod)
}

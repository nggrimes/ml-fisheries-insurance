# Single linear model purrr function


lm_mod_fcn<-function(x,y,data){
  #browser()
  filter_data<-data %>% 
    filter(fish_var==y & var==x) %>% 
    mutate(fish_value=case_when(fish_value==Inf~0,
                                .default=as.numeric(fish_value)))
  
  
  lm_mod<-lm(fish_value~value,data=filter_data)
  
  return(lm_mod)
}

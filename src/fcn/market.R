market<-function(payout,prem,fish_value,ra,ut_mod){

  pay<-unlist(payout)
  premium<-unlist(prem)
  
  if(sum(pay)==0){
    return(data.frame(u_rr_m=NA,lr_m=NA))
  } else{
    
    u_noi<-ut_fcn(fish_value,0,0,ra=ra,mod=ut_mod)$ut
    
    m_eq<-uniroot(find_m,interval=c(0.001,10),
            data=fish_value,
            payout_vec=pay,
            prem_in=premium,
            ra=ra,
            mod=ut_mod,
            u_noi=u_noi)$root
    
    # get lr with new m
lr_m<-sum(pay)/sum(m_eq*premium)
    
    return(data.frame(m_eq=m_eq,lr_m=lr_m))
  }
  
}

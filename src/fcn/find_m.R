find_m<-function(m_in,data,payout_vec,prem,ra,mod){
  # m is the multiplier for the premium
  # we want to find the m that maximizes the utility
  
  u_out<-ut_fcn(data,payout_vec,prem,m=m_in,ra=ra,mod=mod)[1]-ut_fcn(data,0,0,m=m_in,ra=ra,mod=mod)[1]
  
  return(u_out$ut)
}
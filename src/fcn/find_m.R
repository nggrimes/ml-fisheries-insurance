find_m<-function(m_in,data,payout_vec,prem_in,ra,mod,u_noi){
  # m is the multiplier for the premium
  # we want to find the m that maximizes the utility
  
  prem=m_in*prem_in
  
  u_out<-ut_fcn(data,payout_vec,prem,ra=ra,mod=mod)[1]-u_noi
  
  return(u_out$ut)
}

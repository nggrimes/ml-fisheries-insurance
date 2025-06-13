inv_ut<-function(ra,eu,mod){
  if(mod=='cara'){
    return(-log(1-eu*ra)/ra)
  } else if (mod=='log'){
    return(exp(eu))
  } else if (mod=='power'){
    return(eu*(1-ra)^(1/(1-ra)))
  }
  
}
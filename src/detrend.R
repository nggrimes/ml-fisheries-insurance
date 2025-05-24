### GMM Detrend Estimator

load(here::here("data","fisheries","cali_catch.rda"))

library(MASS)
library(tidyverse)



i_vec<-c("landings_mt","value_usd","rev_per_fisher","mt_per_fisher")
j_vec<-unique(cali_catch$species_code)

store_df<-data.frame(species_code=character(),
                     year=numeric(),
                     cdfw_name=character(),
                     comm_name=character(),
                     mgmt_group=character(),
                     value_usd=numeric(),
                     landings_mt=numeric(),
                     landings_lb=numeric(),
                     price_usd_lb=numeric(),
                     n_fisher=numeric(),
                     rev_per_fisher=numeric(),
                     mt_per_fisher=numeric(),
                     lb_per_fisher=numeric(),
                     mt_detrend=numeric(),
                     rev_detrend=numeric(),
                     per_detrend=numeric(),
                     per_mt_detrend=numeric())

for(j in 1:length(j_vec)){
  temp_df<-cali_catch |> 
    filter(species_code==j_vec[j] & year>=1985)|> 
    mutate(rev_per_fisher=case_when(n_fisher==0~0,
                                    TRUE~rev_per_fisher),
           mt_per_fisher=case_when(n_fisher==0~0,
                                   TRUE~mt_per_fisher),
           lb_per_fisher=case_when(n_fisher==0~0,
                                   TRUE~lb_per_fisher))
  
  print(unique(temp_df$species_code))
  
  md_mt=rlm(landings_mt~year,data=temp_df,method="MM")
  md_rev=rlm(value_usd~year,data=temp_df,method="MM")
  md_per=rlm(rev_per_fisher~year,data=temp_df,method="MM")
  md_per_mt=rlm(mt_per_fisher~year,data=temp_df,method="MM")
  
  pred_mt<-predict(md_mt)
  pred_rev<-predict(md_rev)
  pred_per<-predict(md_per)
  pred_per_mt<-predict(md_per_mt)
  
  temp_df$mt_detrend<-temp_df$landings_mt-pred_mt
  temp_df$rev_detrend<-temp_df$value_usd-pred_rev
  temp_df$per_detrend<-temp_df$rev_per_fisher-pred_per
  temp_df$per_mt_detrend<-temp_df$mt_per_fisher-pred_per_mt
  
  store_df<-rbind(store_df,temp_df)
  
}

cali_catch<-store_df

save(cali_catch,file=here::here("data","fisheries","cali_catch_detrend.rda"))

### Repeat for the port data

load(here::here("data","fisheries","cali_port.rda"))

store_df<-data.frame(spp_code=character(),
                     port_code=character(),
                     year=numeric(),
                     revenues_usd=numeric(),
                     landings_mt=numeric(),
                     price_usd_lb=numeric(),
                     port_spp_id=character(),
                     cdfw_name=character(),
                     port_area=character(),
                     comm_name=character(),
                     landings_lb=numeric(),
                     n_fisher=numeric(),
                     rev_per_fisher=numeric(),
                     mt_per_fisher=numeric(),
                     lb_per_fisher=numeric(),
                     mt_detrend=numeric(),
                     rev_detrend=numeric(),
                     per_detrend=numeric(),
                     per_mt_detrend=numeric())

j_vec<-unique(cali_port_catch$port_spp_id)

for(j in 1:length(j_vec)){
  temp_df<-cali_port_catch |> 
    filter(port_spp_id==j_vec[j] & year>=1985) |> 
    mutate(rev_per_fisher=case_when(n_fisher==0~0,
                                    TRUE~rev_per_fisher),
           mt_per_fisher=case_when(n_fisher==0~0,
                                   TRUE~mt_per_fisher),
           lb_per_fisher=case_when(n_fisher==0~0,
                                   TRUE~lb_per_fisher))
  
  print(unique(temp_df$port_spp_id))
  
  # if(unique(temp_df$port_spp_id)=="SablefishBGA"){
  #   browser()
  # }
  # 
  md_mt=rlm(landings_mt~year,data=temp_df,method="MM")
  md_rev=rlm(revenues_usd~year,data=temp_df,method="MM")
  md_per=rlm(rev_per_fisher~year,data=temp_df,method="MM")
  md_per_mt=rlm(mt_per_fisher~year,data=temp_df,method="MM")
  
  pred_mt<-predict(md_mt)
  pred_rev<-predict(md_rev)
  pred_per<-predict(md_per)
  pred_per_mt<-predict(md_per_mt)
  
  temp_df$mt_detrend<-temp_df$landings_mt-pred_mt
  temp_df$rev_detrend<-temp_df$revenues_usd-pred_rev
  temp_df$per_detrend<-temp_df$rev_per_fisher-pred_per
  temp_df$per_mt_detrend<-temp_df$mt_per_fisher-pred_per_mt
  
  store_df<-rbind(store_df,temp_df)
  
}
     
cali_port_catch<-store_df

save(cali_port_catch,file=here::here("data","fisheries","cali_port_detrend.rda"))

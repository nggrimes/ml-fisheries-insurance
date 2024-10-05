## Function used in purrr wrap to combine california state level catch with block catches and weight the environmental variables according to their contribution


cw_join_cali<-function(spp,data){
  
  #browser()

name<-tolower(spp)
catch_data<-readxl::read_xlsx(here::here("data","blocks","state",paste0(name,".xlsx"))) %>% 
  janitor::clean_names() %>% 
  drop_na(total_pounds) %>% 
  filter(total_pounds>0) %>% 
  filter(block_id!="Total") %>% 
  mutate(block_id=as.numeric(block_id))

## construct sf blocks for intersection
block_data<-catch_data %>% 
  inner_join(blocks,by="block_id") %>% 
  mutate(pct_catch=total_pounds/sum(total_pounds)) %>% 
  st_drop_geometry() %>% 
  select(block_id,pct_catch)

join_sst<-block_data %>% 
  inner_join(block_sst,by="block_id") %>%
  mutate(weighted_val=value*pct_catch) %>% 
  group_by(year,var) %>% 
  summarize(value=sum(weighted_val)) %>% 
  filter(year>=1988)

join_beuti<-block_data %>% 
  inner_join(block_beuti,by="block_id") %>%
  mutate(weighted_val=value*pct_catch) %>% 
  group_by(year,var) %>% 
  summarize(value=sum(weighted_val))

join_cuti<-block_data %>%
  inner_join(block_cuti,by="block_id") %>%
  mutate(weighted_val=value*pct_catch) %>% 
  group_by(year,var) %>% 
  summarize(value=sum(weighted_val))

join_hci<-block_data %>%
  inner_join(block_hci,by="block_id") %>%
  mutate(weighted_val=value*pct_catch) %>% 
  group_by(year,var) %>% 
  summarize(value=sum(weighted_val))

join_enso<-enso %>% 
  group_by(year) %>% 
  summarize(value=mean(enso),
            var="enso") %>% 
  mutate(year=as.numeric(year)) %>% 
  filter(year>=1988)

join_pdo<-pdo %>% 
  filter(year>=1988) %>% 
  group_by(year) %>% 
  summarize(value=mean(pdo),
            var="pdo")


env<-rbind(join_sst,join_beuti,join_cuti,join_hci,join_enso,join_pdo)

hold<-data %>% 
  ungroup() %>% 
  select(year,landings_mt,value_usd,rev_per_fisher,mt_per_fisher) %>%
  pivot_longer(-year,names_to="fish_var",values_to="fish_value") %>%
  right_join(env,by="year") %>%
  drop_na(fish_var)

return(hold)

}

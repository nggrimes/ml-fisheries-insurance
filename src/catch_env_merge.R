### Merge fishery data with environmental data

# Load libraries
library(tidyverse)

load(here::here("data","top_species_rev.RData"))
load(here::here("data","collapse_75.rda"))  
load(here::here("data","environmental","env_ts.rda"))

ca_complete<-ca_collapse_75 |> 
  full_join(ca_amp,by="year") |> 
  full_join(pdo_ts,by="year") |> 
  full_join(enso_ts,by="year")  |> 
  full_join(ca_year_temp,by="year") |> 
  drop_na(species_code)

# Repeat for oregon
or_complete<-or_collapse_75 |> 
  full_join(or_amp,by="year") |> 
  full_join(pdo_ts,by="year") |> 
  full_join(enso_ts,by="year")  |> 
  full_join(or_year_temp,by="year") |> 
  drop_na(species_code)

# repeat for washington
wa_complete<-wa_collapse_75 |> 
  full_join(wa_amp,by="year") |> 
  full_join(pdo_ts,by="year") |> 
  full_join(enso_ts,by="year")  |> 
  full_join(wa_year_temp,by="year") |> 
  drop_na(species_code)


# Find the correlations

ca_cor<-ca_complete |> 
  group_by(species_code) |> 
  summarize(enso_catch=cor(enso,landings_mt,use="complete.obs"),
            pdo_catch=cor(pdo,landings_mt,use="complete.obs"),
            amp_catch=cor(amp_sst,landings_mt,use="complete.obs"),
            temp_catch=cor(sst,landings_mt,use="complete.obs"),
            wind_catch=cor(amp_wspd,landings_mt,use="complete.obs"),
            pdo=cor(pdo,value_usd,use="complete.obs"),
            enso=cor(enso,value_usd,use="complete.obs"),
            amp=cor(amp_sst,value_usd,use="complete.obs"),
            wind=cor(amp_wspd,value_usd,use="complete.obs"),
            temp=cor(sst,value_usd,use="complete.obs"),
            mgmt=mgmt_group)|> 
  pivot_longer(!c(species_code,mgmt),names_to="var",values_to="correlation")

# repeat with washington
wa_cor<-wa_complete |> 
  group_by(species_code) |> 
  summarize(enso_catch=cor(enso,landings_mt,use="complete.obs"),
            pdo_catch=cor(pdo,landings_mt,use="complete.obs"),
            amp_catch=cor(amp_sst,landings_mt,use="complete.obs"),
            temp_catch=cor(sst,landings_mt,use="complete.obs"),
            wind_catch=cor(amp_wspd,landings_mt,use="complete.obs"),
            pdo=cor(pdo,value_usd,use="complete.obs"),
            enso=cor(enso,value_usd,use="complete.obs"),
            amp=cor(amp_sst,value_usd,use="complete.obs"),
            wind=cor(amp_wspd,value_usd,use="complete.obs"),
            temp=cor(sst,value_usd,use="complete.obs"),
            mgmt=mgmt_group)|> 
  pivot_longer(!c(species_code,mgmt),names_to="var",values_to="correlation")

# repeat with oregon
or_cor<-or_complete |> 
  group_by(species_code) |> 
  summarize(enso_catch=cor(enso,landings_mt,use="complete.obs"),
            pdo_catch=cor(pdo,landings_mt,use="complete.obs"),
            amp_catch=cor(amp_sst,landings_mt,use="complete.obs"),
            temp_catch=cor(sst,landings_mt,use="complete.obs"),
            wind_catch=cor(amp_wspd,landings_mt,use="complete.obs"),
            pdo=cor(pdo,value_usd,use="complete.obs"),
            enso=cor(enso,value_usd,use="complete.obs"),
            amp=cor(amp_sst,value_usd,use="complete.obs"),
            wind=cor(amp_wspd,value_usd,use="complete.obs"),
            temp=cor(sst,value_usd,use="complete.obs"),
            mgmt=mgmt_group)|> 
  pivot_longer(!c(species_code,mgmt),names_to="var",values_to="correlation")


# create a bar graph of the correlations for each variable with facet_warp with 2 columns
ca_cor |> 
  ggplot(aes(x=species_code,y=correlation,fill=correlation>0))+
  geom_col(stat="identity")+
  facet_wrap(~var)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# make a heatmap of the correlations for each species on the y-axis
ca_heat<-ca_cor |> 
  ggplot(aes(x=var,y=species_code,fill=correlation))+
  geom_tile()+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_fill_gradient2(low="blue",mid="white",high="red",midpoint=0,limits=c(-0.61,0.61),breaks=seq(-0.6,0.6,by=.2),labels=round(seq(-0.6,0.6,by=.2),1))+
  facet_wrap(~mgmt,scales="free_y")+
  theme(axis.text.y=element_blank())+
  labs(y="",x="Environmental Variable",title="Correlation of Environmental Variables with Catch and Value by Species in California")

# repeat for washington
wa_heat<-wa_cor |> 
  ggplot(aes(x=var,y=species_code,fill=correlation))+
  geom_tile()+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_fill_gradient2(low="blue",mid="white",high="red",midpoint=0,limits=c(-0.61,0.61),breaks=seq(-0.6,0.6,by=.2),labels=round(seq(-0.6,0.6,by=.2),1))+
  facet_wrap(~mgmt,scales="free_y")+
  #theme(axis.text.y=element_blank())+
  labs(y="",x="Environmental Variable",title="Correlation of Environmental Variables with Catch and Value by Species in Washington")

# repeat for oregon
or_heat<-or_cor |> 
  ggplot(aes(x=var,y=species_code,fill=correlation))+
  geom_tile()+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_fill_gradient2(low="blue",mid="white",high="red",midpoint=0,limits=c(-0.61,0.61),breaks=seq(-0.6,0.6,by=.2),labels=round(seq(-0.6,0.6,by=.2),1))+
  facet_wrap(~mgmt,scales="free_y")+
  theme(axis.text.y=element_blank())+
  labs(y="",x="Environmental Variable",title="Correlation of Environmental Variables with Catch and Value by Species in Oregon")


save(ca_complete,or_complete,wa_complete,file=here::here("data","complete_data.rda"))

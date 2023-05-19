library(wcfish)
library(tidyverse)

west_port<-pacfin_ports %>% 
  dplyr::select(state2,port_yn,port_name,long_dd,lat_dd,port_code) %>% 
  filter(state2 %in% c("CA","WA","OR"),
         port_yn=="yes")

rm_p<-c("CA2","WA5","CLO","CLW")

p_name=c("BDG","BRG","BRK","COS","CRS","WPT","ERK","LGB","MNT","MRO","NEW","SEA","SB","SD","SF","TAC","TLL")

pac_sub<-pacfin_all5 %>% 
  filter(year>2010 & revenues_usd>0) %>% 
  group_by(port_code) %>% 
  summarize(total=mean(revenues_usd)) %>% 
  filter(!port_code %in% rm_p) %>% 
  mutate(match_code=p_name) %>%
  inner_join(west_port,by=c("match_code"="port_code")) %>% 
  mutate(port_code=fct_reorder(port_code,lat_dd))




a<-st_as_sf(pac_sub,coords = c("long_dd","lat_dd"),crs = 4326)

pc_total_land<-ggplot()+
  geom_tile(data=sst_filter,aes(x=longitude,y=latitude,fill = temp))+
  scale_fill_gradientn(colours=ocean.thermal(20),guide = "colourbar")

pc_total_land<- ggplot()+
  geom_sf(data=state_sf)+
  geom_sf(data=wash_sf,fill=NA,color="black")+
  geom_sf(data=port_complex_sf,fill=NA,color="black")+
  geom_sf(data=oreg_sf,fill=NA,color="black")+
  geom_sf(data=a,aes(color=port_code,size=total))+
  geom_sf_text(data=a,aes(label=port_name),nudge_x = 2.4,nudge_y = 0,size=3.5)+
  coord_sf(xlim = c(-129,-112),ylim=c(30,49))+
  scale_y_continuous(expand=c(0,0))+
  theme_classic()+
  labs(x = NULL, y = NULL,size="Metric Tons (1000s mt)") +
  theme(legend.position = "bottom")+
  guides(colour="none")+
  scale_color_viridis_d()+
  ggtitle("Average Annual Catch by Port Complex 2010-2021")

pc_total_land

ggsave(here::here("documents","presentations","img","pc_total.png"),pc_total_land)

pc_grids<-ggplot()+
  geom_sf(data=state_sf)+
  geom_sf(data=blocks,fill=NA,color='grey')+
  geom_sf(data=eez_wc,fill=NA,size=10,color="black")+
  geom_sf(data=a,aes(color=port_code,size=total))+
  geom_sf_text(data=a,aes(label=port_name),nudge_x = 2.4,nudge_y = 0,size=3.5)+
  coord_sf(xlim = c(-129,-112),ylim=c(30,49))+
  scale_y_continuous(expand=c(0,0))+
  theme_classic()+
  labs(x = NULL, y = NULL,size="Metric Tons (1000s mt)") +
  theme(legend.position = "bottom")+
  guides(colour="none")+
  scale_color_viridis_d()+
  ggtitle("Average Annual Catch by Port Complex 2010-2021")



ggsave(here::here("documents","presentations","img","pc_grid.png"),pc_grids)



pac_mon<-pacfin_all6 %>% 
  filter(year==2019 & value_usd>0,state !="At-Sea") %>% 
  group_by(state,month_num) %>% 
  summarize(total=sum(value_usd/1000000)) %>% 
  mutate(month=month.abb[month_num])

p_mon<-ggplot(data=pac_mon,aes(x=state,y=total,fill=state))+
  geom_col()+
  facet_wrap(~fct_reorder(month,month_num))+
  labs(x="",y="Monthly Revenue in 2019  ($ millions)\n",fill="")+
  theme_classic()+
  theme(axis.text.x = element_blank())+
  scale_fill_manual(values=c("#003660","#79A540",'#047C91'))

ggsave(here::here("documents","presentations","img","p_mon.png"),p_mon)


pac_ann_map<-pacfin_all5 %>% 
  filter(year==2019 & revenues_usd>0) %>% 
  group_by(state) %>% 
  summarize(total=sum(revenues_usd)) %>% 
  mutate(match_code=c("SF","NEW","WPT")) %>%
  inner_join(west_port,by=c("match_code"="port_code"))

map_sf<-st_as_sf(pac_ann_map,coords = c("long_dd","lat_dd"),crs = 4326)

st_total_land<- ggplot()+
  geom_sf(data=state_sf)+
  geom_sf(data=wash_sf,fill=NA,color="black")+
  geom_sf(data=oreg_sf,fill=NA,color="black")+
  geom_sf(data=cali_sf,fill=NA,color="black")+
  geom_sf(data=map_sf,aes(color=state,size=total/1000000))+
  geom_sf_text(data=map_sf,aes(label=state),nudge_x = 2.8,nudge_y = 0,size=3.5)+
  coord_sf(xlim = c(-129,-112),ylim=c(30,49))+
  scale_y_continuous(expand=c(0,0))+
  theme_classic()+
  labs(x = NULL, y = NULL,size="Revenue ($ million)") +
  guides(colour="none")+
  scale_color_viridis_d()+
  scale_size_continuous(range = c(8, 12))+
  ggtitle("Total Landings Revenue by State in 2019")

st_total_land

ggsave(here::here("documents","presentations","img","p_ann.png"),st_total_land)

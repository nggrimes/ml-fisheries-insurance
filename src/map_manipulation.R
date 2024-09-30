library(tidyverse)
library(rerddap)
library(sf)
library(wcfish)
library(raster)
library(rnaturalearth)
library(pals)
library(ncdf4)

## Code for reducing EEZ shp to just wc. Run once and saved now as eez_wc.shp
#eez<-st_read(here::here("data","sf_borders","eez_v11.shp")) %>% 
  #janitor::clean_names() %>% 
  #filter(territory1=="United States" & mrgid == 8456)

#wc_sq<-rbind(c(-130,30),c(-130,48.5),c(-116,48.5),c(-116,30),c(-130,30)) %>% list %>% st_polygon %>% st_sfc

#st_crs(wc_sq)<-4326

#eez_wc<-st_intersection(wc_sq,eez)

#write_sf(eez_wc,here::here("data","sf_borders","eez_wc.shp"))


eez_wc<-st_read(here::here("data","sf_borders","eez_wc.shp"))

eez_bound<-st_read(here::here("data","sf_borders","eez_boundaries_v11.shp")) %>% 
  janitor::clean_names() %>% 
  filter(line_id==2050)

### State borders
state<-ne_states(country='United States of America')
  
state_sf<-st_as_sf(state) %>% 
  filter(name %in% c('California','Oregon','Washington','Nevada'))

state_sf<-state_sf[,c(9,84)]


## SST Data  All envirodata comes from erddap
file_id<-rerddap::info(datasetid = "NOAA_DHW_monthly", url = "https://coastwatch.pfeg.noaa.gov/erddap/")

start="2021-01-01"

end="2023-12-31"

sst<-griddap(datasetx=file_id,
             time=c(start,end),
             latitude=c(30,42),
             longitude=c(-115,-130))$data %>% 
             mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>% 
             rename(t=time,temp=sea_surface_temperature)

sst_filter<-sst %>% 
  filter(t == "2018-12-16") %>% 
  drop_na()

# Fishing blocks graph

p<-ggplot()+
  geom_tile(data=sst_filter,aes(x=longitude,y=latitude,fill = temp))+
  scale_fill_gradientn(colours=ocean.thermal(20),guide = "colourbar")
p+geom_sf(data=state_sf)+
  geom_sf(data=blocks,fill=NA,color='white')+
  geom_sf(data=eez_wc,fill=NA,size=10,color="black")+
  coord_sf(xlim = c(-129,-117),ylim=c(30,49))+
  scale_y_continuous(expand=c(0,0))+
  theme_classic()+
  labs(x = NULL, y = NULL, fill = "SST (°C)") +
  theme(legend.position = c(0.88, 0.65),
        legend.box.background = element_rect(color="#047C91", linewidth =1),
        axis.text.x = element_text(angle = 45,vjust=0.5,colour = "#047C91",family="Segoe UI Semilight"),
        title = element_text(size=28,colour = "#047C91",family="Segoe UI Semilight"),
        axis.text=element_text(size=23,colour = "#047C91",family="Segoe UI Semilight"),
        legend.title = element_text(size=22,colour = "#047C91",family="Segoe UI Semilight"),
        legend.text = element_text(size=18,colour = "#047C91",family="Segoe UI Semilight"))
  ggsave(here::here("documents","presentations","img","p_temp_grid.png"),p)

# State Lines
  cali_sq<-rbind(c(-130,30),c(-130,41.79329),c(-116,41.79329),c(-116,30),c(-130,30)) %>% list %>% st_polygon %>% st_sfc

  st_crs(cali_sq)<-4326
  
  cali_sf<-st_intersection(cali_sq,eez_wc)
  
  st_bbox(cali_sf)
  
  oreg_sq<-rbind(c(-130,41.79329),c(-130,46.049),c(-116,46.049),c(-116,41.79329),c(-130,41.79329)) %>% list %>% st_polygon %>% st_sfc
  
  st_crs(oreg_sq)<-4326
  
  oreg_sf<-st_intersection(oreg_sq,eez_wc)
  
  st_bbox(oreg_sf)
  
  wash_sq<-rbind(c(-130,46.049),c(-130,48.5),c(-116,48.5),c(-116,46.049),c(-130,46.049)) %>% list %>% st_polygon %>% st_sfc
  
  st_crs(wash_sq)<-4326
  
  wash_sf<-st_intersection(wash_sq,eez_wc)
  
  
state_sst_graph<-ggplot()+
  geom_tile(data=sst_filter,aes(x=longitude,y=latitude,fill = temp))+
  scale_fill_gradientn(colours=ocean.thermal(20),guide = "colourbar")
  
state_sst_graph+geom_sf(data=state_sf)+
  geom_sf(data=wash_sf,fill=NA,color="black")+
  geom_sf(data=cali_sf,fill=NA,color="black")+
  geom_sf(data=oreg_sf,fill=NA,color="black")+
  coord_sf(xlim = c(-129,-117),ylim=c(30,49))+
  scale_y_continuous(expand=c(0,0))+
  theme_classic()+
  labs(x = NULL, y = NULL, fill = "SST (°C)") +
  theme(legend.position = "bottom")+
  ggtitle("Month Averaged SST for 12/18")
  

# Port Complex Lines  

port_complex<-pacfin_ports %>% 
  drop_na() %>% 
  filter(state2 %in% c("CA",'OR','WA'))

sd_sq<-rbind(c(-130,30),c(-130,33.199),c(-116,33.199),c(-116,30),c(-130,30)) %>% list %>% st_polygon %>% st_sfc

st_crs(sd_sq)<-4326

sd_sf<-st_intersection(sd_sq,eez_wc)%>% 
  st_sf %>% 
  mutate(port_complex="San Diego",
         state="California")

la_sq<-rbind(c(-130,33.199),c(-130,33.861),c(-116,33.861),c(-116,33.199),c(-130,33.199)) %>% list %>% st_polygon %>% st_sfc

st_crs(la_sq)<-4326

la_sf<-st_intersection(la_sq,eez_wc)%>% 
  st_sf %>% 
  mutate(port_complex="Los Angeles",
         state="California")


sb_sq<-rbind(c(-130,33.861),c(-130,34.797),c(-116,34.797),c(-116,33.861),c(-130,33.861)) %>% list %>% st_polygon %>% st_sfc

st_crs(sb_sq)<-4326

sb_sf<-st_intersection(sb_sq,eez_wc)%>% 
  st_sf %>% 
  mutate(port_complex="Santa Barbara",
         state="California")



mb_sq<-rbind(c(-130,34.797),c(-130,35.604),c(-116,35.604),c(-116,34.797),c(-130,34.797)) %>% list %>% st_polygon %>% st_sfc

st_crs(mb_sq)<-4326

mb_sf<-st_intersection(mb_sq,eez_wc)%>% 
  st_sf %>% 
  mutate(port_complex="Morro Bay",
         state="California")



mont_sq<-rbind(c(-130,35.604),c(-130,36.6505),c(-116,36.6505),c(-116,35.604),c(-130,35.604)) %>% list %>% st_polygon %>% st_sfc

st_crs(mont_sq)<-4326

mont_sf<-st_intersection(mont_sq,eez_wc)%>% 
  st_sf %>% 
  mutate(port_complex="Monterey",
         state="California")



sanf_sq<-rbind(c(-130,36.6505),c(-130,37.6067),c(-116,37.6067),c(-116,36.6505),c(-130,36.6505)) %>% list %>% st_polygon %>% st_sfc

st_crs(sanf_sq)<-4326

sanf_sf<-st_intersection(sanf_sq,eez_wc)%>% 
  st_sf %>% 
  mutate(port_complex="San Francisco",
         state="California")



bb_sq<-rbind(c(-130,37.6067),c(-130,38.561),c(-116,38.561),c(-116,37.6067),c(-130,37.6067)) %>% list %>% st_polygon %>% st_sfc

st_crs(bb_sq)<-4326

bb_sf<-st_intersection(bb_sq,eez_wc)%>% 
  st_sf %>% 
  mutate(port_complex="Bodega Bay",
         state="California")


fb_sq<-rbind(c(-130,38.561),c(-130,39.795),c(-116,39.795),c(-116,38.561),c(-130,38.561)) %>% list %>% st_polygon %>% st_sfc

st_crs(fb_sq)<-4326

fb_sf<-st_intersection(fb_sq,eez_wc)%>% 
  st_sf %>% 
  mutate(port_complex="Fort Bragg",
         state="California")


eur_sq<-rbind(c(-130,39.795),c(-130,41.2),c(-116,41.2),c(-116,39.795),c(-130,39.795)) %>% list %>% st_polygon %>% st_sfc

st_crs(eur_sq)<-4326

eur_sf<-st_intersection(eur_sq,eez_wc) %>% 
  st_sf %>% 
  mutate(port_complex="Eureka",
         state="California")

cc_sq<-rbind(c(-130,41.2),c(-130,41.79329),c(-116,41.79329),c(-116,41.2),c(-130,41.2)) %>% list %>% st_polygon %>% st_sfc

st_crs(cc_sq)<-4326

cc_sf<-st_intersection(cc_sq,eez_wc) %>% 
  st_sf %>% 
  mutate(port_complex="Crescent City",
         state="California")

br_sq<-rbind(c(-130,41.79329),c(-130,42.955),c(-116,42.955),c(-116,41.79329),c(-130,41.79329)) %>% list %>% st_polygon %>% st_sfc

st_crs(br_sq)<-4326

br_sf<-st_intersection(br_sq,eez_wc) %>% 
  st_sf %>% 
  mutate(port_complex="Brookings",
         state="Oregon")

coo_sq<-rbind(c(-130,42.955),c(-130,43.611),c(-116,43.611),c(-116,42.955),c(-130,42.955)) %>% list %>% st_polygon %>% st_sfc

st_crs(coo_sq)<-4326

coo_sf<-st_intersection(coo_sq,eez_wc) %>% 
  st_sf %>% 
  mutate(port_complex="Coos Bay",
         state="Oregon")

new_sq<-rbind(c(-130,43.611),c(-130,45.044),c(-116,45.044),c(-116,43.611),c(-130,43.611)) %>% list %>% st_polygon %>% st_sfc

st_crs(new_sq)<-4326

new_sf<-st_intersection(new_sq,eez_wc) %>% 
  st_sf %>% 
  mutate(port_complex="Newport",
         state="Oregon")



port_complex_sf<-rbind(new_sf,coo_sf,br_sf,cc_sf,eur_sf,fb_sf,sb_sf,bb_sf,sanf_sf,mont_sf,mb_sf,la_sf,sd_sf)

ggplot()+
  geom_sf(data=port_complex_sf,aes(fill=port_complex))


### Radius of port complex

##### Chlorophyll  ####
file_id<-rerddap::info(datasetid = "erdMH1chlamday", url = "https://coastwatch.pfeg.noaa.gov/erddap/")

start="2018-01-01"

end="2018-12-31"

chlor<-griddap(datasetx=file_id,
              time=c(as.Date(start),as.Date(end)),
              latitude=c(30,49.2),
              longitude=c(-115,-130))$data %>% 
  mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>% 
  rename(t=time,chlor=chlorophyll)

chlor_filter<-chlor %>% 
  filter(t == "2018-05-16") %>% 
  drop_na()

state_chlor_graph<-ggplot()+
  geom_tile(data=chlor_filter,aes(x=longitude,y=latitude,fill = chlor))+
  scale_fill_gradientn(colours=ocean.haline(100),guide = "colourbar")+
  geom_sf(data=state_sf)+
  geom_sf(data=wash_sf,fill=NA,color="black")+
  geom_sf(data=cali_sf,fill=NA,color="black")+
  geom_sf(data=oreg_sf,fill=NA,color="black")+
  coord_sf(xlim = c(-129,-117),ylim=c(30,49))+
  scale_y_continuous(expand=c(0,0))+
  theme_classic()+
  labs(x = NULL, y = NULL, fill = "Concentration (mg m^-3)") +
  #theme(legend.position = "bottom")+
  ggtitle("Monthly Chlorophyll Concentration on May 16th 2018")

state_chlor_graph


ggsave(here::here("documents","presentations","img","p_chl.png"),state_chlor_graph)


### wind

file_id<-rerddap::info(datasetid = "nceiPH53sstd1day", url = "https://coastwatch.pfeg.noaa.gov/erddap/")

start="2020-12-01"

end="2020-12-31"

wind<-griddap(datasetx=file_id,
             time=c(start,end),
             latitude=c(30,49.2),
             longitude=c(-115,-130))$data %>% 
  mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>% 
  rename(t=time,wind=wind_speed,temp=sea_surface_temperature) %>% 
  dplyr::select(t,wind,temp,latitude,longitude)%>% 
  drop_na()

wind_filter<-wind %>% 
  filter(t == "2020-12-01") %>% 
  drop_na()

state_wind_graph<-ggplot()+
  geom_tile(data=wind_filter,aes(x=longitude,y=latitude,fill = wind))+
  scale_fill_gradientn(colours=ocean.speed(20),guide = "colourbar")+
  geom_sf(data=state_sf)+
  geom_sf(data=wash_sf,fill=NA,color="black")+
  geom_sf(data=cali_sf,fill=NA,color="black")+
  geom_sf(data=oreg_sf,fill=NA,color="black")+
  coord_sf(xlim = c(-129,-117),ylim=c(30,49))+
  scale_y_continuous(expand=c(0,0))+
  theme_classic()+
  labs(x = NULL, y = NULL, fill = "Wind speed (m/s)") +
  #theme(legend.position = "bottom")+
  ggtitle("Average Wind Speed of 12/01/20")

ggsave(here::here("documents","presentations","img","p_wind.png"),state_wind_graph)

wind_sf<-st_as_sf(wind,coords=c('longitude','latitude'),crs=4326)

sst_sf<-st_as_sf(sst,coords=c('longitude','latitude'),crs=4326)

temp_in<-st_intersection(sst_sf,cali_sf) %>% 
  mutate(lon=st_coordinates(.)[,1],
         lat=st_coordinates(.)[,2]) %>% 
  st_drop_geometry()

temp_filter<-temp_in %>% 
  filter(t == "2018-12-16") %>% 
  drop_na()

state_temp_graph<-ggplot()+
  geom_point(data=temp_filter,aes(x=lon,y=lat,color = temp))+
  scale_color_gradientn(colours=ocean.thermal(20),guide = "colourbar")



#### Practice fishbase import

## Manual data import


cod<-nc_open(here::here("data","Gadus_macrocephalus.nc"))

lat=ncvar_get(cod,"latitude")
lon=ncvar_get(cod,"longitude")
prop=ncvar_get(cod,"probability")

lonlat<-as.matrix(expand.grid(lon,lat))
prop_vec<-as.vector(prop)
prop_df<-data.frame(cbind(lonlat,prop_vec)) %>% 
  rename(lon=Var1,lat=Var2,prop=prop_vec) %>% 
  drop_na() %>% 
  filter(prop>0.5) %>% 
  filter(lat<48) %>% 
  filter(lon< -100)

prop_tran=prop_df

coordinates(prop_tran)=~lon+lat
proj4string(prop_tran)=CRS("+init=epsg:4326")
grid=points2grid(prop_tran)

grid=as.data.frame(SpatialGrid(grid)) 
coordinates(grid)=~lon+lat

grid=rasterFromXYZ(grid)
grid=rasterToPolygons(grid)
grid=st_as_sf(grid)
st_crs(grid)=4326

for(i in 1:nrow(prop_df)){
  lat_v=prop_df$lat[i]
  lon_v=prop_df$lon[i]
  
  pnt=st_sfc(st_point(c(lon_v,lat_v)),crs=4326)
  id=prop_df$prop[i]
  
  
  prop_df$grid_loc[i]=as.numeric(st_intersects(pnt,grid))
  
}

grid=grid[prop_df$grid_loc,]
grid$layer=prop_df$prop

p<-ggplot()+
  geom_tile(data=sst_filter,aes(x=longitude,y=latitude,fill = temp))+
  scale_fill_gradientn(colours=ocean.thermal(20),guide = "colourbar")

p_cod<-ggplot()+
  geom_sf(data=state_sf)+
  geom_sf(data=blocks,fill=NA,color='grey')+
  geom_sf(data=eez_wc,fill=NA,size=10,color="black")+
  geom_tile(data=prop_df,aes(x=lon,y=lat,fill=prop),alpha=0.75)+
  #geom_sf(aes(fill=layer),data=grid,alpha=0.5)+
  coord_sf(xlim = c(-129,-117),ylim=c(30,49))+
  scale_y_continuous(expand=c(0,0))+
  theme_classic()+
  labs(x = NULL, y = NULL, fill = "Prob of\nhabitat") +
  #theme(legend.position = "bottom")+
  ggtitle("Distribution of Pacific cod")

p_cod

ggsave(here::here("documents","presentations","img","p_cod.png"),p_cod)


# tuna
tuna<-nc_open(here::here("data","Thunnus_alalunga.nc"))

lat=ncvar_get(tuna,"latitude")
lon=ncvar_get(tuna,"longitude")
prop=ncvar_get(tuna,"probability")

lonlat<-as.matrix(expand.grid(lon,lat))
prop_vec<-as.vector(prop)
tuna_prop_df<-data.frame(cbind(lonlat,prop_vec)) %>% 
  rename(lon=Var1,lat=Var2,prop=prop_vec) %>% 
  drop_na() %>% 
  filter(prop>.5) %>% 
  filter(lat<48) %>% 
  filter(lon< -100) %>% 
  filter(lon> -130) %>% 
  filter(lat>30)

prop_tran=tuna_prop_df

coordinates(prop_tran)=~lon+lat
proj4string(prop_tran)=CRS("+init=epsg:4326")
tuna_grid=points2grid(prop_tran)

tuna_grid=as.data.frame(SpatialGrid(tuna_grid)) 
coordinates(tuna_grid)=~lon+lat

tuna_grid=rasterFromXYZ(tuna_grid)
tuna_grid=rasterToPolygons(tuna_grid)
tuna_grid=st_as_sf(tuna_grid)
st_crs(tuna_grid)=4326

for(i in 1:nrow(tuna_prop_df)){
  lat_v=tuna_prop_df$lat[i]
  lon_v=tuna_prop_df$lon[i]
  
  pnt=st_sfc(st_point(c(lon_v,lat_v)),crs=4326)
  id=tuna_prop_df$prop[i]
  
  
  tuna_prop_df$grid_loc[i]=as.numeric(st_intersects(pnt,tuna_grid))
  
}

tuna_grid=tuna_grid[tuna_prop_df$grid_loc,]
tuna_grid$layer=tuna_prop_df$prop

p<-ggplot()+
  geom_tile(data=sst_filter,aes(x=longitude,y=latitude,fill = temp))+
  scale_fill_gradientn(colours=ocean.thermal(20),guide = "colourbar")

p_tuna<-ggplot()+
  geom_sf(data=state_sf)+
  geom_sf(data=blocks,fill=NA,color='grey')+
  geom_sf(data=eez_wc,fill=NA,size=10,color="black")+
  geom_tile(data=tuna_prop_df,aes(x=lon,y=lat,fill=prop),alpha=0.5)+
  #geom_sf(data=tuna_grid,aes(fill=layer),alpha=0.5)+
  coord_sf(xlim = c(-129,-117),ylim=c(30,49))+
  scale_y_continuous(expand=c(0,0))+
  theme_classic()+
  labs(x = NULL, y = NULL, fill = "Prob of \nhabitat") +
  #theme(legend.position = "bottom")+
  ggtitle("Distribution of Albarcore Tuna")

p_tuna

ggsave(here::here("documents","presentations","img","p_tuna.png"),p_tuna)

 
#st_intersects is faster if checking points exist and I don't need a layer.

wind_tuna<-wind_sf[st_intersects(wind_sf,tuna_cal$geometry) %>% lengths>0,] %>% 
  st_drop_geometry() %>% 
  group_by(t) %>% 
  summarize(mean_temp=mean(temp), #get enviro at each at each day
            mean_wind=mean(wind)) %>% 
  rename(date=t)

sp_id<-spec$spp_code[which(spec$sci_name=="Thunnus alalunga")]

tuna_fish<-pacfin_all6 %>% 
  filter(spp_code==sp_id & state=="California") %>% 
  dplyr::select(spp_code,state,date,landings_mt,value_usd) %>% 
  inner_join(wind_tuna,by="date")

## Model building 

mdl1<-lm(landings_mt~mean_wind+mean_temp,data=tuna_fish)


### Get all environmental variable in fish map at the state of cali level
temp_in<-st_intersection(sst_sf,tuna_fish) %>% 
  mutate(lon=st_coordinates(.)[,1],
         lat=st_coordinates(.)[,2]) %>% 
  st_drop_geometry()

temp_filter<-temp_in %>% 
  filter(t == "2018-12-16") %>% 
  drop_na()

### top species


top_species<-pacfin_all5 %>% 
  filter(spp_code!="Confidential") %>% 
  group_by(spp_code) %>% 
  summarize(total_rev=sum(revenues_usd)) %>% 
  slice_max(total_rev,n=15)

state_tot_rev<-pacfin_all5 %>% 
  filter(spp_code %in% top_species$spp_code) %>% 
  group_by(state,spp_code,year) %>% 
  summarize(total_rev=sum(revenues_usd))

ca_tot_rev<-state_tot_rev %>% 
  filter(state=="California")

ggplot(ca_tot_rev,aes(x=year,y=total_rev,color=spp_code))+
  geom_line()+
  viridis::scale_color_viridis(discrete =TRUE)+
  theme_classic()

load(here::here("data","sst_noaa_dhw_1985-2020.Rdata"))

sst_sf<-sst %>% 
  st_as_sf(coords=c("longitude","latitude"),crs=4326) %>% 
  st_intersection()

### Alaska crap map

### State borders
state<-ne_states(country='United States of America')

state_sf<-st_as_sf(state) %>% 
  filter(name %in% c('Alaska'))

state_sf<-state_sf[,c(9,84)]


## SST Data  All envirodata comes from erddap
file_id<-rerddap::info(datasetid = "NOAA_DHW_monthly", url = "https://coastwatch.pfeg.noaa.gov/erddap/")

start="2019-01-01"

end="2019-12-31"

ak_sst<-griddap(datasetx=file_id,
             time=c(as.Date(start),as.Date(end)),
             latitude=c(50.5,62.5),
             longitude=c(-153,-179))$data %>% 
  mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>% 
  rename(t=time,temp=sea_surface_temperature)

ak_sst_filter<-ak_sst %>% 
  filter(t == "2019-07-15") %>% 
  drop_na()

crab_ak<-crab %>% janitor::clean_names() %>% 
  filter(survey_year==2018 & common_name=="snow crab") %>% 
  dplyr::select(latitude,longitude,cpue)

crab_sf<-crab_ak %>% 
  st_as_sf(coords = c("longitude","latitude"),crs=4326)

p<-ggplot()+
  geom_tile(data=ak_sst_filter,aes(x=longitude,y=latitude,fill = temp))+
  scale_fill_gradientn(colours=ocean.thermal(20),guide = "colourbar")
p+geom_sf(data=state_sf)+
  geom_sf(data=eez_wc,fill=NA,size=10,color="black")+
  geom_sf(data=crab_sf,aes(size=cpue/1000))+
  coord_sf(xlim = c(-158,-180),ylim=c(50,62))+
  scale_y_continuous(expand=c(0,0))+
  theme_classic()+
  labs(x = NULL, y = NULL, fill = "SST (°C)",size="Distribution of\nCrabs\n(1000 per sq mi)") +
  theme(legend.position = c(0.88, 0.65),
        legend.box.background = element_rect(color="#047C91", linewidth =1),
        axis.text.x = element_text(angle = 45,vjust=0.5,colour = "#047C91",family="Segoe UI Semilight"),
        title = element_text(size=28,colour = "#047C91",family="Segoe UI Semilight"),
        axis.text=element_text(size=23,colour = "#047C91",family="Segoe UI Semilight"),
        legend.title = element_text(size=22,colour = "#047C91",family="Segoe UI Semilight"),
        legend.text = element_text(size=18,colour = "#047C91",family="Segoe UI Semilight"))

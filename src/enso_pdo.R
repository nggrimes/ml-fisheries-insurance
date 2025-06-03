# download pdo and enso data

pdo<-read.table("https://www.ncei.noaa.gov/pub/data/cmb/ersst/v5/index/ersst.v5.pdo.dat",header=TRUE,skip=1) |> 
  pivot_longer(cols=!Year,names_to="month",values_to="pdo") %>%
  mutate(date=paste(month,"1,",Year)%>%mdy ()) |> 
  mutate(pdo=na_if(pdo,99.99)) |> 
  setNames(c("year","month","pdo","date")) %>%
  filter(year>=1979)


enso<-read.table(here::here("data","environmental","meiv2.data"),fill=T,header=FALSE,skip=1,na.strings = "-990.00") |> 
  setNames(c("year",month.abb)) %>%
  slice(1:(nrow(.)-5)) %>%
  gather(key="month",value="enso",2:ncol(.)) %>%
  mutate(date=paste(month,"1,",year)%>%mdy ()) %>%
  arrange(date) %>%
  mutate(enso=as.numeric(enso))%>% 
  mutate(enso=na_if(enso,-999))


oni<-read.table('https://www.cpc.ncep.noaa.gov/data/indices/oni.ascii.txt',header=TRUE) |>
  janitor::clean_names() %>% 
  filter(seas =='JFM') %>% 
  filter(yr>=1980) %>% 
  select(seas,yr,anom)

save(enso,pdo,oni,file=here::here("data","environmental","enso_pdo.rda"))

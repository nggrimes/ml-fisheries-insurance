load(here::here("data","fisheries","cali_catch.rda"))
load(here::here("data","fisheries","cali_port.rda"))


# Test for signifance in trends in catch and port data

cali_mt_trend<-cali_catch |> 
  select(-c(cdfw_name,comm_name,mgmt_group)) |> 
  pivot_longer(cols=-c(species_code,year),names_to="var",values_to="value") |>
  group_by(species_code) |>
  nest() |> 
  mutate(trend=map2(.x='landings_mt',.y=data,~trend_fcn(.x,.y))) |> 
  mutate(mw=map2(.x='landings_mt',.y=data,~mk_fcn(.x,.y)))

cali_rev_trend<-cali_catch |> 
  select(-c(cdfw_name,comm_name,mgmt_group)) |> 
  pivot_longer(cols=-c(species_code,year),names_to="var",values_to="value") |>
  group_by(species_code) |>
  nest() |> 
  mutate(trend=map2(.x='value_usd',.y=data,~trend_fcn(.x,.y))) |> 
  mutate(mw=map2(.x='value_usd',.y=data,~mk_fcn(.x,.y)))

cali_per_trend<- cali_catch |> 
  select(-c(cdfw_name,comm_name,mgmt_group)) |> 
  pivot_longer(cols=-c(species_code,year),names_to="var",values_to="value") |>
  group_by(species_code) |>
  nest() |> 
  mutate(trend=map2(.x='rev_per_fisher',.y=data,~trend_fcn(.x,.y))) |> 
  mutate(mw=map2(.x='rev_per_fisher',.y=data,~mk_fcn(.x,.y)))

cali_trend<-rbind(cali_mt_trend,cali_rev_trend,cali_per_trend) |> 
  ungroup() |> 
  select(-data)


port_mt_trend<-cali_port_catch |> 
  select(-c(cdfw_name,comm_name,port_area,port_spp_id)) |> 
  pivot_longer(cols=-c(spp_code,port_code,year),names_to="var",values_to="value") |>
  group_by(spp_code,port_code) |>
  nest() |> 
  mutate(trend=map2(.x='landings_mt',.y=data,~trend_fcn(.x,.y))) |> 
  mutate(mw=map2(.x='landings_mt',.y=data,~mk_fcn(.x,.y)))

port_rev_trend<-cali_port_catch |> 
  select(-c(cdfw_name,comm_name,port_area,port_spp_id)) |> 
  pivot_longer(cols=-c(spp_code,port_code,year),names_to="var",values_to="value") |>
  group_by(spp_code,port_code) |>
  nest() |> 
  mutate(trend=map2(.x='revenues_usd',.y=data,~trend_fcn(.x,.y))) |> 
  mutate(mw=map2(.x='revenues_usd',.y=data,~mk_fcn(.x,.y)))

port_per_trend<-cali_port_catch |> 
  select(-c(cdfw_name,comm_name,port_area,port_spp_id)) |> 
  pivot_longer(cols=-c(spp_code,port_code,year),names_to="var",values_to="value") |>
  group_by(spp_code,port_code) |>
  nest() |> 
  mutate(trend=map2(.x='rev_per_fisher',.y=data,~trend_fcn(.x,.y))) |> 
  mutate(mw=map2(.x='rev_per_fisher',.y=data,~mk_fcn(.x,.y)))

port_trend<-rbind(port_mt_trend,port_rev_trend,port_per_trend) |> 
  ungroup() |> 
  select(-spp_code,-port_code,-data)

comb<-rbind(port_trend,cali_trend)

trend_fcn<-function(var_name,data){
 #browser()
  temp<-data |> 
    filter(var==var_name & year>1986) |> 
    drop_na()
  
  out<-lm(value~year,data=temp) |> 
    summary()
  
  return(out$coefficients[2,4])
}

mk_fcn<-function(var_name,data){
  temp<-data |> 
    filter(var==var_name & year >1986) |> 
    drop_na()
  
  out<-MannKendall(temp$value)
  
  return(out$sl)
}


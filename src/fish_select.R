# Script to determine consecutive years of data and most important species at the port and state level


library(wcfish)
library(tidyverse)
library(here)


consecutive<-function(x){
  if(all(diff(x)==1)){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

purr_con<-function(df){
  consecutive(df$year)
}

 #### State Level ####

cali_consecutive<-pacfin_all1 %>%
  filter(state=="California") %>%
  group_by(species_code) %>% 
  drop_na(landings_mt) %>%
  nest() %>%
  mutate(consecutive=map_lgl(data, purr_con)) |> 
  filter(consecutive==TRUE) |> 
  unnest(data) |> 
  summarise(n=n_distinct(year),name=unique(comm_name)) |> 
  filter(n>25)

cali_top_list<-cali_consecutive |> 
  full_join(pacfin_all1, by="species_code") |> 
  filter(n>25) |> 
  group_by(species_code) |> 
  filter(year>=2010) |> 
  summarise(sum_rev=mean(value_usd, na.rm=TRUE),name=unique(comm_name)) |> 
  filter(sum_rev>100000) |> 
  drop_na() |> 
  filter(!grepl("Other",name)) |> 
  filter(!grepl("Misc",name)) |>
  filter(!grepl("ockfish",name)) |> 
  filter(!grepl("Unsp",name))

cali_top<-pacfin_all1 |> 
  filter(state=="California") |> 
  filter(species_code %in% cali_top_list$species_code) |> 
  select(species_code,year,mgmt_group,comm_name,value_usd,landings_mt,landings_lb,price_usd_lb) |> 
  mutate(join_index=paste0(comm_name,year))

### State level cdfw data ###

# state level
cdfw_state_catch<-readxl::read_xlsx(here("data","fisheries","ca-1980-2023.xlsx")) |>
  janitor::clean_names() |>
  filter(pounds!="Confidential") |> 
  mutate_at(vars(pounds:value), readr::parse_number)

cdfw_rock<-cdfw_state_catch |> 
  filter(species_name %in% c("Crab, brown rock",
                             "Crab, red rock",
                             "Crab, yellow rock",
                             "Crab, rock unspecified")) |> 
  filter(year>1980) |> 
  group_by(year) |> 
  summarize(species_name="Rock crab",pounds=sum(pounds),value=sum(value),comm_name="Rock crab")

cdfw_match<-cdfw_state_catch |> 
  filter(year>1980) |> 
  filter(species_name %in% c("Anchovy, northern",
                             "Cabezon",
                             "Crab, Dungeness",
                             "Halibut, California",
                             "Lobster, California spiny",
                             "Mackerel, Pacific",
                             "Prawn, ridgeback",
                             "Sablefish",
                             "Salmon, Chinook",
                             "Sardine, Pacific",
                             "Sea urchin, red",
                             "Seabass, white",
                             "Sheephead, California",
                             "Squid, market",
                             "Swordfish",
                             "Tuna, albacore",
                             "Whiting, Pacific",
                             "Prawn, spot",
                             "Tuna, bluefin",
                             "Tuna, yellowfin",
                             "Lingcod",
                             "Opah",
                             "Thornyhead, shortspine",
                             "Bonito, Pacific")) |> 
  mutate(comm_name=case_when(
    species_name=="Anchovy, northern"~"Northern anchovy",
    species_name=="Cabezon"~"Nom. Cabezon",
    species_name=="Crab, Dungeness"~"Dungeness crab",
    species_name=="Halibut, California"~"Nom. Calif halibut",
    species_name=="Lobster, California spiny"~"California spiny lobster",
    species_name=="Mackerel, Pacific"~"Chub mackerel",
    species_name=="Prawn, ridgeback"~"Ridgeback prawn",
    species_name=="Sablefish"~"Sablefish",
    species_name=="Salmon, Chinook"~"Chinook salmon",
    species_name=="Sardine, Pacific"~"Pacific sardine",
    species_name=="Sea urchin, red"~"Red sea urchin",
    species_name=="Seabass, white"~"White seabass",
    species_name=="Sheephead, California"~"Nom. California sheephead",
    species_name=="Squid, market"~"Market squid",
    species_name=="Swordfish"~"Swordfish",
    species_name=="Tuna, albacore"~"Albacore",
    species_name=="Whiting, Pacific"~"Pacific whiting",
    species_name=="Prawn, spot"~"Spotted prawn",
    species_name=="Tuna, bluefin"~"Bluefin tuna",
    species_name=="Tuna, yellowfin"~"Yellowfin tuna",
    species_name=="Lingcod"~"Nom. Lingcod",
    species_name=="Opah"~"Opah",
    species_name=="Thornyhead, shortspine"~"Nom. Shortspine thornyhead",
    species_name=="Bonito, Pacific"~"Pacific bonito"
  )) |> 
  arrange(comm_name)

cdfw_match<-rbind(cdfw_match,cdfw_rock) |> 
  mutate(join_index=paste0(comm_name,year))

# Join the cdfw data with the wcfish data

cali_top_join<-cdfw_match |>
  full_join(cali_top,by=c("comm_name","year")) |>
  arrange(species_code) |>
  mutate(pct_land=(pounds-landings_lb)/landings_lb*100,pct_value=(value-value_usd)/value_usd*100) |>
  group_by(comm_name) |> 
  fill(species_code,mgmt_group,.direction="down") |> 
  arrange(species_code)

# In cali_top_join mutate the value and pounds column to replace the value_usd and landings_lb columns, but only for years 2021,2022, and 2023

cali_top_join<-cali_top_join |>
  mutate(value_usd=case_when(year>2020~value,
                             TRUE~value_usd),
         landings_lb=case_when(year>2020~pounds,
                               TRUE~landings_lb)) |>
  mutate(price_usd_lb=value_usd/landings_lb) |> 
  mutate(landings_mt=landings_lb/2204.62) |> 
  select(-c(pct_land,pct_value,value,pounds,join_index.x,join_index.y)) |> 
  rename(cdfw_name=species_name)


#### Port level ####

# find a list of ports with consecutive years of data
cali_port_consecutive<-pacfin_all5 %>%
  filter(state=="California") %>%
  group_by(spp_code,port_code) %>%
  drop_na(landings_mt) %>%
  nest() %>%
  mutate(consecutive=map_lgl(data, purr_con)) |> 
  filter(consecutive==TRUE) |> 
  unnest(data) |> 
  summarise(n=n_distinct(year)) |> 
  filter(n>25)

# Out of those ports, find the species with an average greater than 50000 dollars of catch in the last decade
cali_port_list<-cali_port_consecutive |> 
  full_join(pacfin_all5, by=c("spp_code","port_code")) |> 
  filter(n>25) |> 
  group_by(spp_code,port_code) |>
  filter(year>=2010 & year<2019) |>
  summarise(sum_rev=mean(revenues_usd, na.rm=TRUE),name=unique(comm_name)) |> 
  filter(sum_rev>75000) |> 
  drop_na() |> 
  filter(!grepl("Other",name)) |> 
  filter(!grepl("Misc",name)) |>
  filter(!grepl("ockfish",name)) |> 
  filter(!grepl("Unsp",name))

cali_top_port<-pacfin_all5 |> 
  filter(state=="California") |> 
  inner_join(cali_port_list,by=c("spp_code","port_code")) |> 
  select(spp_code,port_code,year,comm_name,revenues_usd,landings_mt,price_usd_lb) %>% 
  filter(!(port_code %in% c("CCA","ERA"))) #pull out eurkea to add CCA and ERA together then join

# check eureka, we need to combine era and cca in the pacfin5 for sablefish, dungy, chinook, and albacore
# Need to add the eureka and cca data for each of those fisheries, pps too confidential
era_pacfin<-pacfin_all5 %>% 
  select(!c(port_name,confidential,state,price_usd_lb,sci_name)) %>% 
  filter(port_code %in% c("ERA","CCA") & spp_code %in% c("SABL","CHNK","DCRB","ALBC")) %>% 
  group_by(spp_code,year) %>% 
  pivot_wider(names_from= port_code,
              values_from=c(landings_mt,revenues_usd)) %>% 
  mutate(landings_mt=landings_mt_ERA+landings_mt_CCA,
         revenues_usd=revenues_usd_ERA+revenues_usd_CCA) %>% 
  select(!c(revenues_usd_ERA,revenues_usd_CCA,landings_mt_ERA,landings_mt_CCA)) %>% 
  mutate(price_usd_lb=revenues_usd/landings_mt/2204.62,
         port_code="ERA")

cali_top_port<-rbind(cali_top_port,era_pacfin)

# Port level from cdfw
port_cdfw<-read_csv(here("data","fisheries","port-1980-2023.csv")) |>
  janitor::clean_names() |>
  filter(pounds!="Confidential") |> 
  mutate_at(vars(pounds:value), readr::parse_number)

# aggregate rock crab data at each port and year
cdfw_rock_port<-port_cdfw |> 
  filter(species_name %in% c("Crab, brown rock","Crab, red rock","Crab, yellow rock", "Crab, rock unspecified")) |> 
  filter(year>1980) |> 
  group_by(year,port_area) |> 
  summarize(species_name="Rock crab",pounds=sum(pounds),value=sum(value),comm_name="Rock crab") |> 
  mutate(port_code=case_when(
    port_area=="MONTEREY"~"MNA",
    port_area=="SAN FRANCISCO"~"SFA",
    port_area=="EUREKA"~"ERA",
    port_area=="LOS ANGELES"~"LAA",
    port_area=="SAN DIEGO"~"SDA",
    port_area=="SANTA BARBARA"~"SBA",
    port_area=="MORRO BAY"~"MRA",
    port_area=="FORT BRAGG"~"BGA",
    port_area=="BODEGA BAY"~"BDA"))

cdfw_match_port<-port_cdfw |> 
  filter(year>1980) |> 
  filter(species_name %in% c("Anchovy, northern",
                             "Cabezon",
                             "Crab, Dungeness",
                             "Halibut, California",
                             "Lobster, California spiny",
                             "Mackerel, Pacific",
                             "Prawn, ridgeback",
                             "Sablefish",
                             "Salmon, Chinook",
                             "Sardine, Pacific",
                             "Sea urchin, red",
                             "Seabass, white",
                             "Sheephead, California",
                             "Squid, market",
                             "Swordfish",
                             "Tuna, albacore",
                             "Whiting, Pacific",
                             "Prawn, spot",
                             "Tuna, bluefin",
                             "Tuna, yellowfin",
                             "Lingcod",
                             "Opah",
                             "Thornyhead, shortspine",
                             "Bonito, Pacific",
                             "Shark, thresher",
                             "Shrimp, ocean (pink)")) |> 
  mutate(comm_name=case_when(
    species_name=="Anchovy, northern"~"Northern anchovy",
    species_name=="Cabezon"~"Nom. Cabezon",
    species_name=="Crab, Dungeness"~"Dungeness crab",
    species_name=="Halibut, California"~"Nom. Calif halibut",
    species_name=="Lobster, California spiny"~"California spiny lobster",
    species_name=="Mackerel, Pacific"~"Chub mackerel",
    species_name=="Prawn, ridgeback"~"Ridgeback prawn",
    species_name=="Sablefish"~"Sablefish",
    species_name=="Salmon, Chinook"~"Chinook salmon",
    species_name=="Sardine, Pacific"~"Pacific sardine",
    species_name=="Sea urchin, red"~"Red sea urchin",
    species_name=="Seabass, white"~"White seabass",
    species_name=="Sheephead, California"~"Nom. California sheephead",
    species_name=="Squid, market"~"Market squid",
    species_name=="Swordfish"~"Swordfish",
    species_name=="Tuna, albacore"~"Albacore",
    species_name=="Whiting, Pacific"~"Pacific whiting",
    species_name=="Prawn, spot"~"Spotted prawn",
    species_name=="Tuna, bluefin"~"Bluefin tuna",
    species_name=="Tuna, yellowfin"~"Yellowfin tuna",
    species_name=="Lingcod"~"Nom. Lingcod",
    species_name=="Opah"~"Opah",
    species_name=="Thornyhead, shortspine"~"Nom. Shortspine thornyhead",
    species_name=="Bonito, Pacific"~"Pacific bonito",
    species_name=="Shark, thresher"~"Common thresher shark",
    species_name=="Tuna, skipjack"~"Skipjack tuna",
    species_name=="Shrimp, ocean (pink)"~"Pacific pink shrimp",
    species_name=="Herring, Pacific"~"Pacific herring",
    species_name=="Bonito, Pacific"~"Pacific bonito",
    species_name=="Mackerel, jack"~"Jack mackerel",
    species_name=="Tuna, bigeye"~"Bigeye tuna"
  )) |> 
  mutate(port_code=case_when(
    port_area=="MONTEREY"~"MNA",
    port_area=="SAN FRANCISCO"~"SFA",
    port_area=="EUREKA"~"ERA",
    port_area=="LOS ANGELES"~"LAA",
    port_area=="SAN DIEGO"~"SDA",
    port_area=="SANTA BARBARA"~"SBA",
    port_area=="MORRO BAY"~"MRA",
    port_area=="FORT BRAGG"~"BGA",
    port_area=="BODEGA BAY"~"BDA")) |> 
  arrange(comm_name)

cdfw_match_port<-rbind(cdfw_match_port,cdfw_rock_port) |> 
  mutate(join_index=paste0(comm_name,year,port_code))



# Join the port cdfw data with the wcfish data
cali_top_port_join<-cali_top_port |>
  mutate(join_index=paste0(comm_name,year,port_code)) |>
  full_join(cdfw_match_port,by=c("join_index"))

### try making an index from the original cali_top_port combination of port/species then filtering

# check that it matches original list

check_list <- cali_top_port_join %>%
  group_by(spp_code, port_code) %>%
  summarize(total = sum(landings_mt, na.rm = TRUE), .groups = 'drop')


# In cali_top_join mutate the value and pounds column to replace the value_usd and landings_lb columns, but only for years 2021,2022, and 2023

cali_top_join<-cali_top_join |>
  mutate(value_usd=case_when(year>2020~value,
                             TRUE~value_usd),
         landings_lb=case_when(year>2020~pounds,
                               TRUE~landings_lb)) |>
  mutate(price_usd_lb=value_usd/landings_lb) |> 
  mutate(landings_mt=landings_lb/2204.62) |> 
  select(-c(pct_land,pct_value,value,pounds,join_index.x,join_index.y)) |> 
  rename(cdfw_name=species_name)

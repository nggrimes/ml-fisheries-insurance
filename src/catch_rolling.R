library(tidyverse)
library(wcfish)

# make a function to see if numbers are consecutive
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

cali_consecutive<-pacfin_all1 %>%
  filter(state=="California") %>%
  group_by(species_code) %>% 
  drop_na(landings_mt) %>%
  nest() %>%
  mutate(consecutive=map_lgl(data, purr_con)) |> 
  filter(consecutive==TRUE) |> 
  unnest(data) |> 
  summarise(n=n_distinct(year)) |> 
  filter(n>25)

ca_clean<-pacfin_all1 %>%
  filter(state=="California") %>%
  filter(species_code %in% cali_consecutive$species_code)

# repeat for Oregon
oregon_consecutive<-pacfin_all1 %>%
  filter(state=="Oregon") %>%
  drop_na(landings_mt) %>%
  group_by(species_code) %>%
  nest() %>%
  mutate(consecutive=map_lgl(data, purr_con)) |> 
  filter(consecutive==TRUE) |> 
  unnest(data) |> 
  summarise(n=n_distinct(year)) |> 
  filter(n>25)


or_clean<-pacfin_all1 |> 
  filter(state=="Oregon") |> 
  filter(species_code %in% oregon_consecutive$species_code)

# repeat for Washington
washington_consecutive<-pacfin_all1 %>%
  filter(state=="Washington") %>%
  group_by(species_code) %>%
  drop_na(landings_mt) %>%
  nest() %>%
  mutate(consecutive=map_lgl(data, purr_con)) |> 
  filter(consecutive==TRUE) |> 
  unnest(data) |> 
  summarise(n=n_distinct(year)) |> 
  filter(n>25)

wa_clean<-pacfin_all1 %>%
  filter(state=="Washington") %>%
  filter(species_code %in% washington_consecutive$species_code)

# create a 5 year rolling average column for landings and revenue
rolling_avg<-function(df){
  df %>%
    mutate(rolling_avg_landings=zoo::rollmean(landings_mt, k=5, fill=NA,align = 'right')) %>%
    mutate(rolling_avg_revenue=zoo::rollmean(value_usd, k=5, fill=NA,align = 'right'))
}

ca_clean_roll<-ca_clean %>%
  group_by(species_code) %>%
  nest() %>%
  mutate(rolling_avg=map(data, rolling_avg)) %>%
  unnest(rolling_avg) |> 
  select(-data)

collapse<-0.75 # What coverage is a collapse
# define collapse thresholds as X% decline in landings or revenue
ca_collapse_75<-ca_clean_roll |> 
  group_by(species_code) |> 
  mutate(landings_decline=landings_mt/rolling_avg_landings) |> 
  mutate(revenue_decline=value_usd/rolling_avg_revenue) |> 
  mutate(collapse=ifelse(landings_decline<collapse,1,0))

wa_clean_roll<-wa_clean %>%
  group_by(species_code) %>%
  nest() %>%
  mutate(rolling_avg=map(data, rolling_avg)) %>%
  unnest(rolling_avg) |> 
  select(-data)

# define collapse thresholds as X% decline in landings or revenue
wa_collapse_75<-wa_clean_roll |> 
  group_by(species_code) |> 
  mutate(landings_decline=landings_mt/rolling_avg_landings) |> 
  mutate(revenue_decline=value_usd/rolling_avg_revenue) |> 
  mutate(collapse=ifelse(landings_decline<collapse,1,0))

or_clean_roll<-or_clean %>%
  group_by(species_code) %>%
  nest() %>%
  mutate(rolling_avg=map(data, rolling_avg)) %>%
  unnest(rolling_avg) |> 
  select(-data)

# define collapse thresholds as X% decline in landings or revenue
or_collapse_75<-or_clean_roll |> 
  group_by(species_code) |> 
  mutate(landings_decline=landings_mt/rolling_avg_landings) |> 
  mutate(revenue_decline=value_usd/rolling_avg_revenue) |> 
  mutate(collapse=ifelse(landings_decline<collapse,1,0))

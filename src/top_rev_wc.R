# WCfish data manipulation

# Load the data

library(wcfish)
library(tidyverse)

## Monthly data stored in pacfin_all6 by state

## CA monthly data by prot complex in swfsc

## Annual landing by state and port complex in pacfin_all5

##### Finding the top 10 species by revenue in the West Coast in the last 10 years #####
nchoice<-10

top_wc <- pacfin_all5 %>%
  filter(spp_code != "Confidential") %>%
  filter(year >= 2010) %>%
  group_by(spp_code,state) %>%
  summarise(revenue = sum(landings_mt)) |> 
  ungroup()

top_10_ca<- top_wc %>%
  filter(state == "California") %>%
  slice_max(revenue, n = nchoice)

top_10_or<- top_wc %>%
  filter(state == "Oregon") %>%
  slice_max(revenue, n = nchoice)

top_10_wa<- top_wc %>%
  filter(state == "Washington") %>%
  slice_max(revenue, n = nchoice)

top_ca_rev<-pacfin_all5 %>%
  filter(spp_code %in% top_10_ca$spp_code) %>%
  filter(state == "California")

# Get the names and extract. This allows us to keep track of all other information in pac_fin

top_or_rev<-pacfin_all5 %>%
  filter(spp_code %in% top_10_or$spp_code) %>%
  filter(state == "Oregon")

top_wa_rev<-pacfin_all5 %>%
  filter(spp_code %in% top_10_wa$spp_code) %>%
  filter(state=="Washington")

#plot the top 10 species from califoria by landings
top_ca_rev %>%
  ggplot(aes(x=year, y=landings_mt, fill=spp_code)) +
  geom_col() +
  facet_wrap(~spp_code, scales = "free_y") +
  labs(title = "Top 10 species by revenue in California",
       x = "Year",
       y = "Landings (mt)") +
  theme_minimal()

# save outputs
save(top_ca_rev,top_or_rev,top_wa_rev, file = here::here("data","top_species_rev.RData"))




library(wcfish)
library(sf)
library(tidyverse)
library(raster)
library(rnaturalearth)
library(pals)
library(ncdf4)
library(patchwork)

state<-ne_states(country='United States of America')

state_sf<-st_as_sf(state) %>% 
  filter(name %in% c('California','Oregon','Washington','Nevada'))
load(here::here('data','environmental','sst_dhw_2021-2023.Rdata'))

sst_p<-sst %>% 
  filter(t=='2021-01-16') %>% 
  ggplot()+
  geom_tile(aes(x=longitude,y=latitude,fill=temp))+
  scale_fill_gradientn(colours=ocean.thermal(20),guide = "colourbar")+
  geom_sf(data=state_sf)+
  geom_sf(data=blocks,fill=NA,color='white')+
  coord_sf(xlim = c(-129,-117),ylim=c(30,42))+
  scale_y_continuous(expand=c(0,0))+
  theme_classic()+
  labs(x = NULL, y = NULL, fill = "SST (°C)") +
  theme(legend.position = c(0.85, 0.75),
        axis.text.x=element_text(angle=45,hjust=1),
        legend.title = element_text(size=6),
        legend.text = element_text(size=5),
        legend.key.height = unit(dev.size()[1] / 40, "inches"))

### Weather catch map ###
load(here::here('data','environmental','sst_blocks.rda'))

msqd<-readxl::read_xlsx(here::here('data','blocks','state','msqd.xlsx')) %>% 
  drop_na()

catch_data<-msqd %>% 
  janitor::clean_names() %>% 
  drop_na(total_pounds) %>% 
  filter(total_pounds>0) %>% 
  filter(block_id!="Total") %>% 
  mutate(block_id=as.numeric(block_id))

## construct sf blocks for intersection
block_data<-catch_data %>% 
  inner_join(blocks,by="block_id") %>% 
  mutate(pct_catch=total_pounds/sum(total_pounds)) 

sp_catch_p<-ggplot(block_data)+
  geom_sf(aes(geometry=geometry,fill=pct_catch))+
  scale_fill_viridis_c()+
  geom_sf(data=state_sf)+
  geom_sf(data=blocks,fill=NA,color='black')+
  coord_sf(xlim = c(-129,-117),ylim=c(30,42))+
  theme_classic()+
  labs(fill="% of Catch")+
  theme(legend.position = c(0.85, 0.75),
        axis.text.x=element_text(angle=45,hjust=1),
        legend.title = element_text(size=6),
        legend.text = element_text(size=5),
        legend.key.height = unit(dev.size()[1] / 40, "inches"))



block_data<-block_data %>% 
  st_drop_geometry() %>% 
  dplyr::select(block_id,pct_catch)



sst_join<-sst_blocks %>%
  filter(year==2021 & month==1) %>% 
  filter(block_id %in% block_data$block_id) %>% 
  inner_join(blocks) %>% 
  st_as_sf()

p1<-ggplot()+
  geom_sf(data=sst_join,aes(fill=sst))+
  scale_fill_gradientn(colours=ocean.thermal(20),guide = "colourbar")+
  geom_sf(data=state_sf)+
  geom_sf(data=blocks,fill=NA,color='black')+
  coord_sf(xlim = c(-129,-117),ylim=c(30,42))+
  scale_y_continuous(expand=c(0,0))+
  theme_classic()+
  labs(x = NULL, y = NULL, fill = "SST (°C)",title="") +
  theme(legend.position = c(0.85, 0.75),
        axis.text.x=element_text(angle=45,hjust=1),
        legend.title = element_text(size=6),
        legend.text = element_text(size=5),
        legend.key.height = unit(dev.size()[1] / 40, "inches"))

((sst_p / sp_catch_p) | p1) +
  plot_layout(widths = c(1.2, 0.8)) +
  plot_annotation(tag_levels = 'A') &
  theme(
    plot.margin = margin(0, 0, 0, -10)  # reduce right margin of left column
  )

ggsave(here::here("data","fig","cw_catch.png"),height=5,width=7,dpi=300)

#### RMSE graphs #####

library(tidytext)

load(here::here("data","output","squid_cara10-1.rda"))
p1<-models |> 
  mutate(group=c(rep('Univariate',12),rep('Multivariate',4))) |> 
  ggplot(aes(x=fct_reorder2(pred_mod,group,rmse),y=rmse,fill=group))+
  geom_col()+
  labs(x='Model',y='RMSE',fill='')+
  theme_classic()+
  theme(axis.text.x = element_text(angle=45,hjust=1),)+
  scale_y_continuous(expand=c(0,0))+
  scale_fill_manual(values=c("#003660","#047C90"))

p2<-models |> 
  mutate(group=c(rep('Univariate',12),rep('Multivariate',4))) |> 
  ggplot(aes(x=fct_reorder2(pred_mod,group,rmse),y=rmse_test,fill=group))+
  geom_col()+
  labs(x='Model',y='RMSE',fill='')+
  theme_classic()+
  theme(axis.text.x = element_text(angle=45,hjust=1),)+
  scale_y_continuous(expand=c(0,0))+
  scale_fill_manual(values=c("#003660","#047C90"))

p1/p2+plot_annotation(tag_levels = 'A')

ggsave(here::here("data","fig","rmse.png"),height=7,width=5.5,dpi=300)
#### Actuarially fair graphs ####

ur_p<-models |> 
  mutate(group=c(rep('L',12),rep('M',4))) |> 
  group_by(group) %>%
  arrange(desc(u_rr), .by_group = TRUE) %>%
  mutate(cat_group = factor(paste(group, pred_mod, sep = "_"),
                            levels = paste(group, pred_mod, sep = "_"))) %>%
  ungroup() |> 
  ggplot(aes(x=cat_group,y=u_rr,fill=group))+
  geom_col()+
  labs(x='Model',y='Percent increase in utility\nwith insurance',fill='')+
  theme_classic()+
  theme(axis.text.x = element_text(angle=45,hjust=1),)+
  scale_x_discrete(labels = c("Frequency","Relax","Krill","STI","ONI","Squid Larvae","Spring CUTI","SST","Spring BEUTI","PDO","ENSO","CHCI","SVM","LASSO","Random forest","Regularized RF"))+
  scale_y_continuous(expand=c(0,0),labels=scales::percent)+
  scale_fill_manual(values=c("#003660","#047C90"),labels=c("Univariate","Multivariate"))+
  geom_hline(yintercept=0,linewidth=0.5,color='grey')

lr_p<-models |> 
  mutate(group=c(rep('L',12),rep('M',4))) |> 
  group_by(group) %>%
  arrange(desc(u_rr), .by_group = TRUE) %>%
  mutate(cat_group = factor(paste(group, pred_mod, sep = "_"),
                            levels = paste(group, pred_mod, sep = "_"))) %>%
  ungroup() |> 
  ggplot(aes(x=cat_group,y=lr,fill=group))+ 
  geom_col()+
  labs(x='Model',y='Insurance Loss Ratio',fill='')+
  theme_classic()+
  theme(axis.text.x = element_text(angle=45,hjust=1),)+
  scale_y_continuous(expand=c(0,0))+
  scale_x_discrete(labels = c("Frequency","Relax","Krill","STI","ONI","Squid Larvae","Spring CUTI","SST","Spring BEUTI","PDO","ENSO","CHCI","SVM","LASSO","Random forest","Regularized RF"))+
  scale_fill_manual(values=c("#003660","#047C90"),label=c("Univariate","Multivariate"))+
  geom_hline(yintercept=1,size=1,color='black')

ur_p/lr_p+plot_annotation(tag_levels = 'A')+plot_layout(axes='collect',guides="collect")

ggsave(here::here("data","fig","act_fair.png"),height=7,width=5.5,dpi=300)
#### Market premium graphs #####

urm_p<-models |> 
  mutate(group=c(rep('L',12),rep('M',4))) |> 
  group_by(group) %>%
  arrange(desc(u_rr), .by_group = TRUE) %>%
  mutate(cat_group = factor(paste(group, pred_mod, sep = "_"),
                            levels = paste(group, pred_mod, sep = "_"))) %>%
  ungroup() |> 
  filter(u_rr>0) |> 
  ggplot(aes(x=cat_group,y=m_eq,fill=group))+
  geom_col()+
  labs(x='Model',y='Premium Loading\nLoading Factor (m)',fill='')+
  theme_classic()+
  theme(axis.text.x = element_text(angle=45,hjust=1),)+
  scale_y_continuous(expand=c(0,0))+
  scale_x_discrete(labels = c("Frequency","Relax","Krill","SVM","LASSO","Random forest","Regularized RF"))+
  scale_fill_manual(values=c("#003660","#047C90"),labels=c("Univariate","Multivariate"))+
  geom_hline(yintercept=1,linewidth=0.5,color='black')

lrm_p<-models |> 
  mutate(group=c(rep('L',12),rep('M',4))) |> 
  group_by(group) %>%
  arrange(desc(u_rr), .by_group = TRUE) %>%
  mutate(cat_group = factor(paste(group, pred_mod, sep = "_"),
                            levels = paste(group, pred_mod, sep = "_"))) %>%
  ungroup() |> 
  filter(u_rr>0) |> 
  ggplot(aes(x=cat_group,y=lr_m,fill=group))+  geom_col()+
  labs(x='Model',y='Insurance Loss Ratio',fill='')+
  theme_classic()+
  theme(axis.text.x = element_text(angle=45,hjust=1),)+
  scale_y_continuous(expand=c(0,0))+
  scale_x_discrete(labels = c("Frequency","Relax","Krill","SVM","LASSO","Random forest","Regularized RF"))+
  scale_fill_manual(values=c("#003660","#047C90"),labels=c("Univariate","Multivariate"))+
  geom_hline(yintercept=1,linewidth=0.5,color='black')

urm_p/lrm_p+plot_annotation(tag_levels = 'A')+plot_layout(axes='collect',guides='collect')

ggsave(here::here("data","fig","market_prem.png"),height=7,width=5.5,dpi=300)


#### Insurance payout ####
load(here::here("data","fisheries","cali_catch_detrend_2.rda"))

#load weather data
load(here::here("data","environmental","block_beuti.rda"))
load(here::here("data","environmental","block_cuti.rda"))
load(here::here("data","environmental","block_hci.rda"))
load(here::here("data","environmental","block_sst.rda"))
load(here::here("data","environmental","enso_pdo.rda"))
load(here::here('data','fisheries','squid_bio_all.Rdata'))

source(here::here("src","fcn","cw_squid.R"))

port_cw<-cali_catch %>% 
  filter(species_code=='MSQD') %>% 
  mutate(mt_per_detrend=case_when(mt_per_detrend==Inf~0,
                                  .default=as.numeric(mt_per_detrend)),
         lb_per_fisher=case_when(lb_per_fisher==Inf~0,
                                 .default=as.numeric(lb_per_fisher))) %>% 
  filter(year>=1990) |> 
  nest() %>% 
  mutate(cw_data=pmap(list(spp=species_code,data=data),cw_squid))


df<-port_cw$cw_data[[1]] |> 
  filter(fish_var=='mt_per_detrend') |> 
  filter(!var %in% c('amp_beuti','avg_beuti','f_beuti','s_beuti','w_beuti','amp_cuti','avg_cuti','f_cuti','s_cuti','w_cuti','year')) |> 
  pivot_wider(names_from = var, values_from = value) |> 
  drop_na()

pay_svm<-models |> 
  filter(pred_mod=='svm') |> 
  dplyr::select(payout_vec) |> 
  unlist()

prem_svm<-models |> 
  filter(pred_mod=='svm') |> 
  dplyr::select(prem_vec) |> 
  unlist()

pay_rf<-models |> 
  filter(pred_mod=='rf') |> 
  dplyr::select(payout_vec) |> 
  unlist()

prem_rf<-models |> 
  filter(pred_mod=='rf') |> 
  dplyr::select(prem_vec) |> 
  unlist()

max_len <- max(length(df$fish_value), length(pay_svm))

# Pad each vector
fish_value <- c(df$fish_value, rep(NA, max_len - length(df$fish_value)))

net_p_svm <- c(rep(NA, max_len - length(pay_svm)),pay_svm-prem_svm)

ins_value_svm<-net_p_svm+fish_value

net_p_rf <- c(rep(NA, max_len - length(pay_rf)),pay_rf-prem_rf)

ins_value_rf<-net_p_rf+fish_value

inspay<-data.frame(svm=ins_value_svm,rf=ins_value_rf,fish_value=fish_value,year=df$year) |> 
  ggplot()+
  geom_line(aes(x=year,y=fish_value),color='#003660')+
  geom_point(aes(x=year,y=fish_value,color='Harvest'))+
  geom_point(aes(x=year,y=svm,color='svm'),size=3,shape=16)+
  geom_point(aes(x=year,y=rf,color='rf'),size=3,shape=17)+
  theme_classic()+
  scale_color_manual(values=c(Harvest='#003660',svm='#900C3F',rf="#09847A"),labels=c("Harvest"="Harvest","svm"="SVM","rf"="Random Forest"),name="")+
  geom_hline(yintercept=mean(df$fish_value))+
  labs(x='',y='Harvest per fisher (MT)')+
  theme(legend.position = "bottom")

ggsave(here::here("data","fig","ins_pay.png"),height=6,width=5.5,dpi=300)

#### Payout and premium vectors in main results of paper ####

pay_out<-as.data.frame(map(models$payout_vec,~unlist(.x)))

prem_out<-as.data.frame(map(models$prem_vec,~unlist(.x)))

m_prem_out<-as.data.frame(mapply(`*`,prem_out,models$m_eq))

colnames(pay_out)<-models$pred_mod

colnames(m_prem_out)<-models$pred_mod

years<-seq(2014,2023)


pay_df<-pay_out |> 
  mutate(year=years) |>
  pivot_longer(-year,names_to = "model",values_to = "payout") |>
  filter(!model %in% c("sp_cuti","squid","avg_sst","chci","oni","pdo","enso","sp_beuti","sti")) |>
  mutate(group=ifelse(model %in% c("rf","grrf","svm","lasso"),"Multivariate","Univariate")) |> 
  ggplot(aes(x=year,y=payout,color=model))+
  geom_line()+
  geom_point()+
  theme_classic()+
  labs(title="",
       x="",
       y="Payout",
       color="Model")+
  theme(legend.position = "bottom")+
  facet_wrap(~group,scales = "fixed")+
  scale_color_manual(values=c("#FEBC11","#DF8073","#00A3E0",
                              "#003660","#09847A",
                              "#6D7D33","#EF5645"))

ggsave(here::here("data","fig","pay_out.png"),height=6,width=5.5,dpi=300)


prem_fig<-m_prem_out |> 
  mutate(year=years) |>
  pivot_longer(-year,names_to = "model",values_to = "prem") |>
  filter(!model %in% c("sp_cuti","squid","avg_sst","chci","oni","pdo","enso","sp_beuti","sti")) |>
  mutate(group=ifelse(model %in% c("rf","grrf","svm","lasso"),"Multivariate","Univariate")) |> 
  ggplot(aes(x=year,y=prem,color=model))+
  geom_line()+
  geom_point(aes(shape=model))+
  theme_classic()+
  labs(title="",
       x="",
       y="Premium Paid")+
  theme(legend.position = "bottom")+
  facet_wrap(~group,scales = "fixed")+
  scale_color_manual(name='Model',
                     values=c("#FEBC11","#DF8073","#00A3E0",
                              "#003660","#09847A",
                              "#6D7D33","#EF5645"))+
  scale_shape_manual(name="Model",
                     values=seq(0,6))

ggsave(here::here("data","fig","prem_out.png"),height=6,width=5.5,dpi=300)



#### VIP figures #####

load(here::here("data","output","vip_models.rda"))

my_palette <- c(
  "avg_sst"="#DF8073", # muted coral
  "sp_beuti" = "#FEBC11", # golden yellow
  "freq"= "#003660", # navy blue
  "enso"= "#09847A", # teal
  "krill" ="#6D7D33", # olive green
  "squid" = "#EF5645", # bright red-orange
  
  "sti"= "#7B3C8C", # purple
  "relax"= "#00A9E0", # sky blue
  "sp_cuti" = "#FF7F0E", # orange
  "chci" = "#2CA02C", # green
  "pdo"= "#9467BD", # lavender
  "oni" ="#8C564B"  # brown
)

names(my_palette)<-unique(out$Variable)

shapes_12 <- c(
  "avg_sst"= 0,
  "sp_beuti" = 1,
  "freq"= 2,
  "enso"= 3,
  "krill" = 4,
  "squid" = 5,
  
  "sti"= 6,
  "relax"= 7,
  "sp_cuti" = 8,
  "chci" = 9,
  "pdo"= 10,
  "oni" =11
)

names(shapes_12)<-unique(out$Variable)

### Quick plot  ###
vip_p<-out |> 
  filter(metric=="Importance") |> 
  ggplot()+
  geom_line(aes(x=year,y=value,color=Variable))+
  geom_point(aes(x=year,y=value,color=Variable,shape=Variable))+
  facet_wrap(~model,scales='free',
             labeller=labeller(model=c(
               'grrf'='Regularized RF',
               'rf'='Random Forest',
               'svm'= 'SVM'
             )))+
  labs(title = "",
       x = "",
       y = "Variable Importance")+
  scale_color_manual(values = my_palette)+
  scale_shape_manual(values = shapes_12)+
  theme_classic()+
  scale_x_continuous(breaks=c(2014,2018,2022))

ggsave(here::here("data","fig","vip_models.png"),vip_p,height=5,width=6,dpi=300)

#,label=c("SST","BEUTI","Frequency","ENSO","Krill","Squid","STI","Relax","CUTI","CHCI","PDO","ONI")

#### Lasso VIP ####

load(here::here("data","output","vip_lasso.rda"))

zoom_data <- las_df_long %>%
  filter(value<0.1)

# Add a 'panel' variable to distinguish between full and zoomed views
data_for_plot <- las_df_long %>%
  mutate(panel = "Full Data") %>%
  bind_rows(
    zoom_data %>%
      mutate(panel = "Zoomed In")
  )

# Plot using facet_wrap
ggplot(data_for_plot, aes(x = year, y = value,color=Variable)) +
  geom_line() +
  geom_point(aes(shape=Variable)) +
  facet_wrap(~ panel, scales = "free") + # scales = "free" allows different axis limits
  labs(title = "",
       x = "",
       y = "Variable Importance")+
  scale_color_manual(values = my_palette)+
  scale_shape_manual(values = shapes_12)+
  scale_x_continuous(breaks=c(2014,2018,2022))+
  theme_classic()

ggsave(here::here("data","fig","vip_lasso.png"),height=5,width=6,dpi=300)

#### Correlation graphs ####
my_palette <- c(
  "Upwelling"= "#003660", # navy blue
  "Regional"= "#09847A", # teal
  "Temperature" ="#6D7D33", # olive green
  "Biological"= "#7B3C8C" # purple

)


obj<-df |> 
  dplyr::select(-c(year,fish_var)) |> 
  pivot_longer(cols=-fish_value)

obj$label<-rep(c("Temperature","Upwelling","Upwelling","Upwelling","Upwelling","Upwelling","Temperature","Regional","Regional","Regional","Biological","Biological"),length.out=nrow(obj)) 
  
label_order <- obj %>%
  group_by(name) %>%
  summarise(main_label = first(sort(unique(label)))) %>%
  arrange(main_label) %>%
  pull(name)

obj$name <- factor(obj$name, levels = label_order)

txt_label<-df |> 
  dplyr::select(-c(year,fish_var)) |> 
  pivot_longer(cols=-fish_value) |> 
  group_by(name) |> 
  summarize(corr=cor(value,fish_value)) |> 
  mutate(label = paste0("r = ", round(corr, 2)))

range_df <- obj %>%
  group_by(name) %>%
  summarise(x = max(value), y = max(fish_value))

cor_labels<-left_join(txt_label,range_df,by='name')

ggplot(data=obj,aes(x=value,y=fish_value,color=label))+
  geom_point()+
  geom_smooth(method='lm',se=FALSE)+
  facet_wrap(~name,scales='free',
             labeller=labeller(name=c(
               'avg_sst'='SST',
               'chci'='Habitat Compression',
               'enso'= 'ENSO (Mei v2)',
               'freq'='Frequency',
               'krill'='Krill Abundance',
               'oni'='ONI',
               'pdo'='PDO',
               'relax'='Relax',
               'sp_beuti'='Spring BEUTI',
               'sp_cuti'='Spring CUTI',
               'sti'='Spring Transition',
               'squid'='Squid')))+
  labs(x='',
       y='Harvest (mt)')+
  theme_minimal()+
  scale_color_manual(values=my_palette,name='')+
  geom_label(data=cor_labels,aes(x=x,y=y,label=label),inherit.aes=FALSE,hjust=1,vjust=1)
  

ggsave(here::here("data","fig","corr.png"),height=5,width=8,dpi=600)



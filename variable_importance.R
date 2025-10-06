### Extract variable importance from each model
library(vip)
mod<-models$mods[15]

### Do it manually because I can't be bothered to make a prediction function for each type of output

### svm##

mod<-models$mods[15]

a<-mod[[1]][[1]]

#get training data
df<-port_cw$cw_data[[1]] |> 
  filter(fish_var=='mt_per_detrend') |> 
  filter(!var %in% c('amp_beuti','avg_beuti','f_beuti','s_beuti','w_beuti','amp_cuti','avg_cuti','f_cuti','s_cuti','w_cuti','year')) |> 
  pivot_wider(names_from = var, values_from = value) |> 
  drop_na()


data <- df |>
  dplyr::select(-c(,"pdo","oni","chci","sti","relax","sp_cuti"))

out<-data.frame()

  for(i in 25:34){
    
    mod<-a[[i-24]]
    
    train<-data[1:(i-1),]
    
    test<-data[i,]
    
    train_x<-train |> 
      dplyr::select(-c(fish_var,year))
    

    vi_tbl <- vi_permute(
      object       = mod,
      feature_names = colnames(train_x)[-which(names(train_x) == "fish_value")],
      train        = train_x,
      target       = "fish_value",
      metric       = "rmse",
      pred_wrapper = function(object, newdata) as.vector(predict(object, newdata)),
      nsim         = 50
    ) |> 
      mutate(year=2013+i-24) |> 
      pivot_longer(cols=-c(Variable,year),names_to='metric',values_to='value')
    
    vi_tbl$model<-"svm"
    ## combine results over time
    
    out<-rbind(out,vi_tbl)
  }

### Do it for grrf

mod<-models$mods[14]

a<-mod[[1]][[1]]




for(i in 25:34){
  
  mod<-a[[i-24]]
  train<-df[1:(i-1),]
  
  test<-df[i,]
  
  train_x<-train |> 
    dplyr::select(-c(fish_var,year))
  
  
  vi_tbl <- vi_permute(
    object       = mod,
    feature_names = colnames(train_x)[-which(names(train_x) == "fish_value")],
    train        = train_x,
    target       = "fish_value",
    metric       = "rmse",
    pred_wrapper = function(object, newdata) as.vector(predict(object, newdata)),
    nsim         = 50
  ) |> 
    mutate(year=2013+i-24) |> 
    pivot_longer(cols=-c(Variable,year),names_to='metric',values_to='value')
  
  vi_tbl$model<-"grrf"
  ## combine results over time
  
  out<-rbind(out,vi_tbl)
}

### Do it for ranger


mod<-models$mods[13]

a<-mod[[1]][[1]]



for(i in 25:34){
  
  mod<-a[[i-24]]
  train<-data[1:(i-1),]
  
  test<-data[i,]
  
  train_x<-train |> 
    dplyr::select(-c(fish_var,year))
  
  
  vi_tbl <- vi_permute(
    object       = mod,
    feature_names = colnames(train_x)[-which(names(train_x) == "fish_value")],
    train        = train_x,
    target       = "fish_value",
    metric       = "rmse",
    pred_wrapper = function(object, newdata) predict(object, data = newdata)$predictions,
    nsim         = 50
  ) |> 
    mutate(year=2013+i-24) |> 
    pivot_longer(cols=-c(Variable,year),names_to='metric',values_to='value')
  
  vi_tbl$model<-"rf"
  ## combine results over time
  
  out<-rbind(out,vi_tbl)
}

save(out,file=here::here("data","output","vip_models.rda"))


my_palette <- c(
  "#DF8073", # muted coral
  "#FEBC11", # golden yellow
  "#003660", # navy blue
  "#09847A", # teal
  "#6D7D33", # olive green
  "#EF5645", # bright red-orange
  
  "#7B3C8C", # purple
  "#00A9E0", # sky blue
  "#FF7F0E", # orange
  "#2CA02C", # green
  "#9467BD", # lavender
  "#8C564B"  # brown
)

shapes_12 <- c(
  0,  # square
  1,  # circle
  2,  # triangle point-up
  3,  # plus
  4,  # cross (x)
  5,  # diamond
  6,  # triangle point-down
  7,  # square cross
  8,  # star
  9,  # diamond plus
  10, # circle plus
  11  # up/down triangle
)

### Quick plot  ###
out |> 
  filter(metric=="Importance") |> 
  ggplot()+
  geom_line(aes(x=year,y=value,color=Variable))+
  geom_point(aes(x=year,y=value,color=Variable,shape=Variable))+
  facet_wrap(~model,scales='free')+
  scale_color_manual(values = my_palette)+
  scale_shape_manual(values = shapes_12)

#### Extract coefficients from each glmnet LASSO model ####
mod<-models$mods[16]

a<-mod[[1]]

las_df<-data.frame()
for(i in 25:34){
  # lambda =4.98 from best models
  
  coef_matrix<-coef(a[[i-24]],s=a[[i-24]]$lambda[39])

  coef_vec<-as.numeric(coef_matrix)
  
  coef_vec_df<-as.data.frame(coef_vec) |> 
    mutate(feature = rownames(coef_matrix)) |> 
    pivot_wider(names_from = feature, values_from = coef_vec) |>
    mutate(year=2013+i-24)
  
  
  
  
  las_df<-rbind(las_df,coef_vec_df)
}


## Transform to relative importance score

las_df |> 
  pivot_longer(cols=-year,names_to='Variable',values_to='value') |> 
  filter(Variable!='(Intercept)') |> 
  group_by(year) |> 
  mutate(value=abs(value),
         value=value/sum(value)) -> las_df_long


save(las_df_long,file=here::here("data","output","vip_lasso.rda"))



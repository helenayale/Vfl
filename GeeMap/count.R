library(dplyr)

setwd('D:/GIS_Data/Vfl-oak/GEEMap/')
field_names <- c('Gei', 'Loh', 'Roh90', 'Roh620', 'Roh635')

dat_all <- read.csv('Dat_all_pix_dc.csv')
head(dat_all)

sub_dat <- dat_all[, c('ID', 'Field', 'Parcel', 'pix', 'year', 'month', 'day', 'DOY', 'NDVI')]

sub_dat <- na.omit(sub_dat)
head(sub_dat)

sub_dat$half <- ifelse(sub_dat$day < 16, 1, 2)
head(sub_dat)

for (yr in 2017:2022){
  sub_yr <- subset(sub_dat, year == yr)
  plot_name <- unique(sub_yr$ID)
  for (plot_i in 1: length(plot_name)){
    sub_plt <- subset(sub_yr, ID == plot_name[plot_i])
    par_n <- max(sub_plt$Parcel)
    for (par_i in 1:par_n){
      sub_par <- subset(sub_plt, Parcel == par_i)
      pix_n <- max(sub_par$pix)
      
      ct <- sub_par %>% 
        group_by(DOY = DOY) %>%
        mutate(mNDVI = mean(NDVI), sd = sd(NDVI))
      
      count <- ct  %>% 
        group_by(plot = ID, par = Parcel, year = year, DOY = DOY, mNDVI = mNDVI, sd = sd)  %>% 
        count()
      
      if (par_i == 1){
        dat_plt <- count
      }else{
        dat_plt <- rbind(dat_plt, count)
      }
    }
    
    if (plot_i == 1){
      dat_yr <- dat_plt
    }else{
      dat_yr <- rbind(dat_yr, dat_plt)
    }
  }
  
  if (yr == 2017){
    dat_ct <- dat_yr
  }else{
    dat_ct <- rbind(dat_ct, dat_yr)
  }
}

dat_ct
head(dat_ct)


write.csv(dat_ct, 'count.csv')

ct_spr <- subset(dat_ct, DOY >= 105)
ct_spr <- subset(ct_spr, DOY <= 135)

head(ct_spr)

ct_sum <- subset(dat_ct, DOY >= 213)
ct_sum <- subset(ct_sum, DOY <= 227)

head(ct_sum)

ct_aut <- subset(dat_ct, DOY >= 274)
ct_aut <- subset(ct_aut, DOY <= 289)

head(ct_aut)


count <- ct_spr%>% 
  group_by(plot = plot, par = par, year = year)  %>% 
  count()

count

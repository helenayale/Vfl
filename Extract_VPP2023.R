library(sp)
#library(rgdal)
library(raster)
library(gtools)
library(sf)

library(terra)

library(TDPanalysis)

setwd('D:/GIS_Data/Vfl-oak/WekEO/')
field_names <- c('Gei', 'Loh', 'Roh90', 'Roh620', 'Roh635')
folder_names <- c('Gei', 'Roh', 'Roh', 'Roh', 'Roh')
img_names <- c('UPV', 'UNA', 'UNA', 'UNA', 'UNA')
ver_names <- c('101', '101', '101', '101', '105', '105', '105', '105')

for (yr in 2017:2024){
  print(yr)
  for (plot_i in 1:length(field_names)){
    img_sos <- brick(paste('D:/GIS_Data/Vfl-oak/WekEO/', folder_names[plot_i], '/SOSDate/VPP_', yr, '_S2_T32', img_names[plot_i], '-010m_V', ver_names[yr - 2016], '_s1_SOSD.tif', sep = ''))
    img_eos <- brick(paste('D:/GIS_Data/Vfl-oak/WekEO/', folder_names[plot_i], '/EOSDate/VPP_',yr, '_S2_T32', img_names[plot_i], '-010m_V', ver_names[yr - 2016], '_s1_EOSD.tif', sep = ''))
    eval(parse(text = paste('shape <- st_read("D:/GIS_Data/Vfl-oak/Vfl-detail_parcels/',field_names[plot_i],'-par-wgs84.shp")', sep = '')))
    print(field_names[plot_i])
    year <- yr
    shp <- st_transform(shape, st_crs(img_sos))
    wek_sos <- terra::extract(img_sos, shp, FUN = table, na.rm=FALSE)
    wek_eos <- terra::extract(img_eos, shp, FUN = table, na.rm=FALSE)
    par <- shp$Parcel
    field <- shp$Vfl_name
    interne<- shp$Interne
    
    treat <- shp$Treat_cat
    if (length(treat) == 0){
      treat <- 'Unknown'
    }
    
    for (i in 1:length(wek_sos)){
      pix_num <- seq(1:length(wek_sos[[i]]))
      dat_pix <- data.frame(ID = field_names[plot_i], Field = field[i], Parcel = i, Interne = interne[i], 
                            year = year, pix = pix_num, SOSD = wek_sos[[i]], EOSD = wek_eos[[i]])
      colnames(dat_pix) <- c('ID', 'Field', 'Parcel', 'Interne', 'year', 'pix', 'sos', 'eos')
      
      if (i == 1){
        dat_img <- dat_pix
      }else{
        dat_img <- rbind(dat_img, dat_pix)
      }
    }
    
    if (plot_i == 1){
      dat_yr <- dat_img
    }else{
      dat_yr <- rbind(dat_yr, dat_img)
    }
    
  }
  
  if (yr == 2017){
    dat_all <- dat_yr
  }else{
    dat_all <- rbind(dat_all, dat_yr)
  }
}

head(dat_all)

dat_all$sos <- dat_all$sos - (dat_all$year -2000) *1000
dat_all$eos <- dat_all$eos - (dat_all$year -2000) *1000

head(dat_all)

write.csv(dat_all, 'WekEO_2024.csv', row.names = FALSE)
save(dat_all, file = 'dat_wek_2024.rda')

load('dat_wek_2024.rda')

dat_meta <- read.csv('meta_data2.csv')
head(dat_meta)

unique(dat_meta$key)

dat_all$key <- paste(dat_all$ID, '_', dat_all$Parcel, sep = '')
head(dat_all)
unique(dat_all$key)

dat_wek <- merge(dat_all, dat_meta, by = 'key')
head(dat_wek)

Treat_v <- dat_wek$Treat
Treat_v[Treat_v== "A-Grad"] <- 'Low'
Treat_v[Treat_v == "F-Grad" ] <- 'Medium'
Treat_v[Treat_v == "Z-Baum" ] <- 'High'


dat_wek$Treat_lvl <- factor(Treat_v, levels = c('Low', 'Medium', 'High'))
head(dat_wek)



library(ggplot2)

plt <- ggplot(
  data = dat_wek
) +
  geom_boxplot(
    aes(x = year, y = sos, group = interaction(year, ID, Treat_lvl), color = Treat_lvl)
  )+
  geom_vline(
    aes(xintercept = 2017.45),
    color = "grey",
    linetype = "dotted",
    linewidth = .8
  ) +
  geom_vline(
    aes(xintercept = 2018.45),
    color = "grey",
    linetype = "dotted",
    linewidth = .8
  ) +
  geom_vline(
    aes(xintercept = 2019.45),
    color = "grey",
    linetype = "dotted",
    linewidth = .8
  ) +
  geom_vline(
    aes(xintercept = 2020.45),
    color = "grey",
    linetype = "dotted",
    linewidth = .8
  ) +
  geom_vline(
    aes(xintercept = 2021.45),
    color = "grey",
    linetype = "dotted",
    linewidth = .8
  ) +
  geom_vline(
    aes(xintercept = 2022.45),
    color = "grey",
    linetype = "dotted",
    linewidth = .8
  ) +
  geom_vline(
    aes(xintercept = 2023.45),
    color = "grey",
    linetype = "dotted",
    linewidth = .8
  ) +
  scale_x_continuous(
    breaks = c(2017:2024)
  ) +
  labs(
    title = "Interannual Comparison SOS",
    y = "DOY", x = "Year"
  ) +
  theme_minimal(
    base_size = 15
  ) +
  facet_wrap(
    ~ID
  ) +
  theme(panel.grid = element_blank())

plt


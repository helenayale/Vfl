library(sp)
library(rgdal)
library(raster)
library(gtools)
library(sf)

library(terra)

library(TDPanalysis)

setwd('D:/GIS_Data/Vfl-oak/WekEO/')
field_names <- c('Gei', 'Loh', 'Roh90', 'Roh620', 'Roh635')
folder_names <- c('Gei', 'Roh', 'Roh', 'Roh', 'Roh')
WekEO_names <- c('EOSDate', 'EOSValue', 'SOSDate', 'SOSValue', 'SMaxDate', 'SMaxValue')
file_names <- c('EOSD', 'EOSV', 'SOSD', 'SOSV', 'MAXD', 'MAXV')

# WekEO_names <- c('EOSDate',  'SOSDate',  'SMaxDate')
# file_names <- c('EOSD', 'SOSD', 'MAXD')



for (plot_i in 1:length(field_names)){
  eval(parse(text = paste('shape <- readOGR("D:/GIS_Data/Vfl-oak/Vfl-detail_parcels/',field_names[plot_i],'-par-wgs84.shp")', sep = '')))
  folder_name <- folder_names[plot_i]
  print(field_names[plot_i])
    for (wek_i in 1:length(WekEO_names)){
      wek_name <- WekEO_names[wek_i]
      file_name <- file_names[wek_i]
      print(file_name)
      eval(parse(text = paste('file_inv <-  mixedsort(list.files(path = "D:/GIS_Data/Vfl-oak/WekEO/', folder_name, '/', wek_name,'/", pattern = ".tif$", all.files = TRUE, full.names = FALSE))', sep = '')))
      for (img_i in 1:length(file_inv)){
        print(paste('Img',img_i))
        wek_img <- brick(paste('D:/GIS_Data/Vfl-oak/WekEO/', folder_name, '/', wek_name, '/', file_inv[img_i], sep = ''))
        date_start <- 5
        int <- 0
        year <- substr(file_inv[img_i],date_start,date_start+3)
        shp <- spTransform(shape, CRS(proj4string(wek_img)))
        wek <- terra::extract(wek_img, shp, FUN = table, na.rm=FALSE)
        par <- shp$Parcel
        field <- shp$Vfl_name
        interne<- shp$Interne
        
        treat <- shp$Treat_cat
        if (length(treat) == 0){
          treat <- 'Unknown'
        }
        
        for (i in 1:length(wek)){
          pix_num <- seq(1:length(wek[[i]]))
          dat_pix <- data.frame(ID = field_names[plot_i], Field = field[i], Parcel = i, Interne = interne[i], 
                                year = year, pix = pix_num, value = wek[[i]])
          colnames(dat_pix) <- c('ID', 'Field', 'Parcel', 'Interne', 'year', 'pix', 'Treat', file_name)
          
          if (i == 1){
            dat_img <- dat_pix
          }else{
            dat_img <- rbind(dat_img, dat_pix)
          }
        }
        if (img_i == 1){
          dat_wek <- dat_img
        }else{
          dat_wek <- rbind(dat_wek, dat_img)
        }
      }
      
      if (wek_i == 1){
        dat_plot <- dat_wek
      }else{
        eval(parse(text = paste('dat_plot$', file_name, '<- dat_wek$', file_name, sep = '')))
      }
      
    }
    
  if (plot_i == 1){
    dat_all <- dat_plot
  }else{
    dat_all <- rbind(dat_all, dat_plot)
  }
  
}

dat_all

write.csv(dat_all, "WekEO_orig.csv", row.names = FALSE)

dat_orig <- dat_all

dat_all$EOSD <- dat_all$EOSD - (as.numeric(dat_all$year) - 2000)*1000
dat_all$SOSD <- dat_all$SOSD - (as.numeric(dat_all$year) - 2000)*1000
dat_all$MAXD <- dat_all$MAXD - (as.numeric(dat_all$year) - 2000)*1000


dat_all$EOSV <- dat_all$EOSV / 10000
dat_all$SOSV <- dat_all$SOSV / 10000
dat_all$MAXV <- dat_all$MAXV / 10000

dat_all[, c(9, 11, 13)][dat_all[, c(9, 11, 13)] > 3] <- NA
head(dat_all)

write.csv(dat_all, 'WekEO.csv', row.names = FALSE)


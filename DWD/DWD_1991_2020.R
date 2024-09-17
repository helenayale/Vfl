library(ggplot2)
library(rgdal)
library(gtools)
library(raster)
library(terra)

setwd('D:/GIS_Data/Vfl-oak/')

field_names <- c('Gei', 'Loh', 'Roh90', 'Roh620', 'Roh635')

dwd_img <- raster("D:/GIS_Data/Vfl-oak/DWD/raster_rr_1991x2020_jahr.asc")
dwd_img



# plot_i <- 1
# dwd_i <- 1
# eval(parse(text = paste('shp <- readOGR("D:/GIS_Data/Vfl-oak/Vfl-detail_parcels/',field_names[plot_i],'-par-wgs84.shp")', sep = '')))
# # eval(parse(text = paste('file_inv <-  mixedsort(list.files(path = "D:/GIS_Data/Vfl-oak/DWD/', DWD_names[dwd_i], '/", pattern = ".asc$", all.files = TRUE, full.names = FALSE))', sep = '')))
# # file_inv
# year <- 2017
# eval(parse(text = paste('dwd_img <- raster("D:/GIS_Data/Vfl-oak/DWD/',DWD_names[dwd_i],'/ggrids_germany__annual__phenology__',DWD_names[dwd_i],'__', year,'.asc")', sep = '')))
# dwd_img
# dwd <- terra::extract(dwd_img, shp, mean, na.rm=TRUE)
# dwd

for (plot_i in 1:length(field_names)){
  eval(parse(text = paste('shp <- readOGR("D:/GIS_Data/Vfl-oak/Vfl-detail_parcels/',field_names[plot_i],'-par-wgs84.shp")', sep = '')))
  
      img_prec <- raster("D:/GIS_Data/Vfl-oak/DWD/raster_rr_1991x2020_jahr.asc")
      img_temp <- raster("D:/GIS_Data/Vfl-oak/DWD/raster_tm_1991x2020_jahr.asc")
      shp <- spTransform(shp, CRS(proj4string(dwd_img)))
      dwd_prec <- terra::extract(img_prec, shp, FUN = table, na.rm=FALSE)
      dwd_temp <- terra::extract(img_temp, shp, FUN = table, na.rm=FALSE)
      par <- shp$Parcel
      field <- shp$Vfl_name
      interne<- shp$Interne
      
      treat <- shp$Treat_cat
      if (length(treat) == 0){
        treat <- 'Unknown'
      }
      
      for (i in 1:length(dwd_prec)){
        pix_num <- seq(1:length(dwd_prec[[i]]))
        dat_pix <- data.frame(ID = field_names[plot_i], Field = field[i], Parcel = i, Interne = interne[i], 
                              pix = pix_num, Treat = treat[i], prec = dwd_prec[[i]], temp = dwd_temp[[i]])
        
        if (i == 1){
          dat_img <- dat_pix
        }else{
          dat_img <- rbind(dat_img, dat_pix)
        }
      }
      
      if (plot_i == 1){
        dat_all <- dat_img
      }else{
        dat_all <- rbind(dat_all, dat_img)
      }
  
}

dat_all

write.csv(dat_all, 'DWD_1991_2020.csv', row.names = FALSE)

dat_climate <- read.csv('DWD_1991_2020.csv')
head(dat_climate)


ggplot(data = dat_climate) + geom_boxplot(aes(x = temp, color = ID, group = ID ))

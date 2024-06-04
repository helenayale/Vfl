library(ggplot2)
library(rgdal)
library(gtools)
library(raster)
library(terra)

setwd('D:/GIS_Data/Vfl-oak/')


field_names <- c('Gei', 'Loh', 'Roh90', 'Roh620', 'Roh635')
DWD_names <- c('STEBO', 'STEBV', 'STEF')
Ann_names <- c('AnnDI', 'AnnPrec', 'FrostDay', 'HotDay', 'HRain30', 'IceDay', 'SumDay')
S_names <- c('SDI', 'SPrec', 'SSunDur', 'STMax', 'STMean', 'STMin')


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
  for (year in 2017:2022){
    eval(parse(text = paste('shp <- readOGR("D:/GIS_Data/Vfl-oak/Vfl-detail_parcels/',field_names[plot_i],'-par-wgs84.shp")', sep = '')))
    for (dwd_i in 1:length(DWD_names)){
      folder_name <- DWD_names[dwd_i]
      eval(parse(text = paste('dwd_img <- raster("D:/GIS_Data/Vfl-oak/DWD/',folder_name,'/ggrids_germany__annual__phenology__',folder_name,'__', year,'.asc")', sep = '')))
      # shp <- spTransform(shp, CRS(proj4string(dwd_img)))
      dwd <- terra::extract(dwd_img, shp, FUN = table, na.rm=FALSE)
      par <- shp$Parcel
      field <- shp$Vfl_name
      interne<- shp$Interne
      
      treat <- shp$Treat_cat
      if (length(treat) == 0){
        treat <- 'Unknown'
      }
      
      for (i in 1:length(dwd)){
        pix_num <- seq(1:length(dwd[[i]]))
        dat_pix <- data.frame(ID = field_names[plot_i], Field = field[i], Parcel = i, Interne = interne[i], 
                              year = year, pix = pix_num, Treat = treat[i])
        eval(parse(text = paste('dat_pix$', folder_name, '<- dwd[[i]]', sep = '')))
        
        if (i == 1){
          dat_img <- dat_pix
        }else{
          dat_img <- rbind(dat_img, dat_pix)
        }
      }
      
      if (dwd_i == 1){
        new <- dat_img
      }else{
        eval(parse(text = paste('new$', folder_name, '<- dat_img$', folder_name, sep = '')))
      }
      
    }
    
    if(year == 2017){
      dat_plot <- new
    }else{
      dat_plot <- rbind(dat_plot, new)
    }
  }
  
  if(plot_i == 1){
    dat_all <- dat_plot
  }else{
    dat_all <- rbind(dat_all, dat_plot)
  }
  
}

dat_all

write.csv(dat_all, 'DWD_Pheno.csv', row.names = FALSE)

library(sp)
library(raster)
library(gtools)
library(sf)

library(terra)

library(TDPanalysis)

setwd('D:/GIS_Data/DWD/')

field_names <- c('Gei', 'Loh', 'Roh90', 'Roh620', 'Roh635')

DWD_names <- c('MTMean', 'MPrec', 'MSunD')
file_names <- c('TAMM', 'RSMS', 'SDMS')


for (yr in 2017:2024){
  print(yr)
  eval(parse(text = paste('img_03_temp <- raster("D:/GIS_Data/DWD/MTMean/TAMM_03_', yr, '_01.asc")', sep = '')))
  eval(parse(text = paste('img_04_temp <- raster("D:/GIS_Data/DWD/MTMean/TAMM_04_', yr, '_01.asc")', sep = '')))
  eval(parse(text = paste('img_05_temp <- raster("D:/GIS_Data/DWD/MTMean/TAMM_05_', yr, '_01.asc")', sep = '')))
  eval(parse(text = paste('img_06_temp <- raster("D:/GIS_Data/DWD/MTMean/TAMM_06_', yr, '_01.asc")', sep = '')))
  eval(parse(text = paste('img_09_temp <- raster("D:/GIS_Data/DWD/MTMean/TAMM_09_', yr, '_01.asc")', sep = '')))
  eval(parse(text = paste('img_10_temp <- raster("D:/GIS_Data/DWD/MTMean/TAMM_10_', yr, '_01.asc")', sep = '')))
  eval(parse(text = paste('img_03_prec <- raster("D:/GIS_Data/DWD/MPrec/RSMS_03_', yr, '_01.asc")', sep = '')))
  eval(parse(text = paste('img_04_prec <- raster("D:/GIS_Data/DWD/MPrec/RSMS_04_', yr, '_01.asc")', sep = '')))
  eval(parse(text = paste('img_05_prec <- raster("D:/GIS_Data/DWD/MPrec/RSMS_05_', yr, '_01.asc")', sep = '')))
  eval(parse(text = paste('img_06_prec <- raster("D:/GIS_Data/DWD/MPrec/RSMS_06_', yr, '_01.asc")', sep = '')))
  eval(parse(text = paste('img_09_prec <- raster("D:/GIS_Data/DWD/MPrec/RSMS_09_', yr, '_01.asc")', sep = '')))
  eval(parse(text = paste('img_10_prec <- raster("D:/GIS_Data/DWD/MPrec/RSMS_10_', yr, '_01.asc")', sep = '')))
  eval(parse(text = paste('img_03_sund <- raster("D:/GIS_Data/DWD/MSunD/SDMS_03_', yr, '_01.asc")', sep = '')))
  eval(parse(text = paste('img_04_sund <- raster("D:/GIS_Data/DWD/MSunD/SDMS_04_', yr, '_01.asc")', sep = '')))
  eval(parse(text = paste('img_05_sund <- raster("D:/GIS_Data/DWD/MSunD/SDMS_05_', yr, '_01.asc")', sep = '')))
  eval(parse(text = paste('img_06_sund <- raster("D:/GIS_Data/DWD/MSunD/SDMS_06_', yr, '_01.asc")', sep = '')))
  eval(parse(text = paste('img_09_sund <- raster("D:/GIS_Data/DWD/MSunD/SDMS_09_', yr, '_01.asc")', sep = '')))
  eval(parse(text = paste('img_10_sund <- raster("D:/GIS_Data/DWD/MSunD/SDMS_10_', yr, '_01.asc")', sep = '')))
  
  for (plot_i in 1:length(field_names)){
    print(field_names[plot_i])
    eval(parse(text = paste('shp <- st_read("D:/GIS_Data/Vfl-oak/Vfl-detail_parcels/', field_names[plot_i],'-par-wgs84.shp")', sep = '')))
    shp <- st_transform(shp, CRS(proj4string(img_03_temp)))
    par <- shp$Parcel
    field <- shp$Vfl_name
    interne<- shp$Interne
    temp_03 <- terra::extract(img_03_temp, shp, FUN = table, na.rm=FALSE)
    temp_04<- terra::extract(img_04_temp, shp, FUN = table, na.rm=FALSE)
    temp_05 <- terra::extract(img_05_temp, shp, FUN = table, na.rm=FALSE)
    temp_06 <- terra::extract(img_06_temp, shp, FUN = table, na.rm=FALSE)
    temp_09<- terra::extract(img_09_temp, shp, FUN = table, na.rm=FALSE)
    temp_10 <- terra::extract(img_10_temp, shp, FUN = table, na.rm=FALSE)

    prec_03 <- terra::extract(img_03_prec, shp, FUN = table, na.rm=FALSE)
    prec_04 <- terra::extract(img_04_prec, shp, FUN = table, na.rm=FALSE)
    prec_05 <- terra::extract(img_05_prec, shp, FUN = table, na.rm=FALSE)
    prec_06 <- terra::extract(img_06_prec, shp, FUN = table, na.rm=FALSE)
    prec_09 <- terra::extract(img_09_prec, shp, FUN = table, na.rm=FALSE)
    prec_10 <- terra::extract(img_10_prec, shp, FUN = table, na.rm=FALSE)
    
    sund_03 <- terra::extract(img_03_sund, shp, FUN = table, na.rm=FALSE)
    sund_04  <- terra::extract(img_04_sund, shp, FUN = table, na.rm=FALSE)
    sund_05 <- terra::extract(img_05_sund, shp, FUN = table, na.rm=FALSE)
    sund_06  <- terra::extract(img_06_sund, shp, FUN = table, na.rm=FALSE)
    sund_09 <- terra::extract(img_09_sund, shp, FUN = table, na.rm=FALSE)
    sund_10  <- terra::extract(img_10_sund, shp, FUN = table, na.rm=FALSE)

    
    if(length(temp_03) == 1){
      dat <- data.frame(ID = field_names[plot_i], Field = field, 
                        Parcel = par, Interne = interne, year = yr,
                        temp_03 = temp_03, temp_04 = temp_04, 
                        temp_05 = temp_05, temp_06 = temp_06,
                        temp_09 = temp_09, temp_10 = temp_10,
                        prec_03 = prec_03, prec_04 = prec_04, 
                        prec_05 = prec_05, prec_06 = prec_06,
                        prec_09 = prec_09, prec_10 = prec_10,
                        sund_03 = sund_03, sund_04 = sund_04, 
                        sund_05 = sund_05, sund_06 = sund_06,
                        sund_09 = sund_09, sund_10 = sund_10)
    }else{
      for (i in 1:length(temp_03)){
        dat_pix <- data.frame(ID = field_names[plot_i], Field = field[i], 
                              Parcel = par[i], Interne = interne[i], year = yr,
                              temp_03 = temp_03[[i]], temp_04 = temp_04[[i]], 
                              temp_05 = temp_05[[i]], temp_06 = temp_06[[i]],
                              temp_09 = temp_09[[i]], temp_10 = temp_10[[i]],
                              prec_03 = prec_03[[i]], prec_04 = prec_04[[i]], 
                              prec_05 = prec_05[[i]], prec_06 = prec_06[[i]],
                              prec_09 = prec_09[[i]], prec_10 = prec_10[[i]],
                              sund_03 = sund_03[[i]], sund_04 = sund_04[[i]], 
                              sund_05 = sund_05[[i]], sund_06 = sund_06[[i]],
                              sund_09 = sund_09[[i]], sund_10 = sund_10[[i]])
        if (i == 1){
          dat <- dat_pix
        }else{
          dat <- rbind(dat, dat_pix)
        }
      }
      
    }
    
    
    if (plot_i == 1){
      dat_plt <- dat
    }else{
      dat_plt <- rbind(dat_plt, dat)
    }
  }
  if (yr == 2017){
    dat_all <- dat_plt
  }else{
    dat_all <- rbind(dat_all, dat_plt)
  }
}

head(dat_all)


dat_month <- dat_all

list <- c('03','04','05','06','09','10')
for (i in 1:length(list)){
  n <- list[i]
  eval(parse(text = paste('dat_month$temp_', n, '<- dat_month$temp_', n, '/10', sep = '')))
}

head(dat_month)


write.csv(dat_month, 'DWD_month.csv', row.names = FALSE)
save(dat_month, file = 'dat_dwd_month.rda')


library(sp)
library(raster)
library(gtools)
library(sf)

library(terra)

library(TDPanalysis)

setwd('D:/GIS_Data/DWD/')

field_names <- c('Gei', 'Loh', 'Roh90', 'Roh620', 'Roh635')

DWD_names <- c('STMean', 'SPrec', 'SSunDur')
file_names <- c('TAMM', 'RSMS', 'SDMS')

Phe_names <- c('STEBO', 'STEBV')

for (yr in 2017:2024){
  print(yr)
  eval(parse(text = paste('img_spr_temp <- raster("D:/GIS_Data/DWD/STMean/TAMM_13_', yr, '_01.asc")', sep = '')))
  eval(parse(text = paste('img_sum_temp <- raster("D:/GIS_Data/DWD/STMean/TAMM_14_', yr, '_01.asc")', sep = '')))
  eval(parse(text = paste('img_aut_temp <- raster("D:/GIS_Data/DWD/STMean/TAMM_15_', yr, '_01.asc")', sep = '')))
  eval(parse(text = paste('img_win_chill <- raster("D:/GIS_Data/DWD/STMean/TAMM_16_', yr, '_01.asc")', sep = '')))
  eval(parse(text = paste('img_spr_prec <- raster("D:/GIS_Data/DWD/SPrec/RSMS_13_', yr, '_01.asc")', sep = '')))
  eval(parse(text = paste('img_aut_prec <- raster("D:/GIS_Data/DWD/SPrec/RSMS_15_', yr, '_01.asc")', sep = '')))
  eval(parse(text = paste('img_spr_sund <- raster("D:/GIS_Data/DWD/SSunDur/SDMS_13_', yr, '_01.asc")', sep = '')))
  eval(parse(text = paste('img_aut_sund <- raster("D:/GIS_Data/DWD/SSunDur/SDMS_15_', yr, '_01.asc")', sep = '')))
  eval(parse(text = paste('img_stebo <- raster("D:/GIS_Data/DWD/STEBO/ggrids_germany__annual__phenology__STEBO__', yr, '.asc")', sep = '')))
  eval(parse(text = paste('img_stebv <- raster("D:/GIS_Data/DWD/STEBV/ggrids_germany__annual__phenology__STEBV__', yr, '.asc")', sep = '')))
  eval(parse(text = paste('img_DI <- raster("D:/GIS_Data/DWD/SDI/DIMM_13_', yr, '_01.asc")', sep = '')))
  
  for (plot_i in 1:length(field_names)){
    print(field_names[plot_i])
    eval(parse(text = paste('shp <- st_read("D:/GIS_Data/Vfl-oak/Vfl-detail_parcels/', field_names[plot_i],'-par-wgs84.shp")', sep = '')))
    shp <- st_transform(shp, CRS(proj4string(img_spr_temp)))
    par <- shp$Parcel
    field <- shp$Vfl_name
    interne<- shp$Interne
    spr_temp <- terra::extract(img_spr_temp, shp, FUN = table, na.rm=FALSE)
    sum_temp <- terra::extract(img_sum_temp, shp, FUN = table, na.rm=FALSE)
    aut_temp <- terra::extract(img_aut_temp, shp, FUN = table, na.rm=FALSE)
    win_chill <- terra::extract(img_win_chill , shp, FUN = table, na.rm=FALSE)
    spr_prec <- terra::extract(img_spr_prec, shp, FUN = table, na.rm=FALSE)
    aut_prec <- terra::extract(img_aut_prec, shp, FUN = table, na.rm=FALSE)
    spr_sund <- terra::extract(img_spr_sund, shp, FUN = table, na.rm=FALSE)
    aut_sund  <- terra::extract(img_aut_sund, shp, FUN = table, na.rm=FALSE)
    stebo <- terra::extract(img_stebo, shp, FUN = table, na.rm=FALSE)
    stebv <- terra::extract(img_stebv, shp, FUN = table, na.rm=FALSE)
    DI <- terra::extract(img_DI, shp, FUN = table, na.rm=FALSE)
    
    if(length(spr_temp) == 1){
      dat <- data.frame(ID = field_names[plot_i], Field = field, 
                        Parcel = par, Interne = interne, year = yr,
                        spr_temp = spr_temp, sum_temp = sum_temp, 
                        aut_temp = aut_temp, win_chill = win_chill,
                        spr_prec = spr_prec, aut_prec = aut_prec,
                        spr_sund = spr_sund, aut_sund = aut_sund,
                        stebo = stebo, stebv = stebv, spr_DI = DI)
    }else{
      for (i in 1:length(spr_temp)){
        dat_pix <- data.frame(ID = field_names[plot_i], Field = field[i], 
                          Parcel = par[i], Interne = interne[i], year = yr,
                          spr_temp = spr_temp[[i]], sum_temp = sum_temp[[i]], 
                          aut_temp = aut_temp[[i]], win_chill = win_chill[[i]],
                          spr_prec = spr_prec[[i]], aut_prec = aut_prec[[i]],
                          spr_sund = spr_sund[[i]], aut_sund = aut_sund[[i]],
                          stebo = stebo[[i]], stebv = stebv[[i]], spr_DI = DI[[i]])
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
dat_dwd <- dat_all

dat_dwd$spr_temp <- dat_dwd$spr_temp/10
dat_dwd$sum_temp <- dat_dwd$sum_temp/10
dat_dwd$aut_temp <- dat_dwd$aut_temp/10
dat_dwd$win_chill <- dat_dwd$win_chill/10
head(dat_dwd)


write.csv(dat_dwd, 'DWD_2024.csv', row.names = FALSE)
save(dat_dwd, file = 'dat_dwd_2024.rda')


library(sp)
library(rgdal)
library(raster)
library(gtools)
library(sf)

library(terra)

library(TDPanalysis)

setwd('D:/GIS_Data/Vfl-oak/SC_CDF_0.75/')
field_names <- c('Gei', 'Loh', 'Roh90', 'Roh620', 'Roh635')

####################
### extract data ###
####################

for (plot_i in 1:length(field_names)){
  print(field_names[plot_i])
  eval(parse(text = paste('shp <- readOGR("D:/GIS_Data/Vfl-oak/Vfl-detail_parcels/',field_names[plot_i],'-par-wgs84.shp")', sep = '')))
  for (year in 2017:2021){
    eval(parse(text = paste('file_inv <-  mixedsort(list.files(path = "', field_names[plot_i], '_', year, '/", pattern = ".tif$", all.files = TRUE, full.names = FALSE))', sep = '')))
    
    for (img_i in 1:length(file_inv)){
      print(paste('Img',img_i))
      img <- brick(paste(field_names[plot_i], '_', year, '/',file_inv[img_i], sep = ''))
      n_layers <- nlayers(img)
      date_start <- 1
      int <- 1
      yr <- substr(file_inv[img_i],date_start,date_start+3)
      month <- substr(file_inv[img_i],date_start+3+int+1,date_start+3+int+2)
      day <- substr(file_inv[img_i],date_start+5+int*2+1,date_start+5+int*2+2)
      ymd_date <- paste(yr, '/',month,'/',day, sep = '')
      date <- date.to.DOY(ymd_date, format = "yyyy/mm/dd")
      
      
      blue <- img[['B2']]
      green <- img[['B3']]
      red <- img[['B4']]
      RE1 <- img[['B5']]
      RE2 <- img[['B6']]
      RE3 <- img[['B7']]
      nir <- img[['B8']]
      RE4 <- img[['B8A']]
      SWIR1 <- img[['B11']]
      SWIR2 <- img[['B12']]
      
      NDVI <-  (nir - red)/(nir + red)
      
      
      shp <- spTransform(shp, CRS(proj4string(NDVI)))
      # t <- terra::extract(NDVI, shp, table, na.rm=TRUE)
      
      par <- shp$Parcel
      field <- shp$Vfl_name
      interne<- shp$Interne
      
      treat <- shp$Treat_cat
      if (length(treat) == 0){
        treat <- 'Unknown'
      }
      
      blue_m <- terra::extract(blue, shp, mean, na.rm=TRUE)
      green_m <- terra::extract(green, shp, mean, na.rm=TRUE)
      red_m <- terra::extract(red, shp, mean, na.rm=TRUE)
      RE1_m <- terra::extract(RE1, shp, mean, na.rm=TRUE)
      RE2_m <- terra::extract(RE2, shp, mean, na.rm=TRUE)
      RE3_m <- terra::extract(RE3, shp, mean, na.rm=TRUE)
      nir_m <- terra::extract(nir, shp, mean, na.rm=TRUE)
      RE4_m <- terra::extract(RE4, shp, mean, na.rm=TRUE)
      SWIR1_m <- terra::extract(SWIR1, shp, mean, na.rm=TRUE)
      SWIR2_m <- terra::extract(SWIR2, shp, mean, na.rm=TRUE)
      
      
      NDVI_sd <- terra::extract(NDVI, shp, sd, na.rm=TRUE)
      NDVI_m <- terra::extract(NDVI, shp, mean, na.rm=TRUE)
      NDVI_cv <- NDVI_sd/abs(NDVI_m)*100
      NDVI_med <- terra::extract(NDVI, shp, median, na.rm=TRUE)
      
      new <- data.frame(ID = field_names[plot_i], Field = field, Parcel = par, Interne = interne,
                        year = yr, month = month, day = day, DOY = date, Treat = treat,
                        blue = blue_m, green = green_m, red = red_m, nir = nir_m,
                        RE1 = RE1_m, RE2 = RE2_m, RE3 = RE3_m, RE4 = RE4_m, SWIR1 = SWIR1_m, SWIR2 = SWIR2_m, 
                        NDVI = NDVI_m, Median = NDVI_med, SD = NDVI_sd, CV = NDVI_cv)
      
      if (img_i == 1){
        dat_year <- new
      }else{
        dat_year <- rbind(dat_year, new)
      }
    }
    dat_year <- na.omit(dat_year)
    # eval(parse(text = paste('write.csv(dat_year, "', field_names[plot_i], year,'.csv", row.names = FALSE)', sep = '')))
    if(year == 2017){
      dat_plot <- dat_year
    }else{
      dat_plot <- rbind(dat_plot, dat_year)
    }
  }
  dat_plot <- na.omit(dat_plot)
  eval(parse(text = paste('write.csv(dat_plot, "', field_names[plot_i],'.csv", row.names = FALSE)', sep = '')))
  
  if(plot_i == 1){
    dat_all <- dat_plot
  }else{
    dat_all <- rbind(dat_all, dat_plot)
  }
  
}

dat_all <- na.omit(dat_all)
dat_all

write.csv(dat_all, 'Dat_all.csv', row.names = FALSE)
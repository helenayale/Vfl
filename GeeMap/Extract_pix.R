library(sp)
library(rgdal)
library(raster)
library(gtools)
library(sf)

library(terra)

library(TDPanalysis)

setwd('D:/GIS_Data/Vfl-oak/GEEMap/')
field_names <- c('Gei', 'Loh', 'Roh90', 'Roh620', 'Roh635')
folder_names <- c('Gei', 'Loh', 'Roh', 'Roh', 'Roh')

####################
### extract data ###
####################

for (plot_i in 1:length(field_names)){
  print(field_names[plot_i])
  field_n <- field_names[plot_i]
  folder_n <- folder_names[plot_i]
  eval(parse(text = paste('shp <- readOGR("D:/GIS_Data/Vfl-oak/Vfl-detail_parcels/', field_n,'-par-wgs84.shp")', sep = '')))
  eval(parse(text = paste('file_inv <-  mixedsort(list.files(path = "D:/GIS_Data/Vfl-oak/GEEMap/S2_cld_free/', folder_n,'/", pattern = ".tif$", all.files = TRUE, full.names = FALSE))', sep = '')))
  
  for (img_i in 1:length(file_inv)){
    print(paste('Img',img_i))
    img <- brick(paste('D:/GIS_Data/Vfl-oak/GEEMap/S2_cld_free/', folder_n, '/', file_inv[img_i], sep = ''))
    n_layers <- nlayers(img)
    date_start <- 1
    int <- 0
    year <- substr(file_inv[img_i],date_start,date_start+3)
    month <- substr(file_inv[img_i],date_start+3+int+1,date_start+3+int+2)
    day <- substr(file_inv[img_i],date_start+5+int*2+1,date_start+5+int*2+2)
    ymd_date <- paste(year, '/',month,'/',day, sep = '')
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
    
    NDVI_t <- terra::extract(NDVI, shp, FUN = table, na.rm=FALSE)
    blue_t <- terra::extract(blue, shp, FUN = table, na.rm=FALSE)
    green_t <- terra::extract(green, shp, FUN = table, na.rm=FALSE)
    red_t <- terra::extract(red, shp, FUN = table, na.rm=FALSE)
    RE1_t <- terra::extract(RE1, shp, FUN = table, na.rm=FALSE)
    RE2_t <- terra::extract(RE2, shp, FUN = table, na.rm=FALSE)
    RE3_t <- terra::extract(RE3, shp, FUN = table, na.rm=FALSE)
    nir_t <- terra::extract(nir, shp, FUN = table, na.rm=FALSE)
    RE4_t <- terra::extract(RE4, shp, FUN = table, na.rm=FALSE)
    SWIR1_t <- terra::extract(SWIR1, shp, FUN = table, na.rm=FALSE)
    SWIR2_t <- terra::extract(SWIR2, shp, FUN = table, na.rm=FALSE)
    
    
    
    for (i in 1:length(NDVI_t)){
      pix_num <- seq(1:length(NDVI_t[[i]]))
      dat_pix <- data.frame(ID = field_names[plot_i], Field = field[i], Parcel = i, Interne = interne[i], 
                            pix = pix_num, year = year, month = month, day = day, DOY = date, Treat = treat[i],
                            NDVI = NDVI_t[[i]], blue = blue_t[[i]], green = green_t[[i]], red = red_t[[i]], 
                            RE1 = RE1_t[[i]], RE2 = RE2_t[[i]], RE3 = RE3_t[[i]], nir = nir_t[[i]], RE4 = RE4_t[[i]],
                            SWIR1 = SWIR1_t[[i]], SWIR2 = SWIR2_t[[i]])
      eval(parse(text = paste('dat_', field_names[plot_i], plot_i, '_', year, '<- dat_pix', sep = '')))
      eval(parse(text = paste('dat_', field_names[plot_i], plot_i, '_', year, '<- rbind(dat_', field_names[plot_i], plot_i, '_', year, ', dat_pix)', sep = '')))
      
      if (i == 1){
        dat_img <- dat_pix
      }else{
        dat_img <- rbind(dat_img, dat_pix)
      }
    }
    
    if (img_i == 1){
      dat_plot <- dat_img
    }else{
      dat_plot <- rbind(dat_plot, dat_img)
    }
  }
  
  eval(parse(text = paste('write.csv(dat_plot, "', field_names[plot_i],'_pix.csv", row.names = FALSE)', sep = '')))
  
  if(plot_i == 1){
    dat_all <- dat_plot
  }else{
    dat_all <- rbind(dat_all, dat_plot)
  }
  
}

# dat_all <- na.omit(dat_all)
dat_all

write.csv(dat_all, 'Dat_all_pix.csv', row.names = FALSE)

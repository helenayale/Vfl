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

####################
### extract data ###
####################

for (plot_i in 1:length(field_names)){
  print(field_names[plot_i])
  field_n <- field_names[plot_i]
  folder_n <- folder_names[plot_i]
  eval(parse(text = paste('shp <- st_read("D:/GIS_Data/Vfl-oak/Vfl-detail_parcels/', field_n,'-par-wgs84.shp")', sep = '')))
  eval(parse(text = paste('file_inv <-  mixedsort(list.files(path = "D:/GIS_Data/Vfl-oak/WekEO/', folder_n,'/PPI/", pattern = ".tif$", all.files = TRUE, full.names = FALSE))', sep = '')))
  
  for (img_i in 1:length(file_inv)){
    #print(paste('Img',img_i))
    img <- brick(paste('D:/GIS_Data/Vfl-oak/WekEO/', folder_n, '/PPI/', file_inv[img_i], sep = ''))
    # n_layers <- nlayers(img)
    date_start <- 4
    int <- 0
    year <- substr(file_inv[img_i],date_start,date_start+3)
    month <- substr(file_inv[img_i],date_start+3+int+1,date_start+3+int+2)
    day <- substr(file_inv[img_i],date_start+5+int*2+1,date_start+5+int*2+2)
    ymd_date <- paste(year, '/',month,'/',day, sep = '')
    date <- date.to.DOY(ymd_date, format = "yyyy/mm/dd")
    print(ymd_date)
    
    shp <- st_transform(shp, st_crs(img))
    # t <- terra::extract(NDVI, shp, table, na.rm=TRUE)
    
    par <- shp$Parcel
    field <- shp$Vfl_name
    interne<- shp$Interne
    
    treat <- shp$Treat_cat
    if (length(treat) == 0){
      treat <- 'Unknown'
    }
    
    PPI <- terra::extract(img, shp, FUN = table, na.rm=FALSE)
    
    
    for (i in 1:length(PPI)){
      pix_num <- seq(1:length(PPI[[i]]))
      dat_pix <- data.frame(ID = field_names[plot_i], Field = field[i], Parcel = i, Interne = interne[i], 
                            pix = pix_num, year = year, month = month, day = day, DOY = date, PPI = PPI[[i]])
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
  
  eval(parse(text = paste('write.csv(dat_plot, "', field_names[plot_i],'_PPI_pix.csv", row.names = FALSE)', sep = '')))
  
  if(plot_i == 1){
    dat_all <- dat_plot
  }else{
    dat_all <- rbind(dat_all, dat_plot)
  }
  
}

# dat_all <- na.omit(dat_all)
head(dat_all)

colnames(dat_all) <- c('ID', 'Field', 'Parcel', 'Interne', 'pix', 'year', 'month', 'day', 'DOY', 'PPI')
head(dat_all)

write.csv(dat_all, 'Dat_PPI_pix.csv', row.names = FALSE)

save(dat_all, file = 'dat_PPI.rda')

load('dat_PPI.rda')
library(dplyr)
# max_by_year <- dat_all %>%
#   group_by(year) %>%
#   filter(PPI == max(PPI)) %>%
#   select(year, DOY, PPI) %>%
#   arrange(year) %>%
#   ungroup()
# print(max_by_year)

sort(unique(dat_all$DOY))
annual_dates <- aggregate(DOY ~ year, data = dat_all, 
                          FUN = function(x) sort(unique(x)))
print(annual_dates)

# select DOY on beginning/ending of a month
# select from DOY 1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335
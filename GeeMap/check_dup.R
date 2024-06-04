library(stats)
library(TDPanalysis)

setwd('D:/GIS_Data/Vfl-oak/GEEMap/')
field_names <- c('Gei', 'Loh', 'Roh90', 'Roh620', 'Roh635')

dat_all <- read.csv('Dat_all_pix.csv')
head(dat_all)

# dat_all <- subset(dat_all, NDVI > 0.2)

nir <- dat_all$nir
red <- dat_all$red
blue <- dat_all$blue
dat_all$EVI <- 2.5*((nir - red)/(nir + 6*red - 7.5*blue) + 1)

### check duplicates
# yr = 2022
# plot_i = 3
# plot_n <- field_names[plot_i]
# par_i = 3
# pix_i = 25

for (plot_i in 1:length(field_names)){
  plot_n <- field_names[plot_i]
  sub_plot <- subset(dat_all, ID == plot_n)
  num_par <- max(sub_plot$Parcel)
  for (par_i in 1:num_par){
    sub_par <- subset(sub_plot, Parcel == par_i)
    num_pix <- max(sub_par$pix)
    for (pix_i in 1:num_pix){
      sub_pix <- subset(sub_par, pix == pix_i)
      for (yr in 2017:2022){
        sub_year <- subset(sub_pix, year == yr)
        sub_year$dup <- duplicated(sub_year$DOY)
        sub_dup <- subset(sub_year, dup == TRUE)
        
        if (nrow(sub_dup) != 0){
          for (i in 1:nrow(sub_dup)){
            d_day <- sub_dup[i, 'DOY']
            sub_dup_day <- subset(sub_year, DOY == d_day)
            new_blue <- mean(sub_dup_day$blue)
            new_green <- mean(sub_dup_day$green)
            new_red <- mean(sub_dup_day$red)
            new_RE1 <- mean(sub_dup_day$RE1)
            new_RE2<- mean(sub_dup_day$RE2)
            new_RE3 <- mean(sub_dup_day$RE3)
            new_nir <- mean(sub_dup_day$nir)
            new_RE4 <- mean(sub_dup_day$RE4)
            new_SWIR1 <- mean(sub_dup_day$SWIR1)
            new_SWIR2 <- mean(sub_dup_day$SWIR12)
            
            new_NDVI <- (new_nir - new_red)/(new_nir + new_red)
            new_EVI <- 2.5*((new_nir - new_red)/(new_nir + 6*new_red - 7.5*new_blue) + 1)
            
            sub_new <- subset(sub_year, dup == FALSE)
            # sub_new[which(sub_new$DOY == d_day), "NDVI"] <- new_NDVI
            
            list_band <- c('blue', 'green', 'red', 'RE1', 'RE2', 'RE3', 'nir', 'RE4', 'SWIR1', 'SWIR2', 'NDVI', 'EVI')
            for (j in 1:length(list_band)){
              band_n <- list_band[j]
              eval(parse(text = paste('sub_new[which(sub_new$DOY == d_day), "', band_n, '"] <- new_', band_n, sep = '')))
            }
          }
          sub_new <- sub_new[, 1:(ncol(sub_new)-1)]
        }else{
          sub_new <- sub_year[, 1:(ncol(sub_year)-1)]
        }
        if (yr == 2017){
          dat_year <- sub_new
        }else{
          dat_year <- rbind(dat_year, sub_new)
        }
      }
      if (pix_i == 1){
        dat_pix <- dat_year
      }else{
        dat_pix <- rbind(dat_pix, dat_year)
      }
    }
    if (par_i == 1){
      dat_par <- dat_pix
    }else{
      dat_par <- rbind(dat_par, dat_pix)
    }
  }
  if (plot_i == 1){
    dat_plot <- dat_par
  }else{
    dat_plot <- rbind(dat_plot, dat_par)
  }
}

dat_plot

write.csv(dat_plot, 'Dat_all_pix_dc.csv', row.names = FALSE)

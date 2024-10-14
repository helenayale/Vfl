library(ggplot2)
library(phenex)

setwd('D:/GIS_Data/Vfl-oak/GEEMap/')
field_names <- c('Gei', 'Loh', 'Roh90', 'Roh620', 'Roh635')

dat_all <- read.csv('Dat_int_pix.csv')
head(dat_all)


for (plot_i in 1 : length(field_names)){
  plot <- field_names[plot_i]
  sub_plot <- subset(dat_all, ID == plot)
  for (par_i in 1: max(sub_plot$Parcel)){
    sub_par <- subset(sub_plot, Parcel == par_i)
    for (yr in 2017: 2022){
      # if (year == 2020){
      #   doy_max <- 366
      # }else{
      #   doy_max <- 365
      # }
      sub_year <- subset(sub_par, year == yr)
      for (pix_i in 1:max(sub_year[,'pix'])){
        sub_pix <- subset(sub_year, pix == pix_i)
        ndvi <- modelNDVI(sub_pix$NDVI, year.int = yr, 
                          correction = "none", method = "DLogistic",  MARGIN = 2, 
                          doParallel = FALSE, slidingperiod = 40)
        
        D90 <- ndvi[[1]]@'modelledValues'[90]
        D106 <- ndvi[[1]]@'modelledValues'[106]
        D120 <- ndvi[[1]]@'modelledValues'[120]
        D168 <- ndvi[[1]]@'modelledValues'[168]
        D182 <- ndvi[[1]]@'modelledValues'[182]
        D278 <- ndvi[[1]]@'modelledValues'[278]
        D292 <- ndvi[[1]]@'modelledValues'[292]
        D306 <- ndvi[[1]]@'modelledValues'[306]
        
        
        new <- data.frame(plot = plot, parcel = par_i, year = yr, pix = pix_i,
                          D90 = D90, D106 = D106, D120 = D120, D168 = D168, D182 = D182, D278 = D278, D292 = D292, D306 = D306)
        
        if (pix_i == 1){
          dat_pix <- new
        }else{
          dat_pix <- rbind(dat_pix, new)
        }
      }
      if (yr == 2017){
        dat_yr <- dat_pix
      }else{
        dat_yr <- rbind(dat_yr, dat_pix)
      }
    }
    if (par_i == 1){
      dat_p <- dat_yr
    }else{
      dat_p <- rbind(dat_p, dat_yr)
    }
  }
  if (plot_i == 1){
    dat_final <- dat_p
  }else{
    dat_final <- rbind(dat_final, dat_p)
  }
}

dat_final

write.csv(dat_final, 'dat_fix_day_DL_orig.csv', row.names = FALSE)

dat_pheno_DL <- read.csv('dat_fix_day_DL_orig.csv')
head(dat_pheno_DL)

dat_pheno_DL <- dat_pheno_DL[!duplicated(dat_pheno_DL), ]

write.csv(dat_pheno_DL, 'dat_fix_day_DL.csv', row.names = FALSE)




for (plot_i in 1 : length(field_names)){
  plot <- field_names[plot_i]
  sub_plot <- subset(dat_all, ID == plot)
  for (par_i in 1: max(sub_plot$Parcel)){
    sub_par <- subset(sub_plot, Parcel == par_i)
    for (yr in 2017: 2022){
      sub_year <- subset(sub_par, year == yr)
      for (pix_i in 1:max(sub_year[,'pix'])){
        sub_pix <- subset(sub_year, pix == pix_i)
        if (yr == 2017){
          dat <- na.omit(sub_pix)
          first_day <- dat[1, 'DOY']
          first_NDVI <- dat[1, 'NDVI']
          
          # sub_pix[first_day, 'NDVI']
          sub_pix[1, 'NDVI'] <- first_NDVI
          sub_pix[as.integer(first_day)/2, 'NDVI'] <- first_NDVI
          sub_pix[as.integer(first_day)/3, 'NDVI'] <- first_NDVI
          # sub_pix[as.integer(first_day)/4, 'NDVI'] <- first_NDVI
        }
        
        
        ndvi <- modelNDVI(sub_pix$NDVI, year.int = yr, 
                          correction = "none", method = "SavGol", smoothing = 10, window.sav = 7, 
                          doParallel = FALSE, slidingperiod = 40)
        
        D90 <- ndvi[[1]]@'modelledValues'[90]
        D106 <- ndvi[[1]]@'modelledValues'[106]
        D120 <- ndvi[[1]]@'modelledValues'[120]
        D168 <- ndvi[[1]]@'modelledValues'[168]
        D182 <- ndvi[[1]]@'modelledValues'[182]
        D278 <- ndvi[[1]]@'modelledValues'[278]
        D292 <- ndvi[[1]]@'modelledValues'[292]
        D306 <- ndvi[[1]]@'modelledValues'[306]
        
        
        new <- data.frame(plot = plot, parcel = par_i, year = yr, pix = pix_i,
                          D90 = D90, D106 = D106, D120 = D120, D168 = D168, D182 = D182, D278 = D278, D292 = D292, D306 = D306)
        
        if (pix_i == 1){
          dat_pix <- new
        }else{
          dat_pix <- rbind(dat_pix, new)
        }
      }
      if (yr == 2017){
        dat_yr <- dat_pix
      }else{
        dat_yr <- rbind(dat_yr, dat_pix)
      }
    }
    if (par_i == 1){
      dat_p <- dat_yr
    }else{
      dat_p <- rbind(dat_p, dat_yr)
    }
  }
  if (plot_i == 1){
    dat_final <- dat_p
  }else{
    dat_final <- rbind(dat_final, dat_p)
  }
}

dat_final

write.csv(dat_final, 'dat_fix_day_SG.csv', row.names = FALSE)


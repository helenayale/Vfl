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
      sub_year <- subset(sub_par, year == yr)
      for (pix_i in 1:max(sub_year[,'pix'])){
        sub_pix <- subset(sub_year, pix == pix_i)
        ndvi <- modelNDVI(sub_pix$NDVI, year.int = yr, 
                          correction = "none", method = "DLogistic",  MARGIN = 2, 
                          doParallel = FALSE, slidingperiod = 40)
        sos2 <- phenoPhase(ndvi[[1]], phase="greenup", method="local", threshold=0.25, n=1000)
        eos2 <- phenoPhase(ndvi[[1]], phase="senescence", method="local", threshold=0.85, n=1000)
        sos <- phenoPhase(ndvi[[1]], phase="greenup", method="local", threshold=0.5, n=1000)
        eos <- phenoPhase(ndvi[[1]], phase="senescence", method="local", threshold=0.70, n=1000)
        
        los <- eos$mean - sos$mean
        los2  <- eos2$mean - sos2$mean
        
        sos_v <- ndvi[[1]]@'modelledValues'[sos$mean]
        eos_v <- ndvi[[1]]@'modelledValues'[eos$mean]
        sos2_v <- ndvi[[1]]@'modelledValues'[sos2$mean]
        eos2_v <- ndvi[[1]]@'modelledValues'[eos2$mean]
        max_ndvi <- max(ndvi[[1]]@'values', na.rm = TRUE)
        max_day <- which.max(ndvi[[1]]@'values')
        new <- data.frame(plot = plot, parcel = par_i, year = yr, pix = pix_i, sos = sos$mean, sos_mod_v = sos_v, eos = eos$mean, eos_mod_v = eos_v, los = los,
                          sos2 = sos2$mean, sos2_mod_v = sos2_v, eos2 = eos2$mean, eos2_mod_v = eos2_v, los2 = los2, max = max_ndvi, max_day = max_day)
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


write.csv(dat_final, 'dat_pheno_pix_DL_orig.csv', row.names = FALSE)

dat_pheno_DL <- read.csv('dat_pheno_pix_DL_orig.csv')
head(dat_pheno_DL)

dat_pheno_DL <- dat_pheno_DL[!duplicated(dat_pheno_DL), ]

write.csv(dat_pheno_DL, 'dat_pheno_pix_DL.csv', row.names = FALSE)


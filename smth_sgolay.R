library(stats)
library(TDPanalysis)
library(gsignal)
library(pracma)

setwd('D:/GIS_Data/Vfl-oak/SC_CDF_0.75/')
field_names <- c('Gei', 'Loh', 'Roh90', 'Roh620', 'Roh635')

#################
### smoothing ###
#################

dat_all <- read.csv('Dat_all_pix.csv')
head(dat_all)

### interpolation + smoothing function ###

smth <- function(dat, doy_col, dat_col, fl){
  dat_orig <- data.frame(DOY = dat[,doy_col], orig = dat[,dat_col])
  doy_min <- min(dat_orig[,'DOY'])
  doy_max <- max(dat_orig[,'DOY'])
  doy_new <- seq(from = doy_min, to = doy_max, by = 1)
  dat_smth <- data.frame(DOY = doy_new)
  dat_smth <- merge(dat_smth, dat_orig, by = 'DOY', all = TRUE)
  inter <- approx (dat_smth[, 'orig'], y = NULL, method = "linear", n = nrow(dat_smth), ties = mean)
  dat_smth$inter <- inter$y
  
  smoothed <- savgol(dat_smth$inter, fl, forder = 2, dorder = 0)
  dat_smth$smth <- smoothed
  
  return(dat_smth)
  # $DOY, $orig, $inter, $smth
}

### int + smth ###

band_names <- c('blue','green', 'red', 'nir', 'RE1', 'RE2', 'RE3', 'RE4', 'SWIR1', 'SWIR2', 'NDVI')
span_list <- c(31, 51, 71, 91, 111, 131, 151)
year_list <- seq(2017, 2021)

for (plot_i in 1:length(field_names)){
  sub_plot <- subset(dat_all, ID == field_names[plot_i])
  for (par_i in 1:max(sub_plot[, 'Parcel'])){
    sub_par <- subset(sub_plot, Parcel == par_i)
    for (year_i in 1:length(year_list)){
      sub_yr <- subset(sub_par, year == year_list[year_i])
      year <- year_list[year_i]
      for (pix_i in 1:max(sub_yr[,'pix'])){
        sub_pix <- subset(sub_yr, pix == pix_i)
        for (band_i in 1:length(band_names)){
          for (span_i in 1:length(span_list)){
            col_name <- paste(band_names[band_i], span_list[span_i], sep = '')
            sub_smth <- smth(sub_pix, 'DOY', band_names[band_i], span_list[span_i])
            if (span_i == 1){
              new <- data.frame(DOY = sub_smth$DOY)
              eval(parse(text = paste('new$',band_names[band_i], '<- sub_smth$orig', sep = '')))
              eval(parse(text = paste('new$',band_names[band_i], '_int', '<- sub_smth$inter', sep = '')))
              eval(parse(text = paste('new$',col_name, '<- sub_smth$smth', sep = '')))
            }else{
              eval(parse(text = paste('new$',col_name, '<- sub_smth$smth', sep = '')))
            }
          }
          if (band_i == 1){
            dat_pix <- data.frame(ID = field_names[plot_i], Parcel = par_i, year = year, pix = pix_i, DOY = new$DOY)
            dat_pix <- cbind(dat_pix, new[, -1])
          }else{
            dat_pix <- cbind(dat_pix, new[, -1])
          }
        }
        if (pix_i == 1){
          dat_yr <- dat_pix
        }else{
          dat_yr <- rbind(dat_yr, dat_pix)
        }
      }
      
      if (year == 2017){
        eval(parse(text = paste('dat_', field_names[plot_i], par_i, '<- dat_yr', sep = '')))
      }else{
        eval(parse(text = paste('dat_', field_names[plot_i], par_i, '<- rbind(dat_', field_names[plot_i], par_i, ', dat_yr)', sep = '')))
      }
    }
    if (par_i == 1){
      eval(parse(text = paste('dat_', field_names[plot_i], '<- dat_', field_names[plot_i], par_i, sep = '')))
    }else{
      eval(parse(text = paste('dat_', field_names[plot_i], '<- rbind(dat_', field_names[plot_i], ', dat_', field_names[plot_i], par_i, ')', sep = '')))
    }
  }
  if (plot_i == 1){
    dat_all_s <- dat_Gei
  }else{
    eval(parse(text = paste('dat_all_s <- rbind(dat_all_s, dat_', field_names[plot_i], ')', sep = '')))
  }
}

dat_all_s

write.csv(dat_all_s, 'Dat_smth_pix_SG.csv', row.names = FALSE)

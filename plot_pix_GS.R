library(ggplot2)

setwd('D:/GIS_Data/Vfl-oak/SC_CDF_0.75/')
field_names <- c('Gei', 'Loh', 'Roh90', 'Roh620', 'Roh635')


dat_all_pix <- read.csv('Dat_smth_pix_SG.csv')
head(dat_all_pix)

########################################
# catagorize data into plots and years #
########################################

for (plot_i in 1:length(field_names)){
  plot_name <- field_names[plot_i]
  eval(parse(text = paste('dat_', plot_name, '<- subset(dat_all_pix, ID == plot_name)', sep = '')))
  for (year in 2017:2021){
    eval(parse(text = paste('dat_', plot_name, '_', year, '<- subset(dat_', plot_name, ', year ==', year, ')', sep = '')))
    eval(parse(text = paste('par_num <- max(dat_', plot_name, '[, "Parcel"])', sep = '')))
    for (par_i in 1:par_num){
      eval(parse(text = paste('dat_', plot_name, '_', year, '_', par_i, '<- subset(dat_', plot_name, '_', year, ', Parcel == par_i)', sep = '')))
    }
  }
}



#############################################
# plot and compare span for different bands #
#############################################

group <- 'Gei_2021_1'
band_names <- c('NDVI')

for (band_i in 1:length(band_names)){
  band <- band_names[band_i]
  eval(parse(text = paste('dat_', band, ' <- dat_', group, '[, c("DOY", "pix", "', band, '_int", "', band, '51","', band, '71")]', sep = '')))
  eval(parse(text = paste('int <- data.frame(DOY = dat_', band, '$DOY, pix = dat_', band, '$pix,', band, '= dat_', band, '$', band, '_int, fl = 0)', sep = '')))
  eval(parse(text = paste('band51 <- data.frame(DOY = dat_', band, '$DOY, pix = dat_', band, '$pix,', band, '= dat_', band, '$', band, '51, fl = 51)', sep = '')))
  eval(parse(text = paste('band71 <- data.frame(DOY = dat_', band, '$DOY, pix = dat_', band, '$pix,', band, '= dat_', band, '$', band, '71, fl = 71)', sep = '')))
  eval(parse(text = paste(band, '_comp <- rbind(int, band51,  band71)', sep = '')))
}


ggplot(data = NDVI_comp, aes(x = DOY, y = NDVI, group = fl, color = fl)) + geom_line()  + facet_wrap(~ pix)


head(NDVI_comp)
dat_plot <- subset(dat_Gei_2017_1, pix == 1)
head(dat_plot)

ggplot(data = dat_plot, aes(x = DOY, y = NDVI_int)) + geom_point()

library(ggplot2)

setwd('D:/GIS_Data/Vfl-oak/GEEMap/')
field_names <- c('Gei', 'Loh', 'Roh90', 'Roh620', 'Roh635')

dat_all <- read.csv('Dat_int_pix.csv')
head(dat_all)

library(phenex)

yr = 2020
plot_i <- 1
plot <- field_names[plot_i]
par_i <- 1
pix_i <- 1

dat_plot <- subset(dat_all, ID == plot)
dat_par <- subset(dat_plot, Parcel == par_i)
dat_pix <- subset(dat_par, pix == pix_i)
dat_year <- subset(dat_pix, year == yr)

ndvi <- modelNDVI(dat_year$NDVI, year.int = yr, 
                  correction = "none", method = "DLogistic", MARGIN = 2, 
                  doParallel = FALSE, slidingperiod = 40)
ndvi
plot(ndvi[[1]])

typeof(ndvi)
class(ndvi[[1]])

ndvi[[1]]@'values'
ndvi[[1]]@'modelledValues'

sos <- phenoPhase(ndvi[[1]], phase="greenup", method="local", threshold=0.50, n=1000)
eos <- phenoPhase(ndvi[[1]], phase="senescence", method="local", threshold=0.70, n=1000)

sos$mean

met_list <- c('LinIP', 'Spline', 'DSig', 'DSigC', 'DLogistic', 'Gauss', 'GaussMix', 'Growth', 'FFT', 'SavGol')


for (yr in 2017:2022){
  if (yr == 2020){
    doy <- seq(1:366)
  }else{
    doy <- seq(1:365)
  }
  dat_year <- subset(dat_pix, year == yr)
  for (met_i in 1:length(met_list)){
    met <- met_list[met_i]
    ndvi <- modelNDVI(dat_year$NDVI, year.int = yr, 
                      correction = "none", method = met, MARGIN = 2, 
                      doParallel = FALSE, slidingperiod = 40)
    
    new <- data.frame(DOY = doy, year = yr, method = met, ndvi = ndvi[[1]]@'modelledValues')
    if (met_i == 1){
      dat_met <- new
      new_orig <- data.frame(DOY = doy, year = yr, method = 'orig', ndvi = ndvi[[1]]@'values')
      if (yr == 2017){
        dat_orig <- new_orig
      }else{
        dat_orig <- rbind(dat_orig, new_orig)
      }
    }else{
      dat_met <- rbind(dat_met, new)
    }
  }
  if (yr == 2017){
    dat_yr <- dat_met
  }else{
    dat_yr <- rbind(dat_yr, dat_met)
  }
}

head(dat_yr)

library(tidyverse)
library(ggrepel)
library(ggtext)
library(showtext)

yr <- 2018
sub_dat <- subset(dat_yr, year == yr)
highlights <- c("DLogistic")
# highlights <- c("DLogistic", "SavGol" )
n <- length(highlights)
sub_dat$hl <- if_else(sub_dat$method %in% highlights, sub_dat$method, 'other')

plt <- ggplot(
  data = sub_dat %>% filter(hl !="other"), 
  mapping = aes(x = DOY, y = ndvi, group = method, color = method)
) +
# Lines for the non-highlighted
# geom_line(
#   data = sub_dat %>% filter(hl == "other"),
#   color = "grey75",
#   size = .6,
#   alpha = .5
# ) +
  ## Lines for the highlighted 
  # It's important to put them after the grey lines
  # so the colored ones are on top
  geom_line(
    aes(color = hl),
    size = .9
  )+ 
  geom_point(
    data = dat_orig[dat_orig$year == yr, ], shape = 2, color = "black", size = 3
    ) +
  labs(
    title = paste("Comparison of smoothing methods ", yr, ', Geisenfeld Par1 Pix1', sep = " "),
       y = "ndvi", x = "DOY"
    ) +
  theme_classic()
plt

# for (yr in 2017:2022){
#   p <- ggplot(data = dat_yr[dat_yr$year == yr, ], mapping = aes(x = DOY, y = ndvi, group = method, color = method)) + 
#     geom_line() + geom_point(data = dat_orig[dat_orig$year == yr, ], shape = 2, color = "black", size = 3) +
#     labs(title = paste("Comparison of smoothing methods", yr, sep = " "),
#          y = "ndvi", x = "DOY") +
#     theme_classic()
#   ggsave(paste(yr, '.png', sep = ''), p)
# }
# 
# 
# met_i <- 1
# for (met_i in 1:length(met_list)){
#   met <- met_list[met_i]
#   p <- ggplot(data = dat_yr[dat_yr$method == met, ], mapping = aes(x = DOY, y = ndvi, group = year, color = year)) + geom_line() +
#     # + geom_point(data = dat_orig, shape = 2, color = "black", size = 3) +
#     labs(title = paste("Compare", met, "in all years", sep = " "),
#          y = "ndvi", x = "DOY") +
#     theme_classic()
#   ggsave(paste(met, '.png', sep = ''), p)
# }
# 

SG_list <- c()


ndvi <- modelNDVI(dat_year$NDVI, year.int = yr, 
                  correction = "none", method = 'SavGol', smooth = 50, window = 7, degree = 2, smoothing = 100)
ndvi
plot(ndvi[[1]])



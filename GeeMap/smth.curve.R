library(ggplot2)
library(phenex)

setwd('D:/GIS_Data/Vfl-oak/GEEMap/')
field_names <- c('Gei', 'Loh', 'Roh90', 'Roh620', 'Roh635')

dat_all <- read.csv('Dat_int_pix.csv')
head(dat_all)

dat_pheno <- read.csv('dat_pheno_pix_DL.csv')
head(dat_pheno)

yr = 2020
plot_i <- 1
plot_n <- field_names[plot_i]
par_i <- 1
pix_i <- 1

dat_plot <- subset(dat_all, ID == plot_n)
dat_par <- subset(dat_plot, Parcel == par_i)
dat_pix <- subset(dat_par, pix == pix_i)
dat_year <- subset(dat_pix, year == yr)


ndvi <- modelNDVI(dat_year$NDVI, year.int = yr,
                  correction = "none", method = "DLogistic", MARGIN = 2,
                  doParallel = FALSE, slidingperiod = 40)
ndvi
plot(ndvi[[1]])

if (yr == 2020){
  doy <- seq(1:366)
}else{
  doy <- seq(1:365)
}

sub_dat <- data.frame(DOY = doy, year = yr, orig = ndvi[[1]]@'values', ndvi = ndvi[[1]]@'modelledValues')


dat_plot_p <- subset(dat_pheno, plot == plot_n)
dat_par_p <- subset(dat_plot_p, parcel == par_i)
dat_pix_p <- subset(dat_par_p, pix == pix_i)
dat_year_p <- subset(dat_pix_p, year == yr)
dat_year_p

# gu <- dat_year_p[1, 'greenup']
sos <- dat_year_p[1, 'sos']
eos <- dat_year_p[1, 'eos']
max <- dat_year_p[1, 'max']


plt <- ggplot(
  data = sub_dat
) +
  geom_line(
    aes(x = DOY, y = ndvi),
    color = 'lightblue',
    size = .9
  )+ 
  geom_point(
    aes(x = DOY, y = orig), shape = 2, color = "black", size = 3
  ) +
  geom_vline(
    aes(xintercept = sos),
    color = "grey",
    linetype = "dotted",
    size = .8
  ) +
  geom_vline(
    aes(xintercept = eos),
    color = "grey",
    linetype = "dotted",
    size = .8
  ) +
  geom_hline(
    aes(yintercept = max),
    color = "orange",
    linetype = "dashed",
    size = .8
  ) +
  labs(
    title = paste("NDVI curve", yr, ',', plot_n, ' Par', par_i, 'Pix', pix_i, ', DLogistic', sep = " "),
    y = "NDVI", x = "DOY"
  ) +
  theme_classic(
    base_size = 20
  )

plt

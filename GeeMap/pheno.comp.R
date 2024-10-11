library(ggplot2)
library(ggthemes)
# library(cartography)
# library(mapsf)
# library(awtools)

# CRAN version
# install.packages("ggthemes")

# install.packages("cartography")
# install.packages("mapsf")

setwd('D:/GIS_Data/Vfl-oak/GEEMap/')
field_names <- c('Gei', 'Loh', 'Roh90', 'Roh620', 'Roh635')

dat_all <- read.csv('Dat_int_pix.csv')
head(dat_all)

dat_raw <- read.csv('Dat_all_pix_dc.csv')
head(dat_raw)

dat_pheno_SG <- read.csv('dat_pheno_pix_SG_impu.csv')
head(dat_pheno_SG)

dat_pheno_DL <- read.csv('dat_pheno_pix_DL.csv')
head(dat_pheno_DL)

# dat_pheno_DL <- dat_pheno_DL[!duplicated(dat_pheno_DL), ]
# write.csv(dat_pheno_DL, 'dat_pheno_pix_DL.csv', row.names = FALSE)

dat_dwd <- read.csv('DWD_Pheno.csv')
head(dat_dwd)

dat_wek <- read.csv('WekEO.csv')
head(dat_wek)

# dat_veg <- read.csv('DWD_Veg.csv')
# head(dat_veg)


dat_pheno_SG$field <- substr(dat_pheno_SG$plot,1,3)
dat_pheno_DL$field <- substr(dat_pheno_DL$plot,1,3)
dat_wek$field <- substr(dat_wek$ID,1,3)

dat_pheno_SG$method <- 'SG'
dat_pheno_DL$method <- 'DL'
dat_wek$method <- 'VPP'

dat_wek$sos <- dat_wek$SOSD
dat_wek$eos <- dat_wek$EOSD

#################
###### SOS ######
#################

dat_SG_box <- dat_pheno_SG[, c('year', 'sos', 'method', 'field', 'plot', 'pix', 'parcel')]
dat_DL_box <- dat_pheno_DL[, c('year', 'sos', 'method', 'field',  'plot', 'pix', 'parcel')]
dat_wek_box <- dat_wek[, c('year', 'sos', 'method', 'field', 'Field', 'pix', 'Parcel')]
colnames(dat_wek_box) <- c('year', 'sos', 'method', 'field',  'plot', 'pix', 'parcel')
dat_box <- rbind(dat_DL_box, dat_SG_box)
dat_box <- rbind(dat_box, dat_wek_box)
head(dat_box)

test_SG <- dat_SG_box[dat_SG_box$year == 2022,]
test_SG <- subset(test_SG, plot == 'Gei')
test_SG <- subset(test_SG, parcel == 1)
head(test_SG)

test_wek <- dat_wek_box[dat_wek_box$year == 2022,]
test_wek <- subset(test_wek, field == 'Gei')
test_wek <- subset(test_wek, parcel == 1)
head(test_wek)

y <- test_SG$sos
x <- test_wek$sos
plot(y~x)

# dat_pheno_SG$key <- paste(dat_pheno_SG$plot, '_', dat_pheno_SG$parcel, '_', dat_pheno_SG$year, sep = '')
# dat_pheno_DL$key <- paste(dat_pheno_DL$plot, '_', dat_pheno_DL$parcel, '_', dat_pheno_DL$year, sep = '')
# dat_dwd$key <- paste(dat_dwd$ID, '_', dat_dwd$Parcel, '_', dat_dwd$year, sep = '')
# dat_veg$key <- paste(dat_veg$ID, '_', dat_veg$Parcel, '_', dat_veg$year, sep = '')

dat_dwd_m <- dat_dwd[, c('ID', 'year', 'STEBO')]
dat_dwd_m$method <- 'DWD'
dat_dwd_m$field <- substr(dat_dwd_m$ID,1,3)
dwd_box <- dat_dwd_m[, -1]
colnames(dwd_box) <- c('year', 'sos', 'method', 'field')

head(dwd_box)

# dat_box <- rbind(dat_box, dwd_box)

# library(reshape2)
# dat_sos <- dat_par_SG[, c('year', 'sos', 'STEBO')]
# dat_eos <- dat_pix[, c('year', 'eos', 'STEBV')]
# 
# melt_sos <- melt(dat_sos, id.vars = "year")
# melt_eos <- melt(dat_eos, id.vars = "year")

plot_i <- 3
plot_names <- c('Gei', 'Loh', 'Roh')
plot_n <- plot_names[plot_i] 

sub_box <- subset(dat_box, field == plot_n)
sub_dwd <- subset(dwd_box, field == plot_n)


plt <- ggplot(
  data = sub_box
) +
  geom_boxplot(
    aes(x = year, y = sos, group = interaction(year, method), color = method)
  )+ 
  geom_point(
    data = sub_dwd, aes(x = year, y = sos, color = method), size = 3, shape = 21, fill = "white", position = position_nudge(x = -0.45)
  ) +
  geom_vline(
    aes(xintercept = 2017.45),
    color = "grey",
    linetype = "dotted",
    linewidth = .8
  ) +
  geom_vline(
    aes(xintercept = 2018.45),
    color = "grey",
    linetype = "dotted",
    linewidth = .8
  ) +
  geom_vline(
    aes(xintercept = 2019.45),
    color = "grey",
    linetype = "dotted",
    linewidth = .8
  ) +
  geom_vline(
    aes(xintercept = 2020.45),
    color = "grey",
    linetype = "dotted",
    linewidth = .8
  ) +
  geom_vline(
    aes(xintercept = 2021.45),
    color = "grey",
    linetype = "dotted",
    linewidth = .8
  ) +
  labs(
    title = paste("SOS compare", plot_n, sep = " "),
    y = "DOY", x = "year"
  ) +
  scale_x_continuous(
    breaks = c(2017:2022)
  ) +
  theme_classic(
    base_size = 20
  )
plt

### Loop for SOS

for (plot_i in 1:length(plot_names)){
  plot_n <- plot_names[plot_i] 
  sub_box <- subset(dat_box, field == plot_n)
  sub_dwd <- subset(dwd_box, field == plot_n)
  
  plt <- ggplot(
    data = sub_box
  ) +
    geom_boxplot(
      aes(x = year, y = sos, group = interaction(year, method), color = method)
    )+ 
    geom_point(
      data = sub_dwd, aes(x = year, y = sos, color = method), size = 3, shape = 21, fill = "white", position = position_nudge(x = -0.45)
    ) +
    geom_vline(
      aes(xintercept = 2017.45),
      color = "grey",
      linetype = "dotted",
      linewidth = .8
    ) +
    geom_vline(
      aes(xintercept = 2018.45),
      color = "grey",
      linetype = "dotted",
      linewidth = .8
    ) +
    geom_vline(
      aes(xintercept = 2019.45),
      color = "grey",
      linetype = "dotted",
      linewidth = .8
    ) +
    geom_vline(
      aes(xintercept = 2020.45),
      color = "grey",
      linetype = "dotted",
      linewidth = .8
    ) +
    geom_vline(
      aes(xintercept = 2021.45),
      color = "grey",
      linetype = "dotted",
      linewidth = .8
    ) +
    labs(
      title = paste("SOS compare", plot_n, sep = " "),
      y = "DOY", x = "year"
    ) +
    scale_x_continuous(
      breaks = c(2017:2022)
    ) +
    theme_classic(
      base_size = 20
    )
  ggsave(paste('SOS_comp_', plot_n, '_bise.png', sep = ''), plt)
}

#################
###### EOS ######
#################

dat_SG_box <- dat_pheno_SG[, c('year', 'eos', 'method', 'field')]
dat_DL_box <- dat_pheno_DL[, c('year', 'eos', 'method', 'field')]
dat_wek_box <- dat_wek[, c('year', 'eos', 'method', 'field')]
dat_box <- rbind(dat_DL_box, dat_SG_box)
dat_box <- rbind(dat_box, dat_wek_box)
head(dat_box)

dat_dwd_m <- dat_dwd[, c('ID', 'year', 'STEBV')]
dat_dwd_m$method <- 'DWD'
dat_dwd_m$field <- substr(dat_dwd_m$ID,1,3)
dwd_box <- dat_dwd_m[, -1]
colnames(dwd_box) <- c('year', 'eos', 'method', 'field')

head(dwd_box)

plot_i <- 1
plot_names <- c('Gei', 'Loh', 'Roh')
plot_n <- plot_names[plot_i] 

sub_box <- subset(dat_box, field == plot_n)
sub_dwd <- subset(dwd_box, field == plot_n)


plt <- ggplot(
  data = sub_box
) +
  geom_boxplot(
    aes(x = year, y = eos, group = interaction(year, method), color = method)
  )+ 
  geom_point(
    data = sub_dwd, aes(x = year, y = eos, color = method), size = 3, shape = 21, fill = "white", position = position_nudge(x = -0.45)
  ) +
  geom_vline(
    aes(xintercept = 2017.45),
    color = "grey",
    linetype = "dotted",
    linewidth = .8
  ) +
  geom_vline(
    aes(xintercept = 2018.45),
    color = "grey",
    linetype = "dotted",
    linewidth = .8
  ) +
  geom_vline(
    aes(xintercept = 2019.45),
    color = "grey",
    linetype = "dotted",
    linewidth = .8
  ) +
  geom_vline(
    aes(xintercept = 2020.45),
    color = "grey",
    linetype = "dotted",
    linewidth = .8
  ) +
  geom_vline(
    aes(xintercept = 2021.45),
    color = "grey",
    linetype = "dotted",
    linewidth = .8
  ) +
  labs(
    title = paste("EOS compare", plot_n, sep = " "),
    y = "DOY", x = "year"
  ) +
  scale_x_continuous(
    breaks = c(2017:2022)
  ) +
  theme_classic(
    base_size = 20
  )
plt

for (plot_i in 1:length(plot_names)){
  plot_n <- plot_names[plot_i] 
  sub_box <- subset(dat_box, field == plot_n)
  sub_dwd <- subset(dwd_box, field == plot_n)
  
  plt <- ggplot(
    data = sub_box
  ) +
    geom_boxplot(
      aes(x = year, y = eos, group = interaction(year, method), color = method)
    )+ 
    geom_point(
      data = sub_dwd, aes(x = year, y = eos, color = method), size = 3, shape = 21, fill = "white", position = position_nudge(x = -0.5)
    ) +
    geom_vline(
      aes(xintercept = 2017.45),
      color = "grey",
      linetype = "dotted",
      linewidth = .8
    ) +
    geom_vline(
      aes(xintercept = 2018.45),
      color = "grey",
      linetype = "dotted",
      linewidth = .8
    ) +
    geom_vline(
      aes(xintercept = 2019.45),
      color = "grey",
      linetype = "dotted",
      linewidth = .8
    ) +
    geom_vline(
      aes(xintercept = 2020.45),
      color = "grey",
      linetype = "dotted",
      linewidth = .8
    ) +
    geom_vline(
      aes(xintercept = 2021.45),
      color = "grey",
      linetype = "dotted",
      linewidth = .8
    ) +
    labs(
      title = paste("EOS compare", plot_n, sep = " "),
      y = "DOY", x = "year"
    ) +
    scale_x_continuous(
      breaks = c(2017:2022)
    ) +
    theme_classic(
      base_size = 20
    )
  ggsave(paste('EOS_comp_', plot_n, '.png', sep = ''), plt)
}


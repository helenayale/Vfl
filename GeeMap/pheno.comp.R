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

# dat_all <- read.csv('Dat_int_pix.csv')
# head(dat_all)
# 
# dat_raw <- read.csv('Dat_all_pix_dc.csv')
# head(dat_raw)

dat_pheno_SG <- read.csv('dat_pheno_pix_SG_impu.csv')
head(dat_pheno_SG)

unique(dat_pheno_SG$plot)

dat_pheno_DL <- read.csv('dat_pheno_pix_DL.csv')
head(dat_pheno_DL)

unique(dat_pheno_DL$plot)

# dat_pheno_DL <- dat_pheno_DL[!duplicated(dat_pheno_DL), ]
# write.csv(dat_pheno_DL, 'dat_pheno_pix_DL.csv', row.names = FALSE)

dat_dwd <- read.csv('DWD_Pheno.csv')
head(dat_dwd)

unique(dat_dwd$ID)

dat_wek <- read.csv('WekEO.csv')
head(dat_wek)

unique(dat_wek$ID)

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

dat_meta <- read.csv('meta_data.csv')
head(dat_meta)

unique(dat_meta$key)


#################
###### SOS ######
#################

dat_SG_box <- dat_pheno_SG[, c('year', 'sos', 'eos', 'method', 'field', 'plot', 'pix', 'parcel')]
dat_DL_box <- dat_pheno_DL[, c('year', 'sos', 'eos', 'method', 'field',  'plot', 'pix', 'parcel')]
dat_wek_box <- dat_wek[, c('year', 'sos', 'eos', 'method', 'field', 'ID', 'pix', 'Parcel')]
colnames(dat_wek_box) <- c('year', 'sos', 'eos', 'method', 'field',  'plot', 'pix', 'parcel')
dat_box <- rbind(dat_DL_box, dat_SG_box)
dat_box <- rbind(dat_box, dat_wek_box)
head(dat_box)

dat_box$key <- paste(dat_box$plot, '_', dat_box$parcel, sep = '')
head(dat_box)

unique(dat_box$key)

dat_dwd$key <- paste(dat_dwd$ID, '_', dat_dwd$Parcel, sep = '')
unique(dat_dwd$key)

dat_merge <- merge(dat_box, dat_meta, by = 'key')
head(dat_merge)

dat_merge$key_yr <- paste(dat_merge$key, '_', dat_merge$year, sep = '')
dat_dwd$key_yr <- paste(dat_dwd$key, '_', dat_dwd$year, sep = '')

unique(dat_merge$key_yr)
unique(dat_dwd$key_yr)

dat_dwd_m <- dat_dwd[, c('key_yr', 'STEBO', 'STEBV')]

dat_merge <- merge(dat_merge, dat_dwd_m, by = 'key_yr')
head(dat_merge)


#### temperature data
dat_dwd2 <- read.csv('DWD_Season.csv')
head(dat_dwd2)

dat_dwd2$key <- paste(dat_dwd2$ID, '_', dat_dwd2$Parcel, sep = '')
unique(dat_dwd2$key)
dat_dwd2$key_yr <- paste(dat_dwd2$key, '_', dat_dwd2$year, sep = '')
unique(dat_dwd2$key_yr)

dat_spr <- subset(dat_dwd2, season == 'spr')
head(dat_spr)
dat_spr_m <- dat_spr[, c('key_yr', 'SDI', 'SPrec', 'SSunDur', 'STMax', 'STMean')]
head(dat_spr_m)

dat_merge <- merge(dat_merge, dat_spr_m, by = 'key_yr')
head(dat_merge)

save(dat_merge, file = 'dat_merge.rda')

# test_SG <- dat_SG_box[dat_SG_box$year == 2022,]
# test_SG <- subset(test_SG, plot == 'Gei')
# test_SG <- subset(test_SG, parcel == 1)
# head(test_SG)
# 
# test_wek <- dat_wek_box[dat_wek_box$year == 2022,]
# test_wek <- subset(test_wek, field == 'Gei')
# test_wek <- subset(test_wek, parcel == 1)
# head(test_wek)
# 
# y <- test_SG$sos
# x <- test_wek$sos
# plot(y~x)

# dat_pheno_SG$key <- paste(dat_pheno_SG$plot, '_', dat_pheno_SG$parcel, '_', dat_pheno_SG$year, sep = '')
# dat_pheno_DL$key <- paste(dat_pheno_DL$plot, '_', dat_pheno_DL$parcel, '_', dat_pheno_DL$year, sep = '')
# dat_dwd$key <- paste(dat_dwd$ID, '_', dat_dwd$Parcel, '_', dat_dwd$year, sep = '')
# dat_veg$key <- paste(dat_veg$ID, '_', dat_veg$Parcel, '_', dat_veg$year, sep = '')
# 
dat_dwd_m <- dat_dwd[, c('key_yr', 'ID', 'year', 'STEBO', 'STEBV')]
dat_dwd_m$method <- 'DWD'
dat_spr_m <- dat_spr[, c('key_yr', 'SDI', 'SPrec', 'SSunDur', 'STMax', 'STMean')]
#dwd_box <- dat_dwd_m[, -1]
dwd_box <- merge(dat_dwd_m, dat_spr_m, by = 'key_yr')
colnames(dwd_box) <- c('key_yr','plot', 'year', 'sos', 'eos', 'method', 'SDI', 'SPrec', 'SSunDur', 'STMax', 'STMean')

dwd_box$STMean <- dwd_box$STMean/10
head(dwd_box)


# dat_box <- rbind(dat_box, dwd_box)

# library(reshape2)
# dat_sos <- dat_par_SG[, c('year', 'sos', 'STEBO')]
# dat_eos <- dat_pix[, c('year', 'eos', 'STEBV')]
# 
# melt_sos <- melt(dat_sos, id.vars = "year")
# melt_eos <- melt(dat_eos, id.vars = "year")

plot_i <- 1
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
sub_dwd <- subset(dwd_box, plot == plot_n)


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


#############################
####### plot by treat #######
#############################

load('dat_merge.rda')
head(dat_merge)

unique(dat_merge$plot)

sub_merge <- subset(dat_merge, method == 'DL')
sub_gei <- subset(sub_merge, plot == 'Gei')

dat_count <-sub_gei %>%
  count(year) %>%
  arrange(desc(n))


dat_count <-sub_gei %>%
  group_by(year) %>%
  summarize(
    mean = mean(sos, na.rm = TRUE),
    n_pixels = n(),
    sd = sd(sos, na.rm = TRUE )
    )%>%
  arrange(year)
dat_count

sub_roh620 <- subset(sub_merge, plot == 'Roh620')



plt <- ggplot(
  data = sub_merge
) +
  geom_boxplot(
    aes(x = year, y = sos, group = interaction(year, Treat, plot), color = Treat)
  )+ 
  geom_point(
    data = sub_dwd, aes(x = year, y = sos), size = 3, shape = 21, fill = "white", position = position_nudge(x = -0.5)
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
  theme_minimal(
    base_size = 20
  ) +
  facet_wrap(
     ~plot
  )
plt



sub_F <- subset(sub_merge, Treat == 'F-Grad')


plt <- ggplot(
  data = sub_gei
) +
  geom_boxplot(
    aes(x = year, y = sos, group = interaction(year, plot, Treat), color = Treat)
  )+   
  geom_line(
    data = sub_dwd, aes(x = year, y = (STMean - 6) * (150-100)/(11-6) + 100)
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
    title = paste("SOS compare","Gei", sep = " "),
    y = "DOY", x = "year"
  ) +
  scale_x_continuous(
    breaks = c(2017:2022)
  ) +
  scale_y_continuous(
    name = 'SOS',
    sec.axis = sec_axis(~((. -100)*(11-6)/(150-100))+6,
                        name = "Mean Spr. Temp(°C)")
  ) +
  theme_minimal(
    base_size = 20
  ) 
plt


load('dat_wek.rda')
head(dat_wek)

# load('wekEO_control.rda')
# head(dat_all)
# 
# colnames(dat_all) <- c('ID', 'Field','Parcel', 'Interne', 'year', 'pix', 'eos', 'sos')
# dat_con <- dat_all[, c('ID', 'Field','Parcel', 'Interne', 'year', 'pix', 'sos', 'eos')]
# head(dat_con)
# dat_con$Treat <- 'A-Grad'
# head(dat_con)
# 
# dat_wek2 <- dat_wek[, c('ID', 'Field','Parcel', 'Interne', 'year', 'pix', 'sos', 'eos', 'Treat')]
# head(dat_wek2)
# 
# dat_merge <- rbind(dat_wek2, dat_con)
# head(dat_merge)

plt <- ggplot(
  data = dat_wek
) +
  geom_boxplot(
    aes(x = year, y = sos, group = interaction(year, Treat, ID), color = Treat)
  )+ 
  geom_point(
    data = dat_dwd, aes(x = year, y = STEBO), size = 3, shape = 21, fill = "white", position = position_nudge(x = -0.45)
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
  geom_vline(
    aes(xintercept = 2022.45),
    color = "grey",
    linetype = "dotted",
    linewidth = .8
  ) +
  labs(
    title = paste("SOS compare", sep = " "),
    y = "DOY", x = "year"
  ) +
  scale_x_continuous(
    breaks = c(2017:2023)
  ) +
  theme_minimal(
    base_size = 20
  ) +
  facet_wrap(
    ~ID
  )
plt

unique(dat_wek$ID)


tr <- dat_wek$Treat
tr[tr == 'A-Grad'] <- 'Low'
tr[tr == 'F-Grad'] <- 'Medium'
tr[tr == 'Z-Baum'] <- 'High'

unique(tr)

dat_wek$Treat_lvl <- factor(tr, levels = c('Low', 'Medium', 'High'))


sub_gei <- subset(dat_wek, ID == 'Gei')
sub_roh620 <- subset(dat_wek, ID == 'Roh620')
sub_roh90 <- subset(dat_wek, ID == 'Roh90')

load('DWD_SDI.rda')
head(dat_sea)
dat_sea <- subset(dat_sea, season == 'spr')

#dat_sea$STMean <- dat_sea$STMean/10
range(dat_sea$SDI)

dat_dwd_m <- dat_sea[, c('ID','Parcel', 'year', 'SDI')]
dat_dwd_m$method <- 'DWD'

sub_dwd_gei <- subset(dat_dwd_m, ID == 'Gei')
sub_dwd_roh620 <- subset(dat_dwd_m, ID == 'Roh620')
sub_dwd_roh90 <- subset(dat_dwd_m, ID == 'Roh90')

head(dat_dwd)
dat_dwd_pheno <- dat_dwd[, c('ID','Parcel', 'year', 'STEBO', 'STEBV')]

sub_pheno_gei <- subset(dat_dwd_pheno, ID == 'Gei')
sub_pheno_roh620 <- subset(dat_dwd_pheno, ID == 'Roh620')
sub_pheno_roh90 <- subset(dat_dwd_pheno, ID == 'Roh90')

#head(sub_dwd)
#range(sub_dwd$SDI)

plt <- ggplot(
  data = sub_gei
) +
  geom_boxplot(
    aes(x = year, y = sos, group = interaction(year, ID, Treat_lvl), color = Treat_lvl)
  )+   
  geom_line(
    data = sub_dwd_gei, aes(x = year, y = (SDI - 6) * (150-100)/(10-4) + 100)
  ) +
  geom_vline(
    aes(xintercept = 2017.5),
    color = "grey",
    linetype = "dotted",
    linewidth = .8
  ) +
  geom_vline(
    aes(xintercept = 2018.5),
    color = "grey",
    linetype = "dotted",
    linewidth = .8
  ) +
  geom_vline(
    aes(xintercept = 2019.5),
    color = "grey",
    linetype = "dotted",
    linewidth = .8
  ) +
  geom_vline(
    aes(xintercept = 2020.5),
    color = "grey",
    linetype = "dotted",
    linewidth = .8
  ) +
  geom_vline(
    aes(xintercept = 2021.5),
    color = "grey",
    linetype = "dotted",
    linewidth = .8
  ) +
  geom_vline(
    aes(xintercept = 2022.5),
    color = "grey",
    linetype = "dotted",
    linewidth = .8
  ) +
  labs(
    title = paste("Geisenfeld", sep = " "),
    y = "DOY", x = "year", color = ""
  ) +
  scale_x_continuous(
    breaks = c(2017:2023)
  ) +
  scale_y_continuous(
    name = 'SOS',
    sec.axis = sec_axis(~((. -100)*(10-4)/(150-100))+6,
                        name = "Drought Index (mm/°C)")
  ) +
  scale_color_manual(
    values = c("Low" = "#00AFBB", "Medium" = "#E7B800", "High" = "#FC4E07") 
  ) +
  theme_minimal(
    base_size = 20
  ) +
  theme(panel.grid = element_blank())
plt



plt <- ggplot(
  data = sub_roh90
) +
  geom_boxplot(
    aes(x = year, y = sos, group = interaction(year, ID, Treat_lvl), color = Treat_lvl)
  )+   
  geom_line(
    data = sub_dwd_roh90, aes(x = year, y = (SDI - 6) * (150-100)/(10-4) + 100)
  ) +
  geom_vline(
    aes(xintercept = 2017.5),
    color = "grey",
    linetype = "dotted",
    linewidth = .8
  ) +
  geom_vline(
    aes(xintercept = 2018.5),
    color = "grey",
    linetype = "dotted",
    linewidth = .8
  ) +
  geom_vline(
    aes(xintercept = 2019.5),
    color = "grey",
    linetype = "dotted",
    linewidth = .8
  ) +
  geom_vline(
    aes(xintercept = 2020.5),
    color = "grey",
    linetype = "dotted",
    linewidth = .8
  ) +
  geom_vline(
    aes(xintercept = 2021.5),
    color = "grey",
    linetype = "dotted",
    linewidth = .8
  ) +
  geom_vline(
    aes(xintercept = 2022.5),
    color = "grey",
    linetype = "dotted",
    linewidth = .8
  ) +
  labs(
    title = paste("Rohrbrunn 90", sep = " "),
    y = "DOY", x = "year", color = ""
  ) +
  scale_x_continuous(
    breaks = c(2017:2023)
  ) +
  scale_y_continuous(
    name = 'SOS',
    sec.axis = sec_axis(~((. -100)*(10-4)/(150-100))+6,
                        name = "Drought Index (mm/°C)")
  ) +
  scale_color_manual(
    values = c("Low" = "#00AFBB", "Medium" = "#E7B800", "High" = "#FC4E07") 
  ) +
  theme_minimal(
    base_size = 20
  ) +
  theme(panel.grid = element_blank())
plt


plt <- ggplot(
  data = sub_roh620
) +
  geom_boxplot(
    aes(x = year, y = sos, group = interaction(year, ID, Treat_lvl), color = Treat_lvl)
  )+   
  geom_line(
    data = sub_dwd_roh620, aes(x = year, y = (SDI - 6) * (150-100)/(10-4) + 100)
  ) +
  geom_vline(
    aes(xintercept = 2017.5),
    color = "grey",
    linetype = "dotted",
    linewidth = .8
  ) +
  geom_vline(
    aes(xintercept = 2018.5),
    color = "grey",
    linetype = "dotted",
    linewidth = .8
  ) +
  geom_vline(
    aes(xintercept = 2019.5),
    color = "grey",
    linetype = "dotted",
    linewidth = .8
  ) +
  geom_vline(
    aes(xintercept = 2020.5),
    color = "grey",
    linetype = "dotted",
    linewidth = .8
  ) +
  geom_vline(
    aes(xintercept = 2021.5),
    color = "grey",
    linetype = "dotted",
    linewidth = .8
  ) +
  geom_vline(
    aes(xintercept = 2022.5),
    color = "grey",
    linetype = "dotted",
    linewidth = .8
  ) +
  labs(
    title = paste("Rohrbrunn 620", sep = " "),
    y = "DOY", x = "year", color = ""
  ) +
  scale_x_continuous(
    breaks = c(2017:2023)
  ) +
  scale_y_continuous(
    name = 'SOS',
    sec.axis = sec_axis(~((. -100)*(10-4)/(150-100))+6,
                        name = "Drought Index (mm/°C)")
  ) +
  scale_color_manual(
    values = c("Low" = "#00AFBB", "Medium" = "#E7B800", "High" = "#FC4E07") 
  ) +
  theme_minimal(
    base_size = 20
  ) +
  theme(panel.grid = element_blank())
plt

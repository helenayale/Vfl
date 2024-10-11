library(dplyr)
library(ggplot2)
library(naniar)

setwd('D:/GIS_Data/Vfl-oak/GEEMap/')
field_names <- c('Gei', 'Loh', 'Roh90', 'Roh620', 'Roh635')

dat_all <- read.csv('Dat_int_pix.csv')
head(dat_all)

dat_raw <- read.csv('Dat_all_pix_dc.csv')
head(dat_raw)
dat_raw <- na.omit(dat_raw)
head(dat_raw)

dat_meta <- read.csv('meta_data.csv')
dat_meta

dat_pheno_SG <- read.csv('dat_pheno_pix_SG_impu.csv')
dat_pheno_SG$key <- paste(dat_pheno_SG$plot, '_', dat_pheno_SG$parcel, sep = '')
dat_pheno_SG <- merge(dat_pheno_SG, dat_meta, by = 'key')
dat_pheno_SG <- dat_pheno_SG %>% replace_with_na_at(.vars = 'eos', condition = ~.x < 210) # remove outliers in eos
dat_pheno_DL <- dat_pheno_DL %>% replace_with_na_at(.vars = 'eos50', condition = ~.x < 210) # remove outliers in eos
head(dat_pheno_SG)

dat_pheno_DL <- read.csv('dat_pheno_pix_DL.csv')
dat_pheno_DL$key <- paste(dat_pheno_DL$plot, '_', dat_pheno_DL$parcel, sep = '')
dat_pheno_DL <- merge(dat_pheno_DL, dat_meta, by = 'key')
head(dat_pheno_DL)
# library(naniar)
dat_pheno_DL <- dat_pheno_DL %>% replace_with_na_at(.vars = 'eos', condition = ~.x < 210) # remove outliers in eos, DL
dat_pheno_DL <- dat_pheno_DL %>% replace_with_na_at(.vars = 'eos50', condition = ~.x < 210) # remove outliers in eos, DL

dat_dwd <- read.csv('DWD_Pheno.csv')
dat_dwd$key <- paste(dat_dwd$ID, '_', dat_dwd$Parcel, sep = '')
dat_dwd <- merge(dat_dwd, dat_meta, by = 'key')
head(dat_dwd)
colnames(dat_dwd) <- c('key', 'plot', 'field', 'parcel', 'interne', 'year', 'pix', 'sos', 'eos','fruit', 'Treat', 'Age')
dat_dwd$los <- dat_dwd$eos - dat_dwd$sos

dat_wek <- read.csv('WekEO.csv')
dat_wek$key <- paste(dat_wek$ID, '_', dat_wek$Parcel, sep = '')
dat_wek <- merge(dat_wek, dat_meta, by = 'key')
head(dat_wek)
colnames(dat_wek) <- c('key', 'plot', 'field', 'parcel', 'interne', 'year', 'pix', 'eos', 'eosv', 'sos', 'sosv', 'max', 'maxv', 'Treat', 'Age')

# library(naniar)
dat_wek <- dat_wek %>% replace_with_na_at(.vars = 'eos', condition = ~.x < 210) # remove outliers in eos
head(dat_wek)

dat_wek$los <- dat_wek$eos - dat_wek$sos

###################
### define data ###
###  SG DL VPP  ###
###################

data_names <- c('dat_pheno_SG', 'dat_pheno_DL', 'dat_wek', 'dat_dwd')
title_names <- c('Savitzy-Golay', 'DLogistic', 'WekEO-VPP', 'DWD')
abbr_names <- c('SG', 'DL', 'VPP', 'DWD')
data_i <- 1
eval(parse(text = paste('dat_merge <- ', data_names[data_i], sep = '')))
head(dat_merge)

dat_merge$new_plot <- dat_merge$plot
dat_merge$new_plot <- replace(dat_merge$new_plot, dat_merge$new_plot == 'Loh', 'Roh')
dat_merge$new_plot <- replace(dat_merge$new_plot, dat_merge$new_plot == 'Roh620', 'Roh')
dat_merge$new_plot <- replace(dat_merge$new_plot, dat_merge$new_plot == 'Roh635', 'Roh')
dat_merge$new_plot <- replace(dat_merge$new_plot, dat_merge$new_plot == 'Roh90', 'Roh')
dat_merge$new_plot

nrow(dat_merge)
head(dat_merge)

# yr <- 2017
# plot_i <- 1
# par_i <- 1
# pix_i <- 1
# 
# head(dat_dwd)
# head(dat_raw)
# 
# sub_yr <- subset(dat_raw, year == yr)
# sub_yr_d <- subset(dat_dwd, year == yr)
# 
# sub_plt <- subset(sub_yr, ID == field_names[plot_i])
# sub_plt_d <- subset(sub_yr_d, plot == field_names[plot_i])
# par_n <- max(sub_plt$Parcel)
# 
# sub_par <- subset(sub_plt, Parcel == par_i)
# sub_par_d <- subset(sub_plt_d, parcel == par_i)
# pix_n <- max(sub_par$pix)
# 
# sub_pix <- subset(sub_par, pix == pix_i)
# sub_pix_d <- subset(sub_par_d, pix = pix_i)
# 
# sos <- sub_pix_d[, sub_pix_d$pix == pix_i]$sos
# eos <- sub_pix_d[, sub_pix_d$pix == pix_i]$eos
# 
# sos_r <- sub_pix[which(abs(sub_pix$DOY - sos) == min(abs(sub_pix$DOY - sos))),]$DOY
# eos_r <- sub_pix[which(abs(sub_pix$DOY - eos) == min(abs(sub_pix$DOY - eos))),]$DOY
# 
# NDVI_sos <- sub_pix[which(abs(sub_pix$DOY - sos) == min(abs(sub_pix$DOY - sos))),]$NDVI
# NDVI_eos <- sub_pix[which(abs(sub_pix$DOY - eos) == min(abs(sub_pix$DOY - eos))),]$NDVI
# 
# max_day <- sub_pix[which(sub_pix$NDVI == max(sub_pix$NDVI, na.rm = TRUE)),]$DOY
# NDVI_max <- max(sub_pix$NDVI, na.rm = TRUE)
# 
# dat_comp <- data.frame(plot = field_names[plot_i], par = par_i, pix = pix_i, year = yr, sos = sos, sos_r = sos_r, NDVI_sos = NDVI_sos, eos = eos, eos_r = eos_r, NDVI_eos = NDVI_eos, max_d = max_day, NDVI_max = NDVI_max)
# dat_comp

# Nearest date to SOS/EOS
# field_names <- c('Gei', 'Loh', 'Roh90', 'Roh620', 'Roh635')
# 
# for (yr in 2017:2022){
#   sub_yr <- subset(dat_raw, year == yr)
#   sub_yr_d <- subset(dat_dwd, year == yr)
#   for (plot_i in 1:length(field_names)){
#     sub_plt <- subset(sub_yr, ID == field_names[plot_i])
#     sub_plt_d <- subset(sub_yr_d, plot == field_names[plot_i])
#     par_n <- max(sub_plt$Parcel)
#     for (par_i in 1:par_n){
#       sub_par <- subset(sub_plt, Parcel == par_i)
#       sub_par_d <- subset(sub_plt_d, parcel == par_i)
#       pix_n <- max(sub_par$pix)
#       for (pix_i in 1:pix_n){
#         sub_pix <- subset(sub_par, pix == pix_i)
#         sub_pix_d <- sub_par_d
#         
#         sos <- sub_pix_d$sos
#         eos <- sub_pix_d$eos
#         
#         sos_r <- sub_pix[which(abs(sub_pix$DOY - sos) == min(abs(sub_pix$DOY - sos))),]$DOY
#         eos_r <- sub_pix[which(abs(sub_pix$DOY - eos) == min(abs(sub_pix$DOY - eos))),]$DOY
#         
#         NDVI_sos <- sub_pix[which(abs(sub_pix$DOY - sos) == min(abs(sub_pix$DOY - sos))),]$NDVI
#         NDVI_eos <- sub_pix[which(abs(sub_pix$DOY - eos) == min(abs(sub_pix$DOY - eos))),]$NDVI
#         
#         max_day <- sub_pix[which(sub_pix$NDVI == max(sub_pix$NDVI, na.rm = TRUE)),]$DOY
#         NDVI_max <- max(sub_pix$NDVI, na.rm = TRUE)
#         
#         dat_pix <- data.frame(plot = field_names[plot_i], par = par_i, pix = pix_i, year = yr, sos = sos, sos_r = sos_r, NDVI_sos = NDVI_sos, eos = eos, eos_r = eos_r, NDVI_eos = NDVI_eos, max_d = max_day, NDVI_max = NDVI_max)
#         
#         if (pix_i == 1){
#           dat_par <- dat_pix
#         }else{
#           dat_par <- rbind(dat_par, dat_pix)
#         }
#         
#       }
#       if (par_i == 1){
#         dat_plt <- dat_par
#       }else{
#         dat_plt <- rbind(dat_plt, dat_par)
#       }
#     }
#     if(plot_i == 1){
#       dat_yr <- dat_plt
#     }else{
#       dat_yr <- rbind(dat_yr, dat_plt)
#     }
#   }
#   if(yr == 2017){
#     dat_comp <- dat_yr
#   }else{
#     dat_comp <- rbind(dat_comp, dat_yr)
#   }
# }
# 
# dat_comp
# head(dat_comp)
# dat_comp[dat_comp$plot == 'Loh',]
# 
# plt <- ggplot(
#   data = dat_comp
# ) +
#   geom_boxplot(
#     aes(x = year, y = NDVI_sos, group = interaction(year, Treat), color = Treat)
#   )+
#   geom_vline(
#     aes(xintercept = 2017.45),
#     color = "grey",
#     linetype = "dotted",
#     linewidth = .8
#   ) +
#   geom_vline(
#     aes(xintercept = 2018.45),
#     color = "grey",
#     linetype = "dotted",
#     linewidth = .8
#   ) +
#   geom_vline(
#     aes(xintercept = 2019.45),
#     color = "grey",
#     linetype = "dotted",
#     linewidth = .8
#   ) +
#   geom_vline(
#     aes(xintercept = 2020.45),
#     color = "grey",
#     linetype = "dotted",
#     linewidth = .8
#   ) +
#   geom_vline(
#     aes(xintercept = 2021.45),
#     color = "grey",
#     linetype = "dotted",
#     linewidth = .8
#   )+
#   scale_x_continuous(
#     breaks = c(2017:2022)
#   ) +
#   labs(
#     title = "Interannual Comparison",
#     y = "NDVI_SOS", x = "Year"
#   ) 
#   
# plt


plt <- ggplot(
  data = dat_merge
) +
  geom_boxplot(
    aes(x = year, y = eos, group = interaction(year, plot), color = plot)
  )+
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
  )+
  scale_x_continuous(
    breaks = c(2017:2022)
  ) +
  labs(
    title = "Interannual Comparison EOS",
    y = "DOY", x = "Year"
  ) 
  
plt


plt <- ggplot(
  data = dat_merge
) +
  geom_boxplot(
    aes(x = year, y = sos_mod_v, group = interaction(year, Treat), color = Treat)
  )+
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
  )+
  scale_x_continuous(
    breaks = c(2017:2022)
  ) +
  labs(
    title = "Interannual Comparison",
    y = "NDVI_SOS", x = "Year"
  ) 

plt


plt <- ggplot(
  data = dat_merge
) +
  geom_boxplot(
    aes(x = year, y = eos_mod_v, group = interaction(year, Treat), color = Treat)
  )+
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
  )+
  scale_x_continuous(
    breaks = c(2017:2022)
  ) +
  labs(
    title = "Interannual Comparison",
    y = "NDVI_EOS", x = "Year"
  ) 

plt

head(dat_merge)



plt <- ggplot(
  data = dat_merge
) +
  geom_boxplot(
    aes(x = year, y = max, group = interaction(year, Treat), color = Treat)
  )+
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
  )+
  scale_x_continuous(
    breaks = c(2017:2022)
  ) +
  labs(
    title = "Interannual Comparison Max",
    y = "NDVI", x = "Year"
  ) 

plt

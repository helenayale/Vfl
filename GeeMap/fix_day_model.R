library(dplyr)
library(ggpubr)
library(ggplot2)
library(lme4)
library(lmerTest) # package to show p-value
library(naniar)

setwd('D:/GIS_Data/Vfl-oak/GEEMap/')
field_names <- c('Gei', 'Loh', 'Roh90', 'Roh620', 'Roh635')

# dat_all <- read.csv('Dat_int_pix.csv')
# head(dat_all)
# 
# dat_raw <- read.csv('Dat_all_pix_dc.csv')
# head(dat_raw)

dat_meta <- read.csv('meta_data.csv')
dat_meta

#### Load data SG
dat_fix_SG <- read.csv('dat_fix_day_SG.csv')
dat_fix_SG$key <- paste(dat_fix_SG$plot, '_', dat_fix_SG$parcel, sep = '')
dat_fix_SG <- merge(dat_fix_SG, dat_meta, by = 'key')
# library(naniar)
dat_fix_SG <- dat_fix_SG %>% replace_with_na_at(.vars = 'eos', condition = ~.x < 210) # remove outliers in eos, DL
head(dat_fix_SG)

head(dat_fix_SG)


# merge plots into two: North & Middle
dat_fix_SG$north <- dat_fix_SG$plot
dat_fix_SG$north <- replace(dat_fix_SG$north, dat_fix_SG$north == 'Loh', 'Roh')
dat_fix_SG$north <- replace(dat_fix_SG$north, dat_fix_SG$north == 'Roh620', 'Roh')
dat_fix_SG$north <- replace(dat_fix_SG$north, dat_fix_SG$north == 'Roh635', 'Roh')
dat_fix_SG$north <- replace(dat_fix_SG$north, dat_fix_SG$north == 'Roh90', 'Roh')
dat_fix_SG$north <- replace(dat_fix_SG$north, dat_fix_SG$north == 'Roh', 'North')
dat_fix_SG$north <- replace(dat_fix_SG$north, dat_fix_SG$north == 'Gei', 'Middle')
dat_fix_SG$north

#### Load data DL
dat_fix_DL <- read.csv('dat_fix_day_DL.csv')
dat_fix_DL$key <- paste(dat_fix_DL$plot, '_', dat_fix_DL$parcel, sep = '')
dat_fix_DL <- merge(dat_fix_DL, dat_meta, by = 'key')
# library(naniar)
dat_fix_DL <- dat_fix_DL %>% replace_with_na_at(.vars = 'eos', condition = ~.x < 210) # remove outliers in eos, DL
head(dat_fix_DL)

# merge plots into two: North & Middle
dat_fix_DL$north <- dat_fix_DL$plot
dat_fix_DL$north <- replace(dat_fix_DL$north, dat_fix_DL$north == 'Loh', 'Roh')
dat_fix_DL$north <- replace(dat_fix_DL$north, dat_fix_DL$north == 'Roh620', 'Roh')
dat_fix_DL$north <- replace(dat_fix_DL$north, dat_fix_DL$north == 'Roh635', 'Roh')
dat_fix_DL$north <- replace(dat_fix_DL$north, dat_fix_DL$north == 'Roh90', 'Roh')
dat_fix_DL$north <- replace(dat_fix_DL$north, dat_fix_DL$north == 'Roh', 'North')
dat_fix_DL$north <- replace(dat_fix_DL$north, dat_fix_DL$north == 'Gei', 'Middle')
dat_fix_DL$north
# 
# #### Load data WekEO VPP
# dat_wek <- read.csv('WekEO.csv')
# dat_wek$key <- paste(dat_wek$ID, '_', dat_wek$Parcel, sep = '')
# dat_wek <- merge(dat_wek, dat_meta, by = 'key')
# head(dat_wek)
# colnames(dat_wek) <- c('key', 'plot', 'field', 'parcel', 'interne', 'year', 'pix', 'eos', 'eosv', 'sos', 'sosv', 'max', 'maxv', 'Treat', 'Age')
# # library(naniar)
# dat_wek <- dat_wek %>% replace_with_na_at(.vars = 'eos', condition = ~.x < 210) # remove outliers in eos, DL
# head(dat_wek)
# 
# dat_wek$los <- dat_wek$eos - dat_wek$sos
# head(dat_wek)
# 
# # merge plots into two: North & Middle
# dat_wek$north <- dat_wek$plot
# dat_wek$north <- replace(dat_wek$north, dat_wek$north == 'Loh', 'Roh')
# dat_wek$north <- replace(dat_wek$north, dat_wek$north == 'Roh620', 'Roh')
# dat_wek$north <- replace(dat_wek$north, dat_wek$north == 'Roh635', 'Roh')
# dat_wek$north <- replace(dat_wek$north, dat_wek$north == 'Roh90', 'Roh')
# dat_wek$north <- replace(dat_wek$north, dat_wek$north == 'Roh', 'North')
# dat_wek$north <- replace(dat_wek$north, dat_wek$north == 'Gei', 'Middle')
# head(dat_wek)


#### load DWD data
# leaf unfolding & leaf coloring dates
dat_dwd <- read.csv('DWD_Pheno.csv')
dat_dwd$key <- paste(dat_dwd$ID, '_', dat_dwd$Parcel, sep = '')
dat_dwd <- merge(dat_dwd, dat_meta, by = 'key')
head(dat_dwd)
dat_dwd$DWD_LOS <- dat_dwd$STEBV - dat_dwd$STEBO
# Seasonal Temperature
dat_dwd_s <- read.csv('DWD_Season.csv')
dat_dwd_s$key <- paste(dat_dwd_s$ID, '_', dat_dwd_s$Parcel, '_', dat_dwd_s$year, sep = '')
head(dat_dwd_s)

dat_spr <- subset(dat_dwd_s, dat_dwd_s$season == 'spr')
dat_sum <- subset(dat_dwd_s, dat_dwd_s$season == 'sum')
dat_aut <- subset(dat_dwd_s, dat_dwd_s$season == 'aut')
dat_win <- subset(dat_dwd_s, dat_dwd_s$season == 'win')
head(dat_spr)

dat_t_spr <- dat_spr[, c('key', 'STMean')]
colnames(dat_t_spr) <- c('key', 'spr_temp')
dat_t_sum <- dat_sum[, c('key', 'STMean')]
colnames(dat_t_sum) <- c('key', 'sum_temp')
dat_t_aut <- dat_aut[, c('key', 'STMean')]
colnames(dat_t_aut) <- c('key', 'aut_temp')
dat_t_win <- dat_win[, c('key', 'STMean')]
colnames(dat_t_win) <- c('key', 'win_temp')

dat_temp <- merge(dat_t_spr, dat_t_sum, by = 'key')
dat_temp <- merge(dat_temp , dat_t_aut, by = 'key')
dat_temp <- merge(dat_temp , dat_t_win, by = 'key')
# head(dat_temp)
dat_temp[, -1] <- dat_temp[, -1] * 0.1
head(dat_temp)

dat_dwd$key <- paste(dat_dwd$key, '_', dat_dwd$year, sep = '')
dat_dwd <- merge(dat_dwd, dat_temp, by = 'key')
head(dat_dwd)

#### add DWD data to each dataset
dat_dwd_m <- dat_dwd[,c('key', 'STEBO', 'STEBV', 'DWD_LOS', 'spr_temp', 'sum_temp', 'aut_temp', 'win_temp')]
head(dat_dwd_m)

dat_fix_SG$key <- paste(dat_fix_SG$key, '_', dat_fix_SG$year, sep = '')
dat_fix_SG <- merge(dat_fix_SG, dat_dwd_m, by = 'key')
head(dat_fix_SG)

dat_fix_DL$key <- paste(dat_fix_DL$key, '_', dat_fix_DL$year, sep = '')
dat_fix_DL <- merge(dat_fix_DL, dat_dwd_m, by = 'key')
head(dat_fix_DL)

# dat_wek$key <- paste(dat_wek$key, '_', dat_wek$year, sep = '')
# dat_wek <- merge(dat_wek, dat_dwd_m, by = 'key')
# head(dat_wek)



# # change Treat to L, M, H
# dat_fix_SG$Treat <- replace(dat_fix_SG$Treat, dat_fix_SG$Treat == 'A-Grad', 'L')
# dat_fix_SG$Treat <- replace(dat_fix_SG$Treat, dat_fix_SG$Treat == 'F-Grad', 'M')
# dat_fix_SG$Treat <- replace(dat_fix_SG$Treat, dat_fix_SG$Treat == 'Z-Baum', 'H')
# 
# dat_fix_DL$Treat <- replace(dat_fix_DL$Treat, dat_fix_DL$Treat == 'A-Grad', 'L')
# dat_fix_DL$Treat <- replace(dat_fix_DL$Treat, dat_fix_DL$Treat == 'F-Grad', 'M')
# dat_fix_DL$Treat <- replace(dat_fix_DL$Treat, dat_fix_DL$Treat == 'Z-Baum', 'H')
# 
# dat_wek$Treat <- replace(dat_wek$Treat, dat_wek$Treat == 'A-Grad', 'L')
# dat_wek$Treat <- replace(dat_wek$Treat, dat_wek$Treat == 'F-Grad', 'M')
# dat_wek$Treat <- replace(dat_wek$Treat, dat_wek$Treat == 'Z-Baum', 'H')

# normalize Temperature & DWD
dat_fix_SG$spr_temp <- scale(dat_fix_SG$spr_temp)
dat_fix_SG$sum_temp <- scale(dat_fix_SG$sum_temp)
dat_fix_SG$aut_temp <- scale(dat_fix_SG$aut_temp)
dat_fix_SG$win_temp <- scale(dat_fix_SG$win_temp)
dat_fix_SG$STEBO <- scale(dat_fix_SG$STEBO)
dat_fix_SG$STEBV <- scale(dat_fix_SG$STEBV)
dat_fix_SG$DWD_LOS <- scale(dat_fix_SG$DWD_LOS)

dat_fix_DL$spr_temp <- scale(dat_fix_DL$spr_temp)
dat_fix_DL$sum_temp <- scale(dat_fix_DL$sum_temp)
dat_fix_DL$aut_temp <- scale(dat_fix_DL$aut_temp)
dat_fix_DL$win_temp <- scale(dat_fix_DL$win_temp)
dat_fix_DL$STEBO <- scale(dat_fix_DL$STEBO)
dat_fix_DL$STEBV <- scale(dat_fix_DL$STEBV)
dat_fix_DL$DWD_LOS <- scale(dat_fix_DL$DWD_LOS)

# dat_wek$spr_temp <- scale(dat_wek$spr_temp)
# dat_wek$sum_temp <- scale(dat_wek$sum_temp)
# dat_wek$aut_temp <- scale(dat_wek$aut_temp)
# dat_wek$win_temp <- scale(dat_wek$win_temp)
# dat_wek$STEBO <- scale(dat_wek$STEBO)
# dat_wek$STEBV <- scale(dat_wek$STEBV)
# dat_wek$DWD_LOS <- scale(dat_wek$DWD_LOS)
# 

# add weight column
# dat_fix_VPP <- dat_wek
data_names <- c('dat_fix_SG', 'dat_fix_DL')
abbr_names <- c('SG', 'DL')

for(data_i in 1:length(data_names)){
  eval(parse(text = paste('dat <- ', data_names[data_i], sep = '')))
  count <- dat  %>% 
    group_by(Treat = Treat)  %>% 
    count()
  count
  sum <- sum(count$n)
  
  W_A <- sum/count$n[1]
  W_F <- sum/count$n[2]
  W_Z <- sum/count$n[3]
  
  dat$weight[dat$Treat == 'A-Grad'] <- W_A
  dat$weight[dat$Treat == 'F-Grad'] <- W_F
  dat$weight[dat$Treat == 'Z-Baum'] <- W_Z
  
  eval(parse(text = paste(data_names[data_i], '<- dat', sep = '')))
}

head(dat_fix_SG)
head(dat_fix_DL)
# head(dat_fix_VPP)


# dat_fix_DL$sosv <- dat_fix_DL$sos_mod_v
# dat_fix_SG$sosv <- dat_fix_SG$sos_mod_v
# dat_fix_DL$eosv <- dat_fix_DL$eos_mod_v
# dat_fix_SG$eosv <- dat_fix_SG$eos_mod_v
# dat_fix_DL$maxv <- dat_fix_DL$max
# dat_fix_SG$maxv <- dat_fix_SG$max


################################################################################

###################
### define data ###
###################


data_names <- c('dat_fix_SG', 'dat_fix_DL')
title_names <- c('Savitzy-Golay', 'DLogistic')
abbr_names <- c('SG', 'DL')

data_i <- 1
eval(parse(text = paste('dat_def <- ', data_names[data_i], sep = '')))
head(dat_def)

library(olsrr)
model <- lm(D306 ~ Treat, data = dat_def)
plot(model)

ols_test_normality(model)
ols_test_correlation(model)
ols_plot_resid_fit(model)
ols_plot_resid_hist(model)

dat_merge <- dat_def

mixed.lmer <- lmer(sos ~ Treat + north + spr_temp + STEBO + (1|year), weight = weight, data = dat_merge)  # intercept
summary(mixed.lmer)

r.squaredGLMM(mixed.lmer)

summary(mixed.lmer)[[5]]
summary(mixed.lmer)$coefficients

inter <- summary(mixed.lmer)$coefficients[1,1]
p.value <- summary(mixed.lmer)$coefficients[1,5]
if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "")
int_p <- pvalue

F_Grad <- summary(mixed.lmer)$coefficients[2,1]
p.value <- summary(mixed.lmer)$coefficients[2,5]
if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "")
F_p <- pvalue

Z_Baum <- summary(mixed.lmer)$coefficients[3,1]
p.value <- summary(mixed.lmer)$coefficients[3,5]
if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "")
Z_p <- pvalue

north <- summary(mixed.lmer)$coefficients[4,1]
p.value <- summary(mixed.lmer)$coefficients[4,5]
if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "")
north_p <- pvalue

spr_temp <- summary(mixed.lmer)$coefficients[5,1]
p.value <- summary(mixed.lmer)$coefficients[5,5]
if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "")
spr_p <- pvalue

aut_temp <- NA
aut_p <- NA

DWD <- summary(mixed.lmer)$coefficients[6,1]
p.value <- summary(mixed.lmer)$coefficients[6,5]
if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "")
DWD_p <- pvalue


new <- data.frame(Y = 'sos', data =  abbr_names[data_i], intercept = inter, int_p = int_p,
                  F_Grad = F_Grad, F_p = F_p, Z_Baum = Z_Baum, Z_p = Z_p,
                  north = north,north_p = north_p, spr_temp = spr_temp, spr_p = spr_p,
                  aut_temp = aut_temp, aut_p = aut_p, DWD = DWD, DWD_p = DWD_p)


if (data_i == 1){
  mod_summary <- new
}else{
  mod_summary <- rbind(mod_summary, new)
}



mixed.lmer <- lmer(los ~ Treat + north + spr_temp +  aut_temp + DWD_LOS  + (1|year), data = dat_merge)  # intercept
summary(mixed.lmer)

summary(mixed.lmer)
summary(mixed.lmer)$coefficients[7,1]

#### calculate SD of each index

key_list <- unique(dat_merge$key)

for (key_i in 1:length(key_list)){
  key_n <- key_list[key_i]
  sub_key <- subset(dat_merge, key == key_n)
  plot <- unique(sub_key$plot)
  par <- unique(sub_key$parcel)
  treat <- unique(sub_key$Treat)
  age <- unique(sub_key$Age)
  yr <- unique(sub_key$year)
  north <- unique(sub_key$north)
  STEBO <- unique(sub_key$STEBO)
  STEBV <- unique(sub_key$STEBV)
  DWD_LOS <- unique(sub_key$DWD_LOS)
  spr_temp <- unique(sub_key$spr_temp)
  sum_temp <- unique(sub_key$sum_temp)
  aut_temp <- unique(sub_key$aut_temp)
  win_temp <- unique(sub_key$win_temp)
  
  sub_yr <- subset(sub_key, year == yr)
  
  # calculate sd of each index
  sd_D106 <- sd(sub_yr$D106)
  sd_D120 <- sd(sub_yr$D120)
  sd_D168 <- sd(sub_yr$D168)
  sd_D292 <- sd(sub_yr$D292)
  sd_D306<- sd(sub_yr$D306)
  
  # calculate mean of each index
  m_D106 <- mean(sub_yr$D106)
  m_D120 <- mean(sub_yr$D120)
  m_D168 <- mean(sub_yr$D168)
  m_D292 <- mean(sub_yr$D292)
  m_D306 <- mean(sub_yr$D306)
  
  new <- data.frame(key = key_n, plot = plot, parcel = par, year = yr, Treat = treat, Age = age, 
                    north = north, STEBO = STEBO, STEBV = STEBV, DWD_LOS = DWD_LOS, 
                    spr_temp = spr_temp, sum_temp = sum_temp, aut_temp = aut_temp, 
                    sd_D106 = sd_D106, sd_D120 = sd_D120, sd_D168 = sd_D168, sd_D292 = sd_D292, sd_D306 = sd_D306,
                    m_D106 = m_D106, m_D120 = m_D120, m_D168 = m_D168, m_D292 = m_D292, m_D306 = m_D306)
  
  if (key_i == 1){
    dat_key <- new
  }else{
    dat_key <- rbind(dat_key, new)
  }
  
}

model <- lm(sd_D106 ~ Treat, data = dat_key)
plot(model)

ols_test_normality(model)
ols_test_correlation(model)
ols_plot_resid_fit(model)
ols_plot_resid_hist(model)

################################################################################

####################
####### Loop #######
####################


data_names <- c('dat_fix_SG', 'dat_fix_DL')
title_names <- c('Savitzy-Golay', 'DLogistic')
abbr_names <- c('SG', 'DL')

for(data_i in 1:2){
  eval(parse(text = paste('dat_merge <- ', data_names[data_i], sep = '')))
  
  
  
  #### calculate SD of each index
  
  key_list <- unique(dat_merge$key)
  
  for (key_i in 1:length(key_list)){
    key_n <- key_list[key_i]
    sub_key <- subset(dat_merge, key == key_n)
    plot <- unique(sub_key$plot)
    par <- unique(sub_key$parcel)
    treat <- unique(sub_key$Treat)
    age <- unique(sub_key$Age)
    yr <- unique(sub_key$year)
    north <- unique(sub_key$north)
    STEBO <- unique(sub_key$STEBO)
    STEBV <- unique(sub_key$STEBV)
    DWD_LOS <- unique(sub_key$DWD_LOS)
    spr_temp <- unique(sub_key$spr_temp)
    sum_temp <- unique(sub_key$sum_temp)
    aut_temp <- unique(sub_key$aut_temp)
    win_temp <- unique(sub_key$win_temp)
    
    sub_yr <- subset(sub_key, year == yr)
    # count number of non-NA pixels
    # pix_D106 <- sum(!is.na(sub_yr$D106))
    # pix_D120 <- sum(!is.na(sub_yr$D120))
    # pix_D168 <- sum(!is.na(sub_yr$D168))
    # 
    # calculate sd of each index
    sd_D106 <- sd(sub_yr$D106)
    sd_D120 <- sd(sub_yr$D120)
    sd_D168 <- sd(sub_yr$D168)
    sd_D292 <- sd(sub_yr$D292)
    sd_D306<- sd(sub_yr$D306)
    
    # calculate mean of each index
    m_D106 <- mean(sub_yr$D106)
    m_D120 <- mean(sub_yr$D120)
    m_D168 <- mean(sub_yr$D168)
    m_D292 <- mean(sub_yr$D292)
    m_D306 <- mean(sub_yr$D306)

    
    neww <- data.frame(key = key_n, plot = plot, parcel = par, year = yr, Treat = treat, Age = age, 
                       north = north, STEBO = STEBO, STEBV = STEBV, DWD_LOS = DWD_LOS, 
                       spr_temp = spr_temp, sum_temp = sum_temp, aut_temp = aut_temp, 
                       sd_D106 = sd_D106, sd_D120 = sd_D120, sd_D168 = sd_D168, sd_D292 = sd_D292, sd_D306 = sd_D306,
                       m_D106 = m_D106, m_D120 = m_D120, m_D168 = m_D168, m_D292 = m_D292, m_D306 = m_D306)
    
    if (key_i == 1){
      dat_key <- neww
    }else{
      dat_key <- rbind(dat_key, neww)
    }
    
  }
  
  # dat_key$log_sds <- log(dat_key$sd_sos)
  # dat_key$log_sde <- log(dat_key$sd_eos)
  # dat_key$log_sdl <- log(dat_key$sd_los)
  # dat_key$log_sdsv <- dat_key$sd_sosv
  # dat_key$log_sdev <- dat_key$sd_eosv
  
  dat_test <- dat_key
  head(dat_key)
  
  # ### random select 1/3 of Roh 635
  # 
  # dat_roh635 <- subset(dat_merge, plot == 'Roh635')
  # 
  # dat_roh635_1 <- subset(dat_roh635, parcel == 1)
  # dat_roh635_2 <- subset(dat_roh635, parcel == 2)
  # 
  # dat_other <- subset(dat_merge, plot != 'Roh635')
  # head(dat_other)
  # 
  # dat_samp1 <- dat_roh635_1 %>% sample_frac(0.33)
  # head(dat_samp1)
  # dat_samp2 <- dat_roh635_2 %>% sample_frac(0.33)
  # head(dat_samp2)
  # 
  # dat_samp <- rbind(dat_samp1, dat_samp2)
  # dat_merge <- rbind(dat_other, dat_samp)
  
  
  ### model 5 fixed days
  var_names <- c('D106', 'D120', 'D168', 'D292', 'D306')
  for (i in 1:5){
    var <- var_names[i]
    
    # weight
    if (i == 1){
      eval(parse(text = paste('mixed.lmer <- lmer(', var, ' ~ Treat + north + spr_temp + STEBO  + (1|year), weight = weight, data = dat_merge)', sep = '')))
    }else if (i == 2){
      eval(parse(text = paste('mixed.lmer <- lmer(', var, ' ~ Treat + north + spr_temp + STEBO  + (1|year), weight = weight, data = dat_merge)', sep = '')))
    }else if (i == 3){
      eval(parse(text = paste('mixed.lmer <- lmer(', var, ' ~ Treat + north + sum_temp + DWD_LOS + (1|year), weight = weight, data = dat_merge)', sep = '')))
    }else {
      eval(parse(text = paste('mixed.lmer <- lmer(', var, ' ~ Treat + north + aut_temp + STEBV  + (1|year), weight = weight, data = dat_merge)', sep = '')))
    }
    
    
    inter <- summary(mixed.lmer)$coefficients[1,1]
    p.value <- summary(mixed.lmer)$coefficients[1,5]
    if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
    int_p <- pvalue
    
    F_Grad <- summary(mixed.lmer)$coefficients[2,1]
    p.value <- summary(mixed.lmer)$coefficients[2,5]
    if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
    F_p <- pvalue
    
    Z_Baum <- summary(mixed.lmer)$coefficients[3,1]
    p.value <- summary(mixed.lmer)$coefficients[3,5]
    if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
    Z_p <- pvalue
    
    north <- summary(mixed.lmer)$coefficients[4,1]
    p.value <- summary(mixed.lmer)$coefficients[4,5]
    if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
    north_p <- pvalue
    
    temp <- summary(mixed.lmer)$coefficients[5,1]
    if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
    temp_p <- pvalue
    
    DWD <- summary(mixed.lmer)$coefficients[6,1]
    p.value <- summary(mixed.lmer)$coefficients[6,5]
    if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
    DWD_p <- pvalue
    
    new <- data.frame(Y = var, data =  abbr_names[data_i], intercept = inter, int_p = int_p,
                      M = F_Grad, M_p = F_p, H = Z_Baum, H_p = Z_p,
                      north = north,north_p = north_p, temp = temp, temp_p = temp_p,
                      DWD = DWD, DWD_p = DWD_p)
    if (data_i == 1){
      if(i == 1){
        mod_summary <- new
      }else{
        mod_summary <- rbind(mod_summary, new)
      }
    }else{
        mod_summary <- rbind(mod_summary, new)
    }
  }
  
  
  # model sd of 5 days
  var_names <- c('D106', 'D120', 'D168', 'D292', 'D306')
  for (j in 1:5){
    var <- var_names[j]
    if (j == 1){
      eval(parse(text = paste('mixed.lmer <- lmer(sd_', var, ' ~ Treat + north + spr_temp + STEBO  + (1|year), data = dat_test)', sep = '')))
    }else if (j == 2){
      eval(parse(text = paste('mixed.lmer <- lmer(sd_', var, ' ~ Treat + north + spr_temp + STEBO  + (1|year), data = dat_test)', sep = '')))
    }else if (j == 3){
      eval(parse(text = paste('mixed.lmer <- lmer(sd_', var, ' ~ Treat + north + sum_temp + DWD_LOS  + (1|year), data = dat_test)', sep = '')))
    }else{
      eval(parse(text = paste('mixed.lmer <- lmer(sd_', var, ' ~ Treat + north + aut_temp + STEBV  + (1|year), data = dat_test)', sep = '')))
    }
    
    inter <- summary(mixed.lmer)$coefficients[1,1]
    p.value <- summary(mixed.lmer)$coefficients[1,5]
    if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
    int_p <- pvalue
    
    F_Grad <- summary(mixed.lmer)$coefficients[2,1]
    p.value <- summary(mixed.lmer)$coefficients[2,5]
    if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
    F_p <- pvalue
    
    Z_Baum <- summary(mixed.lmer)$coefficients[3,1]
    p.value <- summary(mixed.lmer)$coefficients[3,5]
    if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
    Z_p <- pvalue
    
    north <- summary(mixed.lmer)$coefficients[4,1]
    p.value <- summary(mixed.lmer)$coefficients[4,5]
    if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
    north_p <- pvalue
    
    temp <- summary(mixed.lmer)$coefficients[5,1]
    p.value <- summary(mixed.lmer)$coefficients[5,5]
    if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
    temp_p <- pvalue
    
    DWD <- summary(mixed.lmer)$coefficients[6,1]
    p.value <- summary(mixed.lmer)$coefficients[6,5]
    if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
    DWD_p <- pvalue
    
 
    new <- data.frame(Y = var, data =  abbr_names[data_i], intercept = inter, int_p = int_p,
                      M = F_Grad, M_p = F_p, H = Z_Baum, H_p = Z_p,
                      north = north,north_p = north_p, temp = temp, temp_p = temp_p,
                      DWD = DWD, DWD_p = DWD_p)
    
    mod_summary <- rbind(mod_summary, new)
    
  }
  
}


mod_summary

write.csv(mod_summary, 'mod_sum_fix_day.csv', row.names = FALSE)



library(dplyr)
library(ggpubr)
library(ggplot2)

setwd('D:/GIS_Data/Vfl-oak/GEEMap/')
field_names <- c('Gei', 'Loh', 'Roh90', 'Roh620', 'Roh635')


dat_meta <- read.csv('meta_data.csv')
dat_meta

#### Load data SG
dat_pheno_SG <- read.csv('dat_pheno_pix_SG_impu.csv')
dat_pheno_SG$key <- paste(dat_pheno_SG$plot, '_', dat_pheno_SG$parcel, sep = '')
dat_pheno_SG <- merge(dat_pheno_SG, dat_meta, by = 'key')
# library(naniar)
dat_pheno_SG <- dat_pheno_SG %>% replace_with_na_at(.vars = 'eos', condition = ~.x < 210) # remove outliers in eos, DL
head(dat_pheno_SG)

head(dat_pheno_SG)

dat <- dat_pheno_SG
count <- dat  %>% 
  group_by(Treat = Treat)  %>% 
  count()
count
# # remove sos in 2017
# dat_pheno_SG$sos <- replace(dat_pheno_SG$sos, dat_pheno_SG$year == 2017, 'NA')
# head(dat_pheno_SG)

# merge plots into two: North & Middle
dat_pheno_SG$north <- dat_pheno_SG$plot
dat_pheno_SG$north <- replace(dat_pheno_SG$north, dat_pheno_SG$north == 'Loh', 'Roh')
dat_pheno_SG$north <- replace(dat_pheno_SG$north, dat_pheno_SG$north == 'Roh620', 'Roh')
dat_pheno_SG$north <- replace(dat_pheno_SG$north, dat_pheno_SG$north == 'Roh635', 'Roh')
dat_pheno_SG$north <- replace(dat_pheno_SG$north, dat_pheno_SG$north == 'Roh90', 'Roh')
dat_pheno_SG$north <- replace(dat_pheno_SG$north, dat_pheno_SG$north == 'Roh', 'North')
dat_pheno_SG$north <- replace(dat_pheno_SG$north, dat_pheno_SG$north == 'Gei', 'Middle')
dat_pheno_SG$north

#### Load data DL
dat_pheno_DL <- read.csv('dat_pheno_pix_DL.csv')
dat_pheno_DL$key <- paste(dat_pheno_DL$plot, '_', dat_pheno_DL$parcel, sep = '')
dat_pheno_DL <- merge(dat_pheno_DL, dat_meta, by = 'key')
# library(naniar)
dat_pheno_DL <- dat_pheno_DL %>% replace_with_na_at(.vars = 'eos', condition = ~.x < 210) # remove outliers in eos, DL
head(dat_pheno_DL)

# merge plots into two: North & Middle
dat_pheno_DL$north <- dat_pheno_DL$plot
dat_pheno_DL$north <- replace(dat_pheno_DL$north, dat_pheno_DL$north == 'Loh', 'Roh')
dat_pheno_DL$north <- replace(dat_pheno_DL$north, dat_pheno_DL$north == 'Roh620', 'Roh')
dat_pheno_DL$north <- replace(dat_pheno_DL$north, dat_pheno_DL$north == 'Roh635', 'Roh')
dat_pheno_DL$north <- replace(dat_pheno_DL$north, dat_pheno_DL$north == 'Roh90', 'Roh')
dat_pheno_DL$north <- replace(dat_pheno_DL$north, dat_pheno_DL$north == 'Roh', 'North')
dat_pheno_DL$north <- replace(dat_pheno_DL$north, dat_pheno_DL$north == 'Gei', 'Middle')
dat_pheno_DL$north

#### Load data WekEO VPP
dat_wek <- read.csv('WekEO.csv')
dat_wek$key <- paste(dat_wek$ID, '_', dat_wek$Parcel, sep = '')
dat_wek <- merge(dat_wek, dat_meta, by = 'key')
head(dat_wek)
colnames(dat_wek) <- c('key', 'plot', 'field', 'parcel', 'interne', 'year', 'pix', 'eos', 'eosv', 'sos', 'sosv', 'max', 'maxv', 'Treat', 'Age')
# library(naniar)
dat_wek <- dat_wek %>% replace_with_na_at(.vars = 'eos', condition = ~.x < 210) # remove outliers in eos, DL
head(dat_wek)

dat_wek$los <- dat_wek$eos - dat_wek$sos
head(dat_wek)

# merge plots into two: North & Middle
dat_wek$north <- dat_wek$plot
dat_wek$north <- replace(dat_wek$north, dat_wek$north == 'Loh', 'Roh')
dat_wek$north <- replace(dat_wek$north, dat_wek$north == 'Roh620', 'Roh')
dat_wek$north <- replace(dat_wek$north, dat_wek$north == 'Roh635', 'Roh')
dat_wek$north <- replace(dat_wek$north, dat_wek$north == 'Roh90', 'Roh')
dat_wek$north <- replace(dat_wek$north, dat_wek$north == 'Roh', 'North')
dat_wek$north <- replace(dat_wek$north, dat_wek$north == 'Gei', 'Middle')
head(dat_wek)


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

dat_pheno_SG$key <- paste(dat_pheno_SG$key, '_', dat_pheno_SG$year, sep = '')
dat_pheno_SG <- merge(dat_pheno_SG, dat_dwd_m, by = 'key')
head(dat_pheno_SG)

dat_pheno_DL$key <- paste(dat_pheno_DL$key, '_', dat_pheno_DL$year, sep = '')
dat_pheno_DL <- merge(dat_pheno_DL, dat_dwd_m, by = 'key')
head(dat_pheno_DL)

dat_wek$key <- paste(dat_wek$key, '_', dat_wek$year, sep = '')
dat_wek <- merge(dat_wek, dat_dwd_m, by = 'key')
head(dat_wek)



# # change Treat to L, M, H
# dat_pheno_SG$Treat <- replace(dat_pheno_SG$Treat, dat_pheno_SG$Treat == 'A-Grad', 'L')
# dat_pheno_SG$Treat <- replace(dat_pheno_SG$Treat, dat_pheno_SG$Treat == 'F-Grad', 'M')
# dat_pheno_SG$Treat <- replace(dat_pheno_SG$Treat, dat_pheno_SG$Treat == 'Z-Baum', 'H')
# 
# dat_pheno_DL$Treat <- replace(dat_pheno_DL$Treat, dat_pheno_DL$Treat == 'A-Grad', 'L')
# dat_pheno_DL$Treat <- replace(dat_pheno_DL$Treat, dat_pheno_DL$Treat == 'F-Grad', 'M')
# dat_pheno_DL$Treat <- replace(dat_pheno_DL$Treat, dat_pheno_DL$Treat == 'Z-Baum', 'H')
# 
# dat_wek$Treat <- replace(dat_wek$Treat, dat_wek$Treat == 'A-Grad', 'L')
# dat_wek$Treat <- replace(dat_wek$Treat, dat_wek$Treat == 'F-Grad', 'M')
# dat_wek$Treat <- replace(dat_wek$Treat, dat_wek$Treat == 'Z-Baum', 'H')

# normalize Temperature & DWD
dat_pheno_SG$spr_temp <- scale(dat_pheno_SG$spr_temp)
dat_pheno_SG$sum_temp <- scale(dat_pheno_SG$sum_temp)
dat_pheno_SG$aut_temp <- scale(dat_pheno_SG$aut_temp)
dat_pheno_SG$win_temp <- scale(dat_pheno_SG$win_temp)
dat_pheno_SG$STEBO <- scale(dat_pheno_SG$STEBO)
dat_pheno_SG$STEBV <- scale(dat_pheno_SG$STEBV)
dat_pheno_SG$DWD_LOS <- scale(dat_pheno_SG$DWD_LOS)

dat_pheno_DL$spr_temp <- scale(dat_pheno_DL$spr_temp)
dat_pheno_DL$sum_temp <- scale(dat_pheno_DL$sum_temp)
dat_pheno_DL$aut_temp <- scale(dat_pheno_DL$aut_temp)
dat_pheno_DL$win_temp <- scale(dat_pheno_DL$win_temp)
dat_pheno_DL$STEBO <- scale(dat_pheno_DL$STEBO)
dat_pheno_DL$STEBV <- scale(dat_pheno_DL$STEBV)
dat_pheno_DL$DWD_LOS <- scale(dat_pheno_DL$DWD_LOS)

dat_wek$spr_temp <- scale(dat_wek$spr_temp)
dat_wek$sum_temp <- scale(dat_wek$sum_temp)
dat_wek$aut_temp <- scale(dat_wek$aut_temp)
dat_wek$win_temp <- scale(dat_wek$win_temp)
dat_wek$STEBO <- scale(dat_wek$STEBO)
dat_wek$STEBV <- scale(dat_wek$STEBV)
dat_wek$DWD_LOS <- scale(dat_wek$DWD_LOS)


# add weight column
dat_pheno_VPP <- dat_wek
data_names <- c('dat_pheno_SG', 'dat_pheno_DL', 'dat_pheno_VPP')
abbr_names <- c('SG', 'DL', 'VPP')

for(data_i in 1:3){
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

head(dat_pheno_SG)
head(dat_pheno_DL)
head(dat_pheno_VPP)


dat_pheno_DL$sosv <- dat_pheno_DL$sos_mod_v
dat_pheno_SG$sosv <- dat_pheno_SG$sos_mod_v
dat_pheno_DL$eosv <- dat_pheno_DL$eos_mod_v
dat_pheno_SG$eosv <- dat_pheno_SG$eos_mod_v
dat_pheno_DL$maxv <- dat_pheno_DL$max
dat_pheno_SG$maxv <- dat_pheno_SG$max



################################################################################

###################
### define data ###
###  SG DL VPP  ###
###################

data_names <- c('dat_pheno_SG', 'dat_pheno_DL', 'dat_wek')
title_names <- c('Savitzy-Golay', 'DLogistic', 'WekEO-VPP')
abbr_names <- c('SG', 'DL', 'VPP')
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

# ### check data
# set.seed(1234)
# dplyr::sample_n(dat_merge, 10)

### density plot
p <-  ggdensity(dat_merge$sos, 
          main = paste('Density of SOS, ', title_names[data_i], sep = ''),
          xlab = "DOY") +
  theme_classic(base_size = 20)
p

### QQ plot
q <- ggqqplot(dat_merge$sos,
         main = paste('QQPlot of SOS, ', title_names[data_i], sep = '')) +
  theme_classic(base_size = 20)
q


### Shapiro-Wilk test of normality for one variable
### p-value > 0.05 assumes normal distribution

sha.test <- shapiro.test(dat_merge$sos)
if (sha.test$p.value > 0.05) {norm <- "normal distribution"} else{norm <- "non-normal distribution"}
norm
sha.test$p.value

library(olsrr)
model <- lm(sos ~ Treat, data = dat_merge)
plot(model)

anova(model)
summary(model)

ols_test_normality(model)
ols_test_correlation(model)
ols_plot_resid_fit(model)
ols_plot_resid_hist(model)

# 
# mod2 <- lm(sos ~ Treat, data = dat_merge)
# plot(mod2)

### Kruskal-Wallis Test

dat_merge$Treat <- as.factor(dat_merge$Treat)
levels(dat_merge$Treat)

# dat_merge$group <- ordered(dat_merge$Treat,
#                          levels = c("A-Grad", "F-Grad", "Z-Baum"))

#### Statistics SOS, EOS, LOS

group_by(dat_merge, Treat) %>%
  summarise(
    count = n(),
    mean = mean(sos, na.rm = TRUE),
    sd = sd(sos, na.rm = TRUE),
    median = median(sos, na.rm = TRUE),
    IQR = IQR(sos, na.rm = TRUE)
  )

group_by(dat_merge, Treat) %>%
  summarise(
    count = n(),
    mean = mean(eos, na.rm = TRUE),
    sd = sd(eos, na.rm = TRUE),
    median = median(eos, na.rm = TRUE),
    IQR = IQR(eos, na.rm = TRUE)
  )

group_by(dat_merge, Treat) %>%
  summarise(
    count = n(),
    mean = mean(los, na.rm = TRUE),
    sd = sd(los, na.rm = TRUE),
    median = median(los, na.rm = TRUE),
    IQR = IQR(los, na.rm = TRUE)
  )



dat_merge$log_sos <- log(dat_merge$sos)
dat_merge$log_eos <- log(dat_merge$eos)
dat_merge$log_los <- log(dat_merge$los)


### density plot
p <-  ggdensity(dat_merge$log_sos, 
                main = paste('Density of Log(SOS), ', title_names[data_i], sep = ''),
                xlab = "DOY") +
  theme_classic(base_size = 20)
p

### QQ plot
q <- ggqqplot(dat_merge$log_sos,
              main = paste('QQPlot of Log(SOS), ', title_names[data_i], sep = '')) +
  theme_classic(base_size = 20)
q


### Shapiro-Wilk test of normality for one variable
### p-value > 0.05 assumes normal distribution

sha.test <- shapiro.test(dat_merge$log_sos)
if (sha.test$p.value > 0.05) {norm <- "normal distribution"} else{norm <- "non-normal distribution"}
norm



my_comparisons <- list( c("A-Grad", "F-Grad"), c("F-Grad", "Z-Baum"), c("A-Grad", "Z-Baum") )

p_sos <-  ggboxplot(dat_merge, x = "Treat", y = "sos", 
                     color = "Treat", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                     order = c("A-Grad", "F-Grad", "Z-Baum"), add = "jitter",
                     ylab = "SOS", xlab = "Treatment", main = paste('Boxplot SOS', abbr_names[data_i], sep = ' '))+ 
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", label.y = c(160, 160, 165), size = 5)+ # Add pairwise comparisons p-value
  stat_compare_means(method = "anova", label.y = 175, size = 5) +    # Add global anova p-value
  theme_classic(base_size = 20)
p_sos

p_eos <-  ggboxplot(dat_merge, x = "Treat", y = "eos", 
                    color = "Treat", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                    order = c("A-Grad", "F-Grad", "Z-Baum"), add = "jitter",
                    ylab = "EOS", xlab = "Treatment", main = paste('Boxplot EOS', abbr_names[data_i], sep = ' '))+ 
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", label.y = c(360, 360, 370), size = 5)+ # Add pairwise comparisons p-value
  stat_compare_means(method = "anova", label.y = 385, size = 5) +    # Add global anova p-value
  theme_classic(base_size = 20)
p_eos

p_los <-  ggboxplot(dat_merge, x = "Treat", y = "los", 
                    color = "Treat", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                    order = c("A-Grad", "F-Grad", "Z-Baum"), add = "jitter",
                    ylab = "LOS", xlab = "Treatment", main = paste('Boxplot LOS', abbr_names[data_i], sep = ' '))+ 
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", label.y = c(225, 225, 235), size = 5)+ # Add pairwise comparisons p-value
  stat_compare_means(method = "anova", label.y = 250, size = 5) +    # Add global anova p-value
  theme_classic(base_size = 20)
p_los









#### calculate SD of each index

key_list <- unique(dat_merge$key)

for (key_i in 1:length(key_list)){
  key_n <- key_list[key_i]
  sub_key <- subset(dat_merge, key == key_n)
  plot <- unique(sub_key$plot)
  par <- unique(sub_key$parcel)
  treat <- unique(sub_key$Treat)
  age <- unique(sub_key$Age)
  for (yr in 2017:2022){
    sub_yr <- subset(sub_key, year == yr)
    # count number of non-NA pixels
    pix_sos <- sum(!is.na(sub_yr$sos))
    pix_eos <- sum(!is.na(sub_yr$eos))
    pix_los <- sum(!is.na(sub_yr$los))
    
    # calculate sd of each index
    sd_sos <- sd(sub_yr$sos)
    sd_eos <- sd(sub_yr$eos)
    sd_los <- sd(sub_yr$los)
    sd_sosv <- sd(sub_yr$sosv)
    sd_eosv <- sd(sub_yr$eosv)
    
    # calculate mean of each index
    m_sos <- mean(sub_yr$sos)
    m_eos <- mean(sub_yr$eos)
    m_los <- mean(sub_yr$los)
    m_sosv <- mean(sub_yr$sosv)
    m_eosv <- mean(sub_yr$eosv)
    
    # calculate CV (Coefficient of Variation)
    cv_sos <- sd_sos/m_sos
    cv_eos <- sd_eos/m_eos
    cv_los <- sd_los/m_los
    
    new <- data.frame(key = key_n, plot = plot, parcel = par, year = yr, Treat = treat, Age = age, 
                      pix_sos = pix_sos, pix_eos = pix_eos, pix_los = pix_los, 
                      m_sos = m_sos, m_eos = m_eos, m_los = m_los, m_sosv = m_sosv, m_eosv = m_eosv,
                      sd_sos = sd_sos, sd_eos = sd_eos, sd_los = sd_los, sd_sosv = sd_sosv, sd_eosv = sd_eosv,
                      cv_sos = cv_sos, cv_eos = cv_eos, cv_los = cv_los)
    if (yr == 2017){
      dat_yr <- new
    }else{
      dat_yr <- rbind(dat_yr, new)
    }
  }
  if (key_i == 1){
    dat_key <- dat_yr
  }else{
    dat_key <- rbind(dat_key, dat_yr)
  }
  
}

dat_key
summary(dat_key)


### density plot
ggdensity(dat_key$sd_sos, 
          main = paste('Density of SD SOS, ', title_names[data_i], sep = ''),
          xlab = "DOY") +
  theme_classic(base_size = 20)

ggqqplot(dat_key$sd_sosv, 
         main = paste('QQ Plot of SD SOS, ', title_names[data_i], sep = '')) +
  theme_classic(base_size = 20)

### Shapiro-Wilk test of normality for one variable
### p-value > 0.05 assumes normal distribution

sha.test <- shapiro.test(dat_key$sd_sos)
if (sha.test$p.value > 0.05) {norm <- "normal distribution"} else{norm <- "non-normal distribution"}
norm

dat_key$log_sds <- log(dat_key$sd_sos)
dat_key$log_sde <- log(dat_key$sd_eos)
dat_key$log_sdl <- log(dat_key$sd_los)

dat_key$log_cvs <- log(dat_key$cv_sos)
dat_key$log_cve <- log(dat_key$cv_eos)
dat_key$log_cvl <- log(dat_key$cv_los)


ggdensity(dat_key$log_sds, 
          main = paste('Density of log(SD SOS), ', title_names[data_i], sep = ''),
          xlab = "DOY") +
  theme_classic(base_size = 20)

ggqqplot(dat_key$log_sds, 
         main = paste('QQ Plot of SD SOS, ', title_names[data_i], sep = '')) +
  theme_classic(base_size = 20)

sha.test <- shapiro.test(dat_key$log_cvl)
if (sha.test$p.value > 0.05) {norm <- "normal distribution"} else{norm <- "non-normal distribution"}
norm
sha.test$p.value

library(olsrr)
model <- lm(log(sd_sos) ~ Treat, data = dat_key)
plot(model)

ols_test_normality(model)
ols_test_correlation(model)
ols_plot_resid_fit(model)
ols_plot_resid_hist(model)

# dat_key_sos <- dat_key_sos[dat_key_sos$year != 2017,]
# compare_means(sd_sos ~ Treat, data = dat_key_sos)

# p <- ggboxplot(dat_key_sos, x = "Treat", y = "sd_sos", 
#           color = "Treat", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
#           order = c("A-Grad", "F-Grad", "Z-Baum"), add = "jitter",
#           ylab = "SD of SOS", xlab = "Treatment", main = paste('Boxplot SD of SOS', 'SG', sep = ' '))
# p + stat_compare_means()

my_comparisons <- list( c("A-Grad", "F-Grad"), c("F-Grad", "Z-Baum"), c("A-Grad", "Z-Baum") )

# p_sd_s <-  ggboxplot(dat_key, x = "Treat", y = "sd_sos", 
#           color = "Treat", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
#           order = c("A-Grad", "F-Grad", "Z-Baum"), add = "jitter",
#           ylab = "SD of SOS", xlab = "Treatment", main = paste('Boxplot SD of SOS', abbr_names[data_i], sep = ' '))+ 
#   stat_compare_means(comparisons = my_comparisons, p.adjust.method = "bonferroni", label = "p.signif", label.y = c(8.5, 8.5, 9), size = 5)+ # Add pairwise comparisons p-value
#   stat_compare_means(label.y = 10, size = 5) +    # Add global p-value
#   theme_classic(base_size = 20)
# p_sd_s

library(rstatix)
pairwise.test = dat_key %>% 
  t_test(log_sds ~ Treat) %>% 
  adjust_pvalue(method = 'bonferroni')

p_sd_s <-  ggboxplot(dat_key, x = "Treat", y = "log_sds", 
                     color = "Treat", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                     order = c("A-Grad", "F-Grad", "Z-Baum"), add = "jitter",
                     ylab = "log(SD of SOS)", xlab = "Treatment", main = paste('Boxplot SD of SOS', abbr_names[data_i], sep = ' '))+ 
  stat_compare_means(comparisons = my_comparisons, p.adjust.method = "bonferroni", method = "t.test", label = "p.signif", label.y = c(2.5, 2.5, 3), size = 5)+ # Add pairwise comparisons p-value
  stat_compare_means(method = "anova", label.sep = ", ", label.y = 4, size = 5) +    # Add global anova p-value
  theme_classic(base_size = 20)
p_sd_s

ggsave(paste('Boxplot_log_SD_SOS_', abbr_names[data_i], '.png', sep = ''), p_sd_s)

p_cv_s <-  ggboxplot(dat_key, x = "Treat", y = "log_cvs",
                     color = "Treat", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                     order = c("A-Grad", "F-Grad", "Z-Baum"), add = "jitter",
                     ylab = "log(CV of SOS)", xlab = "Treatment", main = paste('Boxplot CV of SOS', abbr_names[data_i], sep = ' '))+
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", label.y = c(-2, -2, -1.5), size = 5)+ # Add pairwise comparisons p-value
  stat_compare_means(method = "anova", label.y = -0.5, size = 5) +    # Add global anova p-value
  theme_classic(base_size = 20)
p_cv_s
ggsave(paste('Boxplot_log_CV_SOS_', abbr_names[data_i], '.png', sep = ''), p_cv_s)

# eos
# dat_key_eos <- dat_key[dat_key$pix_eos > 10, ]
# p_sd_e <-  ggboxplot(dat_key, x = "Treat", y = "sd_eos", 
#                      color = "Treat", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
#                      order = c("A-Grad", "F-Grad", "Z-Baum"), add = "jitter",
#                      ylab = "SD of EOS", xlab = "Treatment", main = paste('Boxplot SD of EOS', abbr_names[data_i], sep = ' '))+ 
#   stat_compare_means(comparisons = my_comparisons, label = "p.signif", label.y = c(17, 17, 18), size = 5)+ # Add pairwise comparisons p-value
#   stat_compare_means(label.y = 20, size = 5) +    # Add global p-value
#   theme_classic(base_size = 20)
# p_sd_e

p_sd_e <-  ggboxplot(dat_key, x = "Treat", y = "log_sde", 
                     color = "Treat", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                     order = c("A-Grad", "F-Grad", "Z-Baum"), add = "jitter",
                     ylab = "log(SD of EOS)", xlab = "Treatment", main = paste('Boxplot SD of EOS', abbr_names[data_i], sep = ' '))+ 
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", label.y = c(3.5, 3.5, 4), size = 5)+ # Add pairwise comparisons p-value
  stat_compare_means(method = "anova", label.y = 5, size = 5) +    # Add global anova p-value
  theme_classic(base_size = 20)
p_sd_e


ggsave(paste('Boxplot_log_SD_EOS_', abbr_names[data_i], '.png', sep = ''), p_sd_e)
# 
p_cv_e <-  ggboxplot(dat_key, x = "Treat", y = "log_cve",
                     color = "Treat", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                     order = c("A-Grad", "F-Grad", "Z-Baum"), add = "jitter",
                     ylab = "log(CV of EOS)", xlab = "Treatment", main = paste('Boxplot CV of EOS', abbr_names[data_i], sep = ' '))+
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", label.y = c(-2, -2, -1.5), size = 5)+ # Add pairwise comparisons p-value
  stat_compare_means(method = "anova", label.y = -0.5, size = 5) +    # Add global anova p-value
  theme_classic(base_size = 20)
p_cv_e
ggsave(paste('Boxplot_log_CV_EOS_', abbr_names[data_i], '.png', sep = ''), p_cv_e)

# los
# dat_key_los <- dat_key[dat_key$pix_los > 10, ]
p_sd_l <-  ggboxplot(dat_key, x = "Treat", y = "log_sdl", 
                     color = "Treat", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                     order = c("A-Grad", "F-Grad", "Z-Baum"), add = "jitter",
                     ylab = "log(SD of LOS)", xlab = "Treatment", main = paste('Boxplot SD of LOS', abbr_names[data_i], sep = ' '))+ 
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", label.y = c(4, 4, 4.5), size = 5)+ # Add pairwise comparisons p-value
  stat_compare_means(method = "anova", label.y = 5.5, size = 5) +    # Add global anova p-value
  theme_classic(base_size = 20)
p_sd_l

ggsave(paste('Boxplot_log_SD_LOS_', abbr_names[data_i], '.png', sep = ''), p_sd_l)

p_cv_l <-  ggboxplot(dat_key, x = "Treat", y = "log_cvl",
                     color = "Treat", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                     order = c("A-Grad", "F-Grad", "Z-Baum"), add = "jitter",
                     ylab = "log(CV of LOS)", xlab = "Treatment", main = paste('Boxplot CV of LOS', abbr_names[data_i], sep = ' '))+
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", label.y = c(-1.5, -1.5, -1), size = 5)+ # Add pairwise comparisons p-value
  stat_compare_means(method = "anova", label.y = -0, size = 5) +    # Add global anova p-value
  theme_classic(base_size = 20)
p_cv_l
ggsave(paste('Boxplot_log_CV_LOS_', abbr_names[data_i], '.png', sep = ''), p_cv_l)

                


# p_cv_l <-  ggboxplot(dat_key, x = "Treat", y = "cv_los", 
#                      color = "Treat", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
#                      order = c("A-Grad", "F-Grad", "Z-Baum"), add = "jitter",
#                      ylab = "CV of LOS", xlab = "Treatment", main = paste('Boxplot CV of LOS', abbr_names[data_i], sep = ' '))+ 
#   stat_compare_means(comparisons = my_comparisons, label = "p.signif", label.y = c(0.13, 0.13, 0.14), size = 5)+ # Add pairwise comparisons p-value
#   stat_compare_means(label.y = 0.16, size = 5) +    # Add global p-value
#   theme_classic(base_size = 20)
# p_cv_l
# ggsave(paste('Boxplot_CV_LOS_', abbr_names[data_i], '.png', sep = ''), p_cv_l)



p_sd_s
p_sd_e
p_sd_l


library(dplyr)
library(ggpubr)
library(ggplot2)

setwd('D:/GIS_Data/Vfl-oak/GEEMap/')
field_names <- c('Gei', 'Loh', 'Roh90', 'Roh620', 'Roh635')

# dat_all <- read.csv('Dat_int_pix.csv')
# head(dat_all)
# 
# dat_raw <- read.csv('Dat_all_pix_dc.csv')
# head(dat_raw)

dat_meta <- read.csv('meta_data.csv')
dat_meta

dat_pheno_SG <- read.csv('dat_pheno_pix_SG.csv')
dat_pheno_SG$key <- paste(dat_pheno_SG$plot, '_', dat_pheno_SG$parcel, sep = '')
dat_pheno_SG <- merge(dat_pheno_SG, dat_meta, by = 'key')
head(dat_pheno_SG)

dat_pheno_DL <- read.csv('dat_pheno_pix_DL.csv')
dat_pheno_DL$key <- paste(dat_pheno_DL$plot, '_', dat_pheno_DL$parcel, sep = '')
dat_pheno_DL <- merge(dat_pheno_DL, dat_meta, by = 'key')
head(dat_pheno_DL)
library(naniar)
dat_pheno_DL <- dat_pheno_DL %>% replace_with_na_at(.vars = 'eos', condition = ~.x < 210) # remove outliers in eos, DL

# dat_pheno_DL <- dat_pheno_DL[!duplicated(dat_pheno_DL), ]
# write.csv(dat_pheno_DL, 'dat_pheno_pix_DL.csv', row.names = FALSE)

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
dat_wek$los <- dat_wek$eos - dat_wek$sos

################################################################################

###################
### define data ###
###      SG     ###
###################

data_names <- c('dat_pheno_SG', 'dat_pheno_DL', 'dat_wek')
title_names <- c('Savitzy-Golay', 'DLogistic', 'WekEO-VPP')
abbr_names <- c('SG', 'DL', 'VPP')
data_i <- 1
eval(parse(text = paste('dat_merge <- ', data_names[data_i], sep = '')))
head(dat_merge)

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




my_comparisons <- list( c("A-Grad", "F-Grad"), c("F-Grad", "Z-Baum"), c("A-Grad", "Z-Baum") )
p_sos <- ggboxplot(dat_merge, x = "Treat", y = "sos", 
          color = "Treat", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("A-Grad", "F-Grad", "Z-Baum"), add = "jitter",
          ylab = "SOS", xlab = "Treatment", main = paste('Boxplot SOS', abbr_names[data_i], sep = ' '))+ 
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", label.y = c(160, 160, 165), size = 5)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 175, size = 5) +    # Add global p-value
  theme_classic(base_size = 20)
p_sos
ggsave(paste('Boxplot_SOS_', abbr_names[data_i], '.png', sep = ''), p_sos)

p_eos <- ggboxplot(dat_merge, x = "Treat", y = "eos", 
          color = "Treat", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("A-Grad", "F-Grad", "Z-Baum"), add = "jitter",
          ylab = "EOS", xlab = "Treatment", main = paste('Boxplot EOS', abbr_names[data_i], sep = ' '))+ 
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", label.y = c(320, 320, 325), size = 5)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 335, size = 5) +    # Add global p-value
  theme_classic(base_size = 20)
p_eos
ggsave(paste('Boxplot_EOS_', abbr_names[data_i], '.png', sep = ''), p_eos)

p_los <- ggboxplot(dat_merge, x = "Treat", y = "los", 
          color = "Treat", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("A-Grad", "F-Grad", "Z-Baum"), add = "jitter",
          ylab = "LOS", xlab = "Treatment", main = paste('Boxplot LOS', abbr_names[data_i], sep = ' '))+ 
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", label.y = c(200, 200, 205), size = 5)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 215, size = 5) +    # Add global p-value
  theme_classic(base_size = 20)
p_los
ggsave(paste('Boxplot_LOS_', abbr_names[data_i], '.png', sep = ''), p_los)


## As the p-value is less than the significance level 0.05, we can conclude that there are significant differences between the treatment groups.
kruskal.test(sos ~ Treat, data = dat_merge)

## multiple pairwise-comparison between groups
pairwise.wilcox.test(dat_merge$sos, dat_merge$Treat,
                     p.adjust.method = "BH")


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
    
    # calculate mean of each index
    m_sos <- mean(sub_yr$sos)
    m_eos <- mean(sub_yr$eos)
    m_los <- mean(sub_yr$los)
    
    # calculate CV (Coefficient of Variation)
    cv_sos <- sd_sos/m_sos
    cv_eos <- sd_eos/m_eos
    cv_los <- sd_los/m_los
    
    new <- data.frame(key = key_n, plot = plot, parcel = par, year = yr, Treat = treat, Age = age, 
                      pix_sos = pix_sos, pix_eos = pix_eos, pix_los = pix_los, 
                      m_sos = m_sos, m_eos = m_eos, m_los = m_los,
                      sd_sos = sd_sos, sd_eos = sd_eos, sd_los = sd_los, 
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

# # sos
# dat_key_sos <- dat_key[dat_key$pix_sos > 10, ]

### density plot
ggdensity(dat_key$sd_sos, 
          main = paste('Density of SD SOS, ', title_names[data_i], sep = ''),
          xlab = "DOY") +
  theme_classic(base_size = 20)

ggqqplot(dat_key$sd_sos, 
         main = paste('QQ Plot of SD SOS, ', title_names[data_i], sep = '')) +
  theme_classic(base_size = 20)

### Shapiro-Wilk test of normality for one variable
### p-value > 0.05 assumes normal distribution

sha.test <- shapiro.test(dat_merge$sos)
if (sha.test$p.value > 0.05) {norm <- "normal distribution"} else{norm <- "non-normal distribution"}
norm

# dat_key_sos <- dat_key_sos[dat_key_sos$year != 2017,]
# compare_means(sd_sos ~ Treat, data = dat_key_sos)

# p <- ggboxplot(dat_key_sos, x = "Treat", y = "sd_sos", 
#           color = "Treat", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
#           order = c("A-Grad", "F-Grad", "Z-Baum"), add = "jitter",
#           ylab = "SD of SOS", xlab = "Treatment", main = paste('Boxplot SD of SOS', 'SG', sep = ' '))
# p + stat_compare_means()

my_comparisons <- list( c("A-Grad", "F-Grad"), c("F-Grad", "Z-Baum"), c("A-Grad", "Z-Baum") )

p_sd_s <-  ggboxplot(dat_key, x = "Treat", y = "sd_sos", 
          color = "Treat", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("A-Grad", "F-Grad", "Z-Baum"), add = "jitter",
          ylab = "SD of SOS", xlab = "Treatment", main = paste('Boxplot SD of SOS', abbr_names[data_i], sep = ' '))+ 
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", label.y = c(8.5, 8.5, 9), size = 5)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 10, size = 5) +    # Add global p-value
  theme_classic(base_size = 20)
p_sd_s
ggsave(paste('Boxplot_SD_SOS_', abbr_names[data_i], '.png', sep = ''), p_sd_s)

p_cv_s <-  ggboxplot(dat_key, x = "Treat", y = "cv_sos", 
                     color = "Treat", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                     order = c("A-Grad", "F-Grad", "Z-Baum"), add = "jitter",
                     ylab = "CV of SOS", xlab = "Treatment", main = paste('Boxplot CV of SOS', abbr_names[data_i], sep = ' '))+ 
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", label.y = c(0.11, 0.11, 0.12), size = 5)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 0.15, size = 5) +    # Add global p-value
  theme_classic(base_size = 20)
p_cv_s
ggsave(paste('Boxplot_CV_SOS_', abbr_names[data_i], '.png', sep = ''), p_cv_s)

# eos
# dat_key_eos <- dat_key[dat_key$pix_eos > 10, ]
p_sd_e <-  ggboxplot(dat_key, x = "Treat", y = "sd_eos", 
                     color = "Treat", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                     order = c("A-Grad", "F-Grad", "Z-Baum"), add = "jitter",
                     ylab = "SD of EOS", xlab = "Treatment", main = paste('Boxplot SD of EOS', abbr_names[data_i], sep = ' '))+ 
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", label.y = c(17, 17, 18), size = 5)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 20, size = 5) +    # Add global p-value
  theme_classic(base_size = 20)
p_sd_e
ggsave(paste('Boxplot_SD_EOS_', abbr_names[data_i], '.png', sep = ''), p_sd_e)

p_cv_e <-  ggboxplot(dat_key, x = "Treat", y = "cv_eos", 
                     color = "Treat", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                     order = c("A-Grad", "F-Grad", "Z-Baum"), add = "jitter",
                     ylab = "CV of EOS", xlab = "Treatment", main = paste('Boxplot CV of EOS', abbr_names[data_i], sep = ' '))+ 
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", label.y = c(0.07, 0.07, 0.08), size = 5)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 0.1, size = 5) +    # Add global p-value
  theme_classic(base_size = 20)
p_cv_e
ggsave(paste('Boxplot_CV_EOS_', abbr_names[data_i], '.png', sep = ''), p_cv_e)

# los
# dat_key_los <- dat_key[dat_key$pix_los > 10, ]
p_sd_l <-  ggboxplot(dat_key, x = "Treat", y = "sd_los", 
                     color = "Treat", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                     order = c("A-Grad", "F-Grad", "Z-Baum"), add = "jitter",
                     ylab = "SD of LOS", xlab = "Treatment", main = paste('Boxplot SD of LOS', abbr_names[data_i], sep = ' '))+ 
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", label.y = c(17, 17, 18), size = 5)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 20, size = 5) +    # Add global p-value
  theme_classic(base_size = 20)
p_sd_l
ggsave(paste('Boxplot_SD_LOS_', abbr_names[data_i], '.png', sep = ''), p_sd_l)

p_cv_l <-  ggboxplot(dat_key, x = "Treat", y = "cv_los", 
                     color = "Treat", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                     order = c("A-Grad", "F-Grad", "Z-Baum"), add = "jitter",
                     ylab = "CV of LOS", xlab = "Treatment", main = paste('Boxplot CV of LOS', abbr_names[data_i], sep = ' '))+ 
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", label.y = c(0.13, 0.13, 0.14), size = 5)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 0.16, size = 5) +    # Add global p-value
  theme_classic(base_size = 20)
p_cv_l
ggsave(paste('Boxplot_CV_LOS_', abbr_names[data_i], '.png', sep = ''), p_cv_l)


p_cv_s
p_cv_e
p_cv_l

p_sd_s
p_sd_e
p_sd_l

###################
### define data ###
###      DL     ###
###################


data_names <- c('dat_pheno_SG', 'dat_pheno_DL', 'dat_wek')
title_names <- c('Savitzy-Golay', 'DLogistic', 'WekEO-VPP')
abbr_names <- c('SG', 'DL', 'VPP')
data_i <- 2
eval(parse(text = paste('dat_merge <- ', data_names[data_i], sep = '')))
head(dat_merge)


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

### Kruskal-Wallis Test

dat_merge$Treat <- as.factor(dat_merge$Treat)
levels(dat_merge$Treat)

# dat_merge$group <- ordered(dat_merge$Treat,
#                          levels = c("A-Grad", "F-Grad", "Z-Baum"))

group_by(dat_merge, Treat) %>%
  summarise(
    count = n(),
    mean = mean(sos, na.rm = TRUE),
    sd = sd(sos, na.rm = TRUE),
    median = median(sos, na.rm = TRUE),
    IQR = IQR(sos, na.rm = TRUE)
  )


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



my_comparisons <- list( c("A-Grad", "F-Grad"), c("F-Grad", "Z-Baum"), c("A-Grad", "Z-Baum") )
p_sos <- ggboxplot(dat_merge, x = "Treat", y = "sos", 
                   color = "Treat", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                   order = c("A-Grad", "F-Grad", "Z-Baum"), add = "jitter",
                   ylab = "SOS", xlab = "Treatment", main = paste('Boxplot SOS', abbr_names[data_i], sep = ' '))+ 
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", label.y = c(150, 150, 155), size = 5)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 165, size = 5) +    # Add global p-value
  theme_classic(base_size = 20)
p_sos
ggsave(paste('Boxplot_SOS_', abbr_names[data_i], '.png', sep = ''), p_sos)

p_eos <- ggboxplot(dat_merge, x = "Treat", y = "eos", 
                   color = "Treat", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                   order = c("A-Grad", "F-Grad", "Z-Baum"), add = "jitter",
                   ylab = "EOS", xlab = "Treatment", main = paste('Boxplot EOS', abbr_names[data_i], sep = ' '))+ 
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", label.y = c(355, 355, 365), size = 5)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 380, size = 5) +    # Add global p-value
  theme_classic(base_size = 20)
p_eos
ggsave(paste('Boxplot_EOS_', abbr_names[data_i], '.png', sep = ''), p_eos)

p_los <- ggboxplot(dat_merge, x = "Treat", y = "los", 
                   color = "Treat", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                   order = c("A-Grad", "F-Grad", "Z-Baum"), add = "jitter",
                   ylab = "LOS", xlab = "Treatment", main = paste('Boxplot LOS', abbr_names[data_i], sep = ' '))+ 
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", label.y = c(245, 245, 260), size = 5)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 285, size = 5) +    # Add global p-value
  theme_classic(base_size = 20)
p_los
ggsave(paste('Boxplot_LOS_', abbr_names[data_i], '.png', sep = ''), p_los)


## As the p-value is less than the significance level 0.05, we can conclude that there are significant differences between the treatment groups.
kruskal.test(sos ~ Treat, data = dat_merge)

## multiple pairwise-comparison between groups
pairwise.wilcox.test(dat_merge$sos, dat_merge$Treat,
                     p.adjust.method = "BH")


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
    
    # calculate mean of each index
    m_sos <- mean(sub_yr$sos)
    m_eos <- mean(sub_yr$eos)
    m_los <- mean(sub_yr$los)
    
    # calculate CV (Coefficient of Variation)
    cv_sos <- sd_sos/m_sos
    cv_eos <- sd_eos/m_eos
    cv_los <- sd_los/m_los
    
    new <- data.frame(key = key_n, plot = plot, parcel = par, year = yr, Treat = treat, Age = age, 
                      pix_sos = pix_sos, pix_eos = pix_eos, pix_los = pix_los, 
                      m_sos = m_sos, m_eos = m_eos, m_los = m_los,
                      sd_sos = sd_sos, sd_eos = sd_eos, sd_los = sd_los, 
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

# # sos
# dat_key_sos <- dat_key[dat_key$pix_sos > 10, ]

### density plot
ggdensity(dat_key$sd_sos, 
          main = paste('Density of SD SOS, ', title_names[data_i], sep = ''),
          xlab = "DOY") +
  theme_classic(base_size = 20)

ggqqplot(dat_key$sd_sos, 
         main = paste('QQ Plot of SD SOS, ', title_names[data_i], sep = '')) +
  theme_classic(base_size = 20)

### Shapiro-Wilk test of normality for one variable
### p-value > 0.05 assumes normal distribution

sha.test <- shapiro.test(dat_merge$sos)
if (sha.test$p.value > 0.05) {norm <- "normal distribution"} else{norm <- "non-normal distribution"}
norm

# dat_key_sos <- dat_key_sos[dat_key_sos$year != 2017,]
compare_means(sd_sos ~ Treat, data = dat_key_sos)

# p <- ggboxplot(dat_key_sos, x = "Treat", y = "sd_sos", 
#           color = "Treat", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
#           order = c("A-Grad", "F-Grad", "Z-Baum"), add = "jitter",
#           ylab = "SD of SOS", xlab = "Treatment", main = paste('Boxplot SD of SOS', 'SG', sep = ' '))
# p + stat_compare_means()

my_comparisons <- list( c("A-Grad", "F-Grad"), c("F-Grad", "Z-Baum"), c("A-Grad", "Z-Baum") )

p_sd_s <-  ggboxplot(dat_key, x = "Treat", y = "sd_sos", 
                     color = "Treat", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                     order = c("A-Grad", "F-Grad", "Z-Baum"), add = "jitter",
                     ylab = "SD of SOS", xlab = "Treatment", main = paste('Boxplot SD of SOS', abbr_names[data_i], sep = ' '))+ 
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", label.y = c(12.5, 12.5, 13.5), size = 5)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 15, size = 5) +    # Add global p-value
  theme_classic(base_size = 20)
p_sd_s
ggsave(paste('Boxplot_SD_SOS_', abbr_names[data_i], '.png', sep = ''), p_sd_s)

p_cv_s <-  ggboxplot(dat_key, x = "Treat", y = "cv_sos", 
                     color = "Treat", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                     order = c("A-Grad", "F-Grad", "Z-Baum"), add = "jitter",
                     ylab = "CV of SOS", xlab = "Treatment", main = paste('Boxplot CV of SOS', abbr_names[data_i], sep = ' '))+ 
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", label.y = c(0.1, 0.1, 0.11), size = 5)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 0.125, size = 5) +    # Add global p-value
  theme_classic(base_size = 20)
p_cv_s
ggsave(paste('Boxplot_CV_SOS_', abbr_names[data_i], '.png', sep = ''), p_cv_s)

# eos
# dat_key_eos <- dat_key[dat_key$pix_eos > 10, ]
p_sd_e <-  ggboxplot(dat_key, x = "Treat", y = "sd_eos", 
                     color = "Treat", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                     order = c("A-Grad", "F-Grad", "Z-Baum"), add = "jitter",
                     ylab = "SD of EOS", xlab = "Treatment", main = paste('Boxplot SD of EOS', abbr_names[data_i], sep = ' '))+ 
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", label.y = c(24.5, 24.5, 26), size = 5)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 29, size = 5) +    # Add global p-value
  theme_classic(base_size = 20)
p_sd_e
ggsave(paste('Boxplot_SD_EOS_', abbr_names[data_i], '.png', sep = ''), p_sd_e)

p_cv_e <-  ggboxplot(dat_key, x = "Treat", y = "cv_eos", 
                     color = "Treat", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                     order = c("A-Grad", "F-Grad", "Z-Baum"), add = "jitter",
                     ylab = "CV of EOS", xlab = "Treatment", main = paste('Boxplot CV of EOS', abbr_names[data_i], sep = ' '))+ 
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", label.y = c(0.085, 0.085, 0.095), size = 5)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 0.11, size = 5) +    # Add global p-value
  theme_classic(base_size = 20)
p_cv_e
ggsave(paste('Boxplot_CV_EOS_', abbr_names[data_i], '.png', sep = ''), p_cv_e)

# los
# dat_key_los <- dat_key[dat_key$pix_los > 10, ]
p_sd_l <-  ggboxplot(dat_key, x = "Treat", y = "sd_los", 
                     color = "Treat", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                     order = c("A-Grad", "F-Grad", "Z-Baum"), add = "jitter",
                     ylab = "SD of LOS", xlab = "Treatment", main = paste('Boxplot SD of LOS', abbr_names[data_i], sep = ' '))+ 
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", label.y = c(24.5, 24.5, 26), size = 5)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 28.5, size = 5) +    # Add global p-value
  theme_classic(base_size = 20)
p_sd_l
ggsave(paste('Boxplot_SD_LOS_', abbr_names[data_i], '.png', sep = ''), p_sd_l)

p_cv_l <-  ggboxplot(dat_key, x = "Treat", y = "cv_los", 
                     color = "Treat", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                     order = c("A-Grad", "F-Grad", "Z-Baum"), add = "jitter",
                     ylab = "CV of LOS", xlab = "Treatment", main = paste('Boxplot CV of LOS', abbr_names[data_i], sep = ' '))+ 
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", label.y = c(0.165, 0.165, 0.175), size = 5)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 0.19, size = 5) +    # Add global p-value
  theme_classic(base_size = 20)
p_cv_l
ggsave(paste('Boxplot_CV_LOS_', abbr_names[data_i], '.png', sep = ''), p_cv_l)


p_cv_s
p_cv_e
p_cv_l

p_sd_s
p_sd_e
p_sd_l


###################
### define data ###
###     VPP     ###
###################


data_names <- c('dat_pheno_SG', 'dat_pheno_DL', 'dat_wek')
title_names <- c('Savitzy-Golay', 'DLogistic', 'WekEO-VPP')
abbr_names <- c('SG', 'DL', 'VPP')
data_i <- 3
eval(parse(text = paste('dat_merge <- ', data_names[data_i], sep = '')))
head(dat_merge)


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

### Kruskal-Wallis Test

dat_merge$Treat <- as.factor(dat_merge$Treat)
levels(dat_merge$Treat)

# dat_merge$group <- ordered(dat_merge$Treat,
#                          levels = c("A-Grad", "F-Grad", "Z-Baum"))

group_by(dat_merge, Treat) %>%
  summarise(
    count = n(),
    mean = mean(sos, na.rm = TRUE),
    sd = sd(sos, na.rm = TRUE),
    median = median(sos, na.rm = TRUE),
    IQR = IQR(sos, na.rm = TRUE)
  )


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



my_comparisons <- list( c("A-Grad", "F-Grad"), c("F-Grad", "Z-Baum"), c("A-Grad", "Z-Baum") )
p_sos <- ggboxplot(dat_merge, x = "Treat", y = "sos", 
                   color = "Treat", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                   order = c("A-Grad", "F-Grad", "Z-Baum"), add = "jitter",
                   ylab = "SOS", xlab = "Treatment", main = paste('Boxplot SOS', abbr_names[data_i], sep = ' '))+ 
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", label.y = c(145, 145, 150), size = 5)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 160, size = 5) +    # Add global p-value
  theme_classic(base_size = 20)
p_sos
ggsave(paste('Boxplot_SOS_', abbr_names[data_i], '.png', sep = ''), p_sos)

p_eos <- ggboxplot(dat_merge, x = "Treat", y = "eos", 
                   color = "Treat", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                   order = c("A-Grad", "F-Grad", "Z-Baum"), add = "jitter",
                   ylab = "EOS", xlab = "Treatment", main = paste('Boxplot EOS', abbr_names[data_i], sep = ' '))+ 
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", label.y = c(340, 340, 350), size = 5)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 375, size = 5) +    # Add global p-value
  theme_classic(base_size = 20)
p_eos
ggsave(paste('Boxplot_EOS_', abbr_names[data_i], '.png', sep = ''), p_eos)

p_los <- ggboxplot(dat_merge, x = "Treat", y = "los", 
                   color = "Treat", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                   order = c("A-Grad", "F-Grad", "Z-Baum"), add = "jitter",
                   ylab = "LOS", xlab = "Treatment", main = paste('Boxplot LOS', abbr_names[data_i], sep = ' '))+ 
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", label.y = c(245, 245, 260), size = 5)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 285, size = 5) +    # Add global p-value
  theme_classic(base_size = 20)
p_los
ggsave(paste('Boxplot_LOS_', abbr_names[data_i], '.png', sep = ''), p_los)


## As the p-value is less than the significance level 0.05, we can conclude that there are significant differences between the treatment groups.
kruskal.test(sos ~ Treat, data = dat_merge)

## multiple pairwise-comparison between groups
pairwise.wilcox.test(dat_merge$sos, dat_merge$Treat,
                     p.adjust.method = "BH")


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
    
    # calculate mean of each index
    m_sos <- mean(sub_yr$sos)
    m_eos <- mean(sub_yr$eos)
    m_los <- mean(sub_yr$los)
    
    # calculate CV (Coefficient of Variation)
    cv_sos <- sd_sos/m_sos
    cv_eos <- sd_eos/m_eos
    cv_los <- sd_los/m_los
    
    new <- data.frame(key = key_n, plot = plot, parcel = par, year = yr, Treat = treat, Age = age, 
                      pix_sos = pix_sos, pix_eos = pix_eos, pix_los = pix_los, 
                      m_sos = m_sos, m_eos = m_eos, m_los = m_los,
                      sd_sos = sd_sos, sd_eos = sd_eos, sd_los = sd_los, 
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

# # sos
# dat_key_sos <- dat_key[dat_key$pix_sos > 10, ]

### density plot
ggdensity(dat_key$sd_sos, 
          main = paste('Density of SD SOS, ', title_names[data_i], sep = ''),
          xlab = "DOY") +
  theme_classic(base_size = 20)

ggqqplot(dat_key$sd_sos, 
         main = paste('QQ Plot of SD SOS, ', title_names[data_i], sep = '')) +
  theme_classic(base_size = 20)

### Shapiro-Wilk test of normality for one variable
### p-value > 0.05 assumes normal distribution

sha.test <- shapiro.test(dat_merge$sos)
if (sha.test$p.value > 0.05) {norm <- "normal distribution"} else{norm <- "non-normal distribution"}
norm

# dat_key_sos <- dat_key_sos[dat_key_sos$year != 2017,]
compare_means(sd_sos ~ Treat, data = dat_key_sos)

# p <- ggboxplot(dat_key_sos, x = "Treat", y = "sd_sos", 
#           color = "Treat", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
#           order = c("A-Grad", "F-Grad", "Z-Baum"), add = "jitter",
#           ylab = "SD of SOS", xlab = "Treatment", main = paste('Boxplot SD of SOS', 'SG', sep = ' '))
# p + stat_compare_means()

my_comparisons <- list( c("A-Grad", "F-Grad"), c("F-Grad", "Z-Baum"), c("A-Grad", "Z-Baum") )

p_sd_s <-  ggboxplot(dat_key, x = "Treat", y = "sd_sos", 
                     color = "Treat", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                     order = c("A-Grad", "F-Grad", "Z-Baum"), add = "jitter",
                     ylab = "SD of SOS", xlab = "Treatment", main = paste('Boxplot SD of SOS', abbr_names[data_i], sep = ' '))+ 
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", label.y = c(7, 7, 7.5), size = 5)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 8.5, size = 5) +    # Add global p-value
  theme_classic(base_size = 20)
p_sd_s
ggsave(paste('Boxplot_SD_SOS_', abbr_names[data_i], '.png', sep = ''), p_sd_s)

p_cv_s <-  ggboxplot(dat_key, x = "Treat", y = "cv_sos", 
                     color = "Treat", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                     order = c("A-Grad", "F-Grad", "Z-Baum"), add = "jitter",
                     ylab = "CV of SOS", xlab = "Treatment", main = paste('Boxplot CV of SOS', abbr_names[data_i], sep = ' '))+ 
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", label.y = c(0.065, 0.065, 0.07), size = 5)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 0.08, size = 5) +    # Add global p-value
  theme_classic(base_size = 20)
p_cv_s
ggsave(paste('Boxplot_CV_SOS_', abbr_names[data_i], '.png', sep = ''), p_cv_s)

# eos
# dat_key_eos <- dat_key[dat_key$pix_eos > 10, ]
p_sd_e <-  ggboxplot(dat_key, x = "Treat", y = "sd_eos", 
                     color = "Treat", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                     order = c("A-Grad", "F-Grad", "Z-Baum"), add = "jitter",
                     ylab = "SD of EOS", xlab = "Treatment", main = paste('Boxplot SD of EOS', abbr_names[data_i], sep = ' '))+ 
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", label.y = c(21.5, 21.5, 23), size = 5)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 26, size = 5) +    # Add global p-value
  theme_classic(base_size = 20)
p_sd_e
ggsave(paste('Boxplot_SD_EOS_', abbr_names[data_i], '.png', sep = ''), p_sd_e)

p_cv_e <-  ggboxplot(dat_key, x = "Treat", y = "cv_eos", 
                     color = "Treat", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                     order = c("A-Grad", "F-Grad", "Z-Baum"), add = "jitter",
                     ylab = "CV of EOS", xlab = "Treatment", main = paste('Boxplot CV of EOS', abbr_names[data_i], sep = ' '))+ 
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", label.y = c(0.08, 0.08, 0.085), size = 5)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 0.1, size = 5) +    # Add global p-value
  theme_classic(base_size = 20)
p_cv_e
ggsave(paste('Boxplot_CV_EOS_', abbr_names[data_i], '.png', sep = ''), p_cv_e)

# los
# dat_key_los <- dat_key[dat_key$pix_los > 10, ]
p_sd_l <-  ggboxplot(dat_key, x = "Treat", y = "sd_los", 
                     color = "Treat", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                     order = c("A-Grad", "F-Grad", "Z-Baum"), add = "jitter",
                     ylab = "SD of LOS", xlab = "Treatment", main = paste('Boxplot SD of LOS', abbr_names[data_i], sep = ' '))+ 
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", label.y = c(23.5, 23.5, 25), size = 5)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 28, size = 5) +    # Add global p-value
  theme_classic(base_size = 20)
p_sd_l
ggsave(paste('Boxplot_SD_LOS_', abbr_names[data_i], '.png', sep = ''), p_sd_l)

p_cv_l <-  ggboxplot(dat_key, x = "Treat", y = "cv_los", 
                     color = "Treat", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                     order = c("A-Grad", "F-Grad", "Z-Baum"), add = "jitter",
                     ylab = "CV of LOS", xlab = "Treatment", main = paste('Boxplot CV of LOS', abbr_names[data_i], sep = ' '))+ 
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", label.y = c(0.14, 0.14, 0.15), size = 5)+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 0.17, size = 5) +    # Add global p-value
  theme_classic(base_size = 20)
p_cv_l
ggsave(paste('Boxplot_CV_LOS_', abbr_names[data_i], '.png', sep = ''), p_cv_l)


p_cv_s
p_cv_e
p_cv_l

p_sd_s
p_sd_e
p_sd_l
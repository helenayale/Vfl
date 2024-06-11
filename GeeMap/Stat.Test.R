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


### define data
dat_merge <- dat_pheno_SG
head(dat_merge)

### check data
set.seed(1234)
dplyr::sample_n(dat_merge, 10)

### density plot
ggdensity(dat_merge$sos, 
          main = "Density of SOS",
          xlab = "DOY")

### QQ plot
ggqqplot(dat_merge$sos)


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

ggboxplot(dat_merge, x = "Treat", y = "sos", 
          color = "Treat", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("A-Grad", "F-Grad", "Z-Baum"),
          ylab = "SOS", xlab = "Treatment")


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
    
    new <- data.frame(key = key_n, plot = plot, parcel = par, year = yr, Treat = treat, pix_sos = pix_sos, pix_eos = pix_eos, pix_los = pix_los, sd_sos = sd_sos, sd_eos = sd_eos, sd_los = sd_los, Age = age)
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

# sos
dat_key_sos <- dat_key[dat_key$pix_sos > 10, ]

### density plot
ggdensity(dat_key_sos$sd_sos, 
          main = "Density of SOS SD",
          xlab = "DOY")

ggqqplot(dat_key_sos$sd_sos)

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
ggboxplot(dat_key_sos, x = "Treat", y = "sd_sos", 
          color = "Treat", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("A-Grad", "F-Grad", "Z-Baum"), add = "jitter",
          ylab = "SD of SOS", xlab = "Treatment", main = paste('Boxplot SD of SOS', 'SG', sep = ' '))+ 
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", label.y = c(8, 8, 9))+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 10)     # Add global p-value

### As the p-value is less than the significance level 0.05, we can conclude that there are significant differences between the treatment groups.




# kru.test <- kruskal.test(sd_sos ~ Treat, data = dat_key_sos)
# if (kru.test$p.value < 0.001) {
#   pv <- "***"
# }else if (kru.test$p.value < 0.05){
#   pv <- "**"
# }else if (kru.test$p.value < 0.01){
#   pv <- "*"
# } else{
#   pv <- " "
#     }
# pv
# 
# ## multiple pairwise-comparison between groups
# pai.test <- pairwise.wilcox.test(dat_key_sos$sd_sos, dat_key_sos$Treat,
#                      p.adjust.method = "BH")
# pai.test$p.value
# pv_group <- pai.test$p.value
# for (i in 1:ncol(pv_group)){
#   for (j in 1:nrow(pv_group)){
#     pv.v <- pv_group[j,i]
#     if (is.na(pv.v) == TRUE){
#       pvstar <- "-"
#     }else if(pv.v < 0.001){
#       pvstart <- "***"
#     }else if (pv.v < 0.05){
#       pvstar <- "**"
#     }else if (pv.v < 0.01){
#       pvstar <- "*"
#     }else{
#       pvstar <- " "
#     }
#     pv_group[j,i] <- pvstar
#   }
# }
# pv_group


# eos
dat_key_eos <- dat_key[dat_key$pix_eos > 10, ]

# ggboxplot(dat_key_eos, x = "Treat", y = "sd_eos", 
#           color = "Treat", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
#           order = c("A-Grad", "F-Grad", "Z-Baum"),
#           ylab = "SD of EOS", xlab = "Treatment")
# 
# kruskal.test(sd_eos ~ Treat, data = dat_key_eos)
# 
# pairwise.wilcox.test(dat_key_sos$sd_eos, dat_key_eos$Treat,
#                      p.adjust.method = "BH")

my_comparisons <- list( c("A-Grad", "F-Grad"), c("F-Grad", "Z-Baum"), c("A-Grad", "Z-Baum") )
ggboxplot(dat_key_eos, x = "Treat", y = "sd_eos", 
          color = "Treat", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("A-Grad", "F-Grad", "Z-Baum"), add = "jitter",
          ylab = "SD of EOS", xlab = "Treatment", main = paste('Boxplot SD of EOS', 'SG', sep = ' '))+ 
  stat_compare_means(comparisons = my_comparisons,  label = "p.signif", label.y = c(15, 15, 16))+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 18)     # Add global p-value

# los
dat_key_los <- dat_key[dat_key$pix_los > 10, ]
# 
# ggboxplot(dat_key_los, x = "Treat", y = "sd_los", 
#           color = "Treat", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
#           order = c("A-Grad", "F-Grad", "Z-Baum"),
#           ylab = "SD of LOS", xlab = "Treatment")
# 
# kruskal.test(sd_los ~ Treat, data = dat_key_los)
# 
# pairwise.wilcox.test(dat_key_los$sd_los, dat_key_los$Treat,
#                      p.adjust.method = "BH")

my_comparisons <- list( c("A-Grad", "F-Grad"), c("F-Grad", "Z-Baum"), c("A-Grad", "Z-Baum") )
ggboxplot(dat_key_los, x = "Treat", y = "sd_los", 
          color = "Treat", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("A-Grad", "F-Grad", "Z-Baum"), add = "jitter",
          ylab = "SD of LOS", xlab = "Treatment", main = paste('Boxplot SD of LOS', 'SG', sep = ' '))+ 
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", label.y = c(15, 15, 16))+ # Add pairwise comparisons p-value
  stat_compare_means(label.y = 18)     # Add global p-value

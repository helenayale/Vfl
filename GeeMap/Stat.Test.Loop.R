library(dplyr)
library(ggpubr)
library(ggplot2)

setwd('D:/GIS_Data/Vfl-oak/GEEMap/')
field_names <- c('Gei', 'Loh', 'Roh90', 'Roh620', 'Roh635')

### load my data
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

### load dwd & wekeo data
dat_dwd <- read.csv('DWD_Pheno.csv')
dat_dwd$key <- paste(dat_dwd$ID, '_', dat_dwd$Parcel, sep = '')
dat_dwd <- merge(dat_dwd, dat_meta, by = 'key')
head(dat_dwd)
colnames(dat_dwd) <- c('key', 'plot', 'field', 'parcel', 'interne', 'year', 'pix', 'sos', 'eos','fruit', 'Treat')
dat_dwd$los <- dat_dwd$eos - dat_dwd$sos

dat_wek <- read.csv('WekEO.csv')
dat_wek$key <- paste(dat_wek$ID, '_', dat_wek$Parcel, sep = '')
dat_wek <- merge(dat_wek, dat_meta, by = 'key')
head(dat_wek)
colnames(dat_wek) <- c('key', 'plot', 'field', 'parcel', 'interne', 'year', 'pix', 'eos', 'eosv', 'sos', 'sosv', 'max', 'maxv', 'Treat')
dat_wek$los <- dat_wek$eos - dat_wek$sos

data_names <- c('dat_pheno_SG', 'dat_pheno_DL', 'dat_wek', 'dat_dwd')
data_titles <- c('SG', 'DL', 'WekEO', 'DWD')
index_names <- c('sos', 'eos', 'los')
index_titles <- c('SOS', 'EOS', 'LOS')

dat_i <- 1
index_i  <- 1

for (dat_i in 1:length(data_names)){
  ### define data
  eval(parse(text = paste('dat_merge <- ', data_names[dat_i], sep = '')))
  d_ttl <- data_titles[dat_i]
  
  for (index_i in 1:length(index_names)){
    ind <- index_names[index_i]
    i_ttl <- index_titles[index_i]
    ### density plot
    p_den <- ggdensity(dat_merge$sos, 
                       main = paste('Density of', i_ttl, d_ttl, sep = ' '),
                       xlab = "DOY")
    ggsave(paste('Dens.',i_ttl, '.', d_ttl, sep = ''), p_den)
    
    ### QQ plot
    p_qq <- ggqqplot(dat_merge$sos,
                     main = paste('QQ Plot of', i_ttl, d_ttl, sep = ' '))
    ggsave(paste('QQ.', i_ttl, '.', d_ttl, sep = ''), p_qq)
    
    ### Shapiro-Wilk test of normality for one variable
    ### p-value > 0.05 assumes normal distribution
    
    sha.test <- shapiro.test(dat_merge$sos)
    if (sha.test$p.value > 0.05) {norm <- "normal distribution"} else{norm <- "non-normal distribution"}
    sha.test$p.value
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
  }
  
}
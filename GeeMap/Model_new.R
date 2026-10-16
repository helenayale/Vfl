library(dplyr)
library(ggpubr)
library(ggplot2)
library(lme4)
library(lmerTest) # package to show p-value
library(naniar)
library(MuMIn)

setwd('D:/GIS_Data/Vfl-oak/GEEMap/')
field_names <- c('Gei', 'Loh', 'Roh90', 'Roh620', 'Roh635')


#### Load data WekEO VPP
load('dat_wek.rda')
head(dat_wek)
colnames(dat_wek) <- c('key', 'plot', 'field', 'parcel', 'interne', 'year', 'pix', 'sos', 'eos', 'Treat', 'Age')
# library(naniar)
dat_wek <- dat_wek %>% replace_with_na_at(.vars = 'eos', condition = ~.x < 210) # remove outliers in eos, DL
head(dat_wek)
unique(dat_wek$plot)

dat_wek$los <- dat_wek$eos - dat_wek$sos
head(dat_wek)

#plot_filter <- c('Gei', 'Roh620', 'Roh90')
#sub_wek <- dat_wek[dat_wek$plot %in% plot_filter,]
#sub_wek <- subset(dat_wek, plot %in% plot_filter)

sub_wek <- dat_wek
unique(sub_wek$plot)

# merge plots into two: North & South
sub_wek$north <- sub_wek$plot
sub_wek$north <- replace(sub_wek$north, sub_wek$north == 'Gei', 'South')
sub_wek$north <- replace(sub_wek$north, sub_wek$north == 'Roh620', 'North')
sub_wek$north <- replace(sub_wek$north, sub_wek$north == 'Roh90', 'North')
sub_wek$north <- replace(sub_wek$north, sub_wek$north == 'Roh635', 'North')
sub_wek$north <- replace(sub_wek$north, sub_wek$north == 'Loh', 'North')

head(sub_wek)


#### load DWD data
# Spring Temperature
load('DWD_STMean.rda')
dat_sea <- subset(dat_sea, season == 'spr')
head(dat_sea)

range(dat_sea$STMean)
dat_sea$STMean <- dat_sea$STMean / 10

dat_sea$key <- paste(dat_sea$ID, '_', dat_sea$Parcel, sep = '')
dat_dwd <- dat_sea[, c('key', 'ID', 'Parcel', 'year', 'STMean')]

dat_dwd$key_yr <- paste(dat_dwd$key, '_', dat_dwd$year, sep = '')
head(dat_dwd)

# Spring Precipitation
load('DWD_SPrec.rda')
dat_sea <- subset(dat_sea, season == 'spr')
head(dat_sea)


dat_sea$key <- paste(dat_sea$ID, '_', dat_sea$Parcel, sep = '')
dat_sea$key_yr <- paste(dat_sea$key, '_', dat_sea$year, sep = '')
dat_sprec <- dat_sea[, c('key_yr', 'SPrec')]
head(dat_sprec)

dat_dwd <- merge(dat_dwd, dat_sprec, by = 'key_yr')
head(dat_dwd)

load('DWD_Pheno.rda')
dat_phe$key <- paste(dat_phe$ID, '_', dat_phe$Parcel, sep = '')
dat_phe$key_yr <- paste(dat_phe$key, '_', dat_phe$year, sep = '')
dat_p <- dat_phe[, c('key_yr', 'STEBO', 'STEBV')]
dat_dwd <- merge(dat_dwd, dat_p, by = 'key_yr')
head(dat_dwd)

#### add DWD data to dataset
sub_wek$key_yr <- paste(sub_wek$key, '_', sub_wek$year, sep = '')
head(sub_wek)

add_dwd <- dat_dwd[, c('key_yr', 'STMean', 'SPrec', 'STEBO', 'STEBV')]
sub_wek <- merge(sub_wek, add_dwd, by = 'key_yr')
head(sub_wek)

# normalize Temperature & Precipitation

sub_wek$spr_temp <- scale(sub_wek$STMean, center = TRUE, scale = TRUE)
sub_wek$spr_prec <- scale(sub_wek$SPrec, center = TRUE, scale = TRUE)
sub_wek$stebo_sc <- scale(sub_wek$STEBO, center = TRUE, scale = TRUE)
sub_wek$stebv_sc <- scale(sub_wek$STEBV, center = TRUE, scale = TRUE)

# Treat to level, L M H

tr <- sub_wek$Treat
tr[tr == 'A-Grad'] <- 'Low'
tr[tr == 'F-Grad'] <- 'Medium'
tr[tr == 'Z-Baum'] <- 'High'

unique(tr)

sub_wek$Treat_lvl <- factor(tr, levels = c('Low', 'Medium', 'High'))

head(sub_wek)

# add weight column

# count <- sub_wek  %>%
#   count(plot, Treat_lvl, name = 'count')
# count

count <- sub_wek  %>%
  count(Treat_lvl, name = 'count')
count

n_plot <- unique(count$plot)

sum <- sum(count$count)

W_A <- sum/count$count[1]
W_F <- sum/count$count[2]
W_Z <- sum/count$count[3]

count$weight[count$Treat_lvl == 'Low'] <- W_A
count$weight[count$Treat_lvl == 'Medium'] <- W_F
count$weight[count$Treat_lvl == 'High'] <- W_Z

sub_wek <- sub_wek %>%
  mutate(weight = case_when(
    Treat_lvl == 'Low' ~ W_A,
    Treat_lvl == 'Medium'~ W_F,
    Treat_lvl == 'High' ~ W_Z
  ))
head(sub_wek)

unique(sub_wek$plot)

# 
# for(i in 1:length(n_plot)){
#   plot_name <- n_plot[i]
#   sub_dat <- subset(count, plot == plot_name)
#   sum <- sum(sub_dat$count)
#   
#   W_A <- sum/sub_dat$count[1]
#   W_F <- sum/sub_dat$count[2]
#   W_Z <- sum/sub_dat$count[3]
#   
#   count$weight[count$Treat == 'Low' & count$plot ==  plot_name] <- W_A
#   count$weight[count$Treat == 'Medium' & count$plot ==  plot_name] <- W_F
#   count$weight[count$Treat == 'High' & count$plot ==  plot_name] <- W_Z
# 
# }
# 
# count
# 
# count$key_tr <- paste(count$plot, count$Treat_lvl, sep = '')
# sub_wek$key_tr <- paste(sub_wek$plot, sub_wek$Treat_lvl, sep = '')
# 
# dat_weight <- count[, c('key_tr', 'weight')]
# sub_wek <- merge(sub_wek, dat_weight, by = 'key_tr')
# head(sub_wek)

################################################################################

####################
### Linear Model ###
####################

x <- sub_wek$STMean
y <- sub_wek$STEBO
cor_result <- cor.test(x, y, method = "pearson")

print(cor_result)


mixed.lmer <- lmer(eos ~ Treat_lvl + north + spr_temp + spr_prec + (1|year), data = sub_wek)  # intercept
summary(mixed.lmer)
mixed.lmer
summary(mixed.lmer)$coefficients

#mixed.lmer <- lmer(sos ~ Treat_lvl + north + spr_temp + spr_prec + (1|year), weight = weight, data = sub_wek)  # intercept
#summary(mixed.lmer)

library(MuMIn)
r_sq_val <- r.squaredGLMM(mixed.lmer)
print(r_sq_val)

r.squaredGLMM(mixed.lmer)


########################################
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

temp <- summary(mixed.lmer)$coefficients[5,1]
p.value <- summary(mixed.lmer)$coefficients[5,5]
if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "")
temp_p <- pvalue

prec <- summary(mixed.lmer)$coefficients[6,1]
p.value <- summary(mixed.lmer)$coefficients[6,5]
if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "")
prec_p <- pvalue


new <- data.frame(Y = 'sos', data =  abbr_names[data_i], intercept = inter, int_p = int_p,
                          Medium = F_Grad, M_p = F_p, High = Z_Baum, H_p = Z_p,
                          north = north, north_p = north_p, temp = temp, temp_p = temp_p,
                          prec = prec, prec_p = prec_p)


#### calculate SD of each index

key_list <- unique(sub_wek$key_yr)
key_list


for (key_i in 1:length(key_list)){
  key_n <- key_list[key_i]
  sub_key <- subset(sub_wek, key_yr == key_n)
  plot <- unique(sub_key$plot)
  par <- unique(sub_key$parcel)
  treat <- unique(sub_key$Treat_lvl)
  age <- unique(sub_key$Age)
  yr <- unique(sub_key$year)
  north <- unique(sub_key$north)
  spr_temp <- mean(sub_key$spr_temp)
  spr_prec <- mean(sub_key$spr_prec)
  STMean <- mean(sub_key$STMean)
  SPrec <- mean(sub_key$SPrec)
  weight <- unique(sub_key$weight)
  
  # count number of non-NA pixels
  pix_sos <- sum(!is.na(sub_key$sos))
  pix_eos <- sum(!is.na(sub_key$eos))
  pix_los <- sum(!is.na(sub_key$los))
    
   # calculate sd of each index
  sd_sos <- sd(sub_key$sos)
  sd_eos <- sd(sub_key$eos)
  sd_los <- sd(sub_key$los)
    
   # calculate mean of each index
  m_sos <- mean(sub_key$sos)
  m_eos <- mean(sub_key$eos)
  m_los <- mean(sub_key$los)

    
  # calculate CV (Coefficient of Variation)
  cv_sos <- sd_sos/m_sos
  cv_eos <- sd_eos/m_eos
  cv_los <- sd_los/m_los
    
  new <- data.frame(key_yr = key_n, plot = plot, parcel = par, year = yr, 
                    Treat_lvl = treat, Age = age, weight = weight,
                    north = north, spr_temp = spr_temp, spr_prec = spr_prec, 
                    STMean = STMean, SPrec = SPrec,
                    pix_sos = pix_sos, pix_eos = pix_eos, pix_los = pix_los,
                    m_sos = m_sos, m_eos = m_eos, m_los = m_los,
                    sd_sos = sd_sos, sd_eos = sd_eos, sd_los = sd_los, 
                    cv_sos = cv_sos, cv_eos = cv_eos, cv_los = cv_los)

  if (key_i == 1){
    dat_key <- new
  }else{
    dat_key <- rbind(dat_key, new)
  }
  
}

head(dat_key)
summary(dat_key)

dat_key$log_sds <- log(dat_key$sd_sos)
dat_key$log_sde <- log(dat_key$sd_eos)
dat_key$log_sdl <- log(dat_key$sd_los)

dat_key$log_cvs <- log(dat_key$cv_sos)
dat_key$log_cve <- log(dat_key$cv_eos)
dat_key$log_cvl <- log(dat_key$cv_los)


count_key <- dat_key  %>%
  count(Treat_lvl, name = 'count')
count_key

sum <- sum(count_key$count)

W_A <- sum/count_key$count[1]
W_F <- sum/count_key$count[2]
W_Z <- sum/count_key$count[3]

count_key$weight[count_key$Treat_lvl == 'Low'] <- W_A
count_key$weight[count_key$Treat_lvl == 'Medium'] <- W_F
count_key$weight[count_key$Treat_lvl == 'High'] <- W_Z

dat_key <- dat_key %>%
  mutate(weight2 = case_when(
    Treat_lvl == 'Low' ~ W_A,
    Treat_lvl == 'Medium'~ W_F,
    Treat_lvl == 'High' ~ W_Z
  ))
head(dat_key)
range(dat_key$weight2)
range(dat_key$weight)

dat_test <- subset(dat_key, year != 2023)
range(dat_test$year)

mixed.lmer <- lmer(log_sds ~ Treat_lvl + north + spr_temp + spr_prec  + (1|year), weight = weight2,  data = dat_key)  # intercept
summary(mixed.lmer)

r_sq_val <- r.squaredGLMM(mixed.lmer)
print(r_sq_val)

r.squaredGLMM(mixed.lmer)


plt <- ggplot(
  data = dat_key
) +
  geom_boxplot(
    aes(x = year, y = sd_eos, group = interaction(year, Treat_lvl, plot), color = Treat_lvl)
  )+ 
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
    title = paste("SD EOS compare", sep = " "),
    y = "DOY", x = "year"
  ) +
  scale_x_continuous(
    breaks = c(2017:2023)
  ) +
  theme_minimal(
    base_size = 20
  ) +
  theme(panel.grid = element_blank())+
  facet_wrap(
    ~plot
  )
plt


plt <- ggplot(
  data = dat_key
) +
  geom_boxplot(
    aes(x = Treat_lvl, y = sd_eos, group = Treat_lvl, color = Treat_lvl)
  ) +
  labs(
    title = paste("SD EOS compare", sep = " "),
    y = "SD", x = "Treat", color = 'Treat'
  ) +
  theme_minimal(
    base_size = 20
  ) +
  theme(panel.grid = element_blank())

plt

plt <- ggplot(
  data = dat_key
) +
  geom_boxplot(
    aes(x = Treat_lvl, y = sd_sos, group = Treat_lvl, color = Treat_lvl)
  ) + 
  labs(
    title = paste("SD SOS compare", sep = " "),
    y = "SD", x = "Treat", color = 'Treat'
  ) +
  theme_minimal(
    base_size = 20
  ) +
  theme(panel.grid = element_blank())

plt


### Shapiro-Wilk test of normality for one variable
### p-value > 0.05 assumes normal distribution

sha.test <- shapiro.test(dat_key$log_sds)
if (sha.test$p.value > 0.05) {norm <- "normal distribution"} else{norm <- "non-normal distribution"}
norm


my_comparisons <- list( c("Low", "Medium"), c("Medium", "High"), c("Low", "High") )

p_sos <-  ggboxplot(dat_key, x = "Treat_lvl", y = "log_sds", 
                    color = "Treat_lvl", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                    order = c("Low", "Medium", "High"), add = "jitter")+ 
  labs(
    title = paste("LOG SD SOS compare", sep = " "),
    y = "Log(SD SOS)", x = "", color = 'Treat'
  ) +
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", label.y = c(2.5, 2.5, 3), size = 5)+ # Add pairwise comparisons p-value
  stat_compare_means(method = "anova", label.y = 4, size = 5) +    # Add global anova p-value
  theme_classic(base_size = 20)
p_sos

p_eos <-  ggboxplot(dat_key, x = "Treat_lvl", y = "log_sde", 
                    color = "Treat_lvl", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                    order = c("Low", "Medium", "High"), add = "jitter")+ 
  labs(
    title = paste("LOG SD EOS compare", sep = " "),
    y = "Log(SD EOS)", x = "", color = 'Treat'
  ) +
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", label.y = c(3.5, 3.5, 4), size = 5)+ # Add pairwise comparisons p-value
  stat_compare_means(method = "anova", label.y = 5, size = 5) +    # Add global anova p-value
  theme_classic(base_size = 20)
p_eos


p_sos <-  ggboxplot(dat_key, x = "Treat_lvl", y = "sd_sos", 
                    color = "Treat_lvl", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                    order = c("Low", "Medium", "High"), add = "jitter")+ 
  labs(
    title = paste("SD SOS compare", sep = " "),
    y = "SD SOS", x = "", color = 'Treat'
  ) +
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", label.y = c(7.5, 7.5, 8.5), size = 5)+ # Add pairwise comparisons p-value
  stat_compare_means(method = "anova", label.y = 9.5, size = 5) +    # Add global anova p-value
  theme_classic(base_size = 20)
p_sos


p_eos <-  ggboxplot(dat_key, x = "Treat_lvl", y = "sd_eos", 
                    color = "Treat_lvl", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
                    order = c("Low", "Medium", "High"), add = "jitter")+ 
  labs(
    title = paste("SD EOS compare", sep = " "),
    y = "SD EOS", x = "", color = 'Treat'
  ) +
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", label.y = c(21, 21, 23), size = 5)+ # Add pairwise comparisons p-value
  stat_compare_means(method = "anova", label.y = 26, size = 5) +    # Add global anova p-value
  theme_classic(base_size = 20)
p_eos

####################################
#####################
### Random Forest ###
#####################

library(randomForest)
library(vip)

dat_forest <- sub_wek[, c('sos', 'Treat_lvl', 'north', 'STMean', 'SPrec')]

tuneRF( x = dat_forest[, -which(names(dat_forest) == 'sos')],
        y = dat_forest$sos,
        mtryStart = 2,
        ntreeTry = 200,
        stepFactor = 1.5,
        improve = 0.01,
        plot = TRUE)


set.seed(1)
forest <- randomForest(sos ~ ., data = dat_forest, ntree = 200, mtry = 4, localImp = TRUE)

plot(forest, main = "Learning curve of the forest")


## RMSE & R sqr
rf_model <- forest
RMSE <- min(sqrt(rf_model$mse))
RMSE
max(rf_model$rsq)


#### SD

dat_forest <- na.omit(dat_key[, c('sd_eos', 'Treat_lvl', 'north', 'STMean', 'SPrec')])

tuneRF( x = dat_forest[, -which(names(dat_forest) == 'sd_eos')],
        y = dat_forest$sd_eos,
        mtryStart = 2,
        ntreeTry = 200,
        stepFactor = 1.5,
        improve = 0.01,
        plot = TRUE)


set.seed(1)
forest <- randomForest(sd_eos ~ ., data = dat_forest, ntree = 200, mtry = 2, localImp = TRUE)

plot(forest, main = "Learning curve of the forest")


## RMSE & R sqr
rf_model <- forest
RMSE <- min(sqrt(rf_model$mse))
RMSE
max(rf_model$rsq)

#### mean per parcel

dat_forest <- dat_key[, c('m_sos', 'Treat_lvl', 'north', 'STMean', 'SPrec')]

tuneRF( x = dat_forest[, -which(names(dat_forest) == 'm_sos')],
        y = dat_forest$m_sos,
        mtryStart = 2,
        ntreeTry = 200,
        stepFactor = 1.5,
        improve = 0.01,
        plot = TRUE)


set.seed(1)
forest <- randomForest(m_sos ~ ., data = dat_forest, ntree = 200, mtry = 3, localImp = TRUE)

plot(forest, main = "Learning curve of the forest")


## RMSE & R sqr
rf_model <- forest
RMSE <- min(sqrt(rf_model$mse))
RMSE
max(rf_model$rsq)


################################################################################

##########################
### Linear Model Ver.2 ###
##########################

#### meta data change
dat_meta <- read.csv('meta_data.csv')
dat_meta

sub_wek2 <- sub_wek[, !names(sub_wek) %in%  c('Treat', 'Treat_lvl', 'Age', 'weight')]
sub_wek2 <- merge(sub_wek2, dat_meta, by = 'key')
head(sub_wek2)


# Treat to level, L M H

tr <- sub_wek2$Treat
tr[tr == 'A-Grad'] <- 'Low'
tr[tr == 'F-Grad'] <- 'Medium'
tr[tr == 'Z-Baum'] <- 'High'

sub_wek2$Treat_lvl <- factor(tr, levels = c('Low', 'Medium', 'High'))

head(sub_wek2)

# add weight column

# count <- sub_wek2  %>%
#   count(plot, Treat_lvl, name = 'count')
# count

count <- sub_wek2  %>%
  count(Treat_lvl, name = 'count')
count

n_plot <- unique(count$plot)

sum <- sum(count$count)

W_A <- sum/count$count[1]
W_F <- sum/count$count[2]
W_Z <- sum/count$count[3]

count$weight[count$Treat_lvl == 'Low'] <- W_A
count$weight[count$Treat_lvl == 'Medium'] <- W_F
count$weight[count$Treat_lvl == 'High'] <- W_Z

sub_wek2 <- sub_wek2 %>%
  mutate(weight = case_when(
    Treat_lvl == 'Low' ~ W_A,
    Treat_lvl == 'Medium'~ W_F,
    Treat_lvl == 'High' ~ W_Z
  ))
head(sub_wek2)


mixed.lmer <- lmer(sos ~ Treat_lvl + north + spr_temp + spr_prec + (1|year), data = sub_wek2)  # intercept
summary(mixed.lmer)
mixed.lmer
summary(mixed.lmer)$coefficients

#mixed.lmer <- lmer(sos ~ Treat_lvl + north + spr_temp + spr_prec + (1|year), weight = weight, data = sub_wek2)  # intercept
#summary(mixed.lmer)

#library(MuMIn)
r_sq_val <- r.squaredGLMM(mixed.lmer)
print(r_sq_val)

r.squaredGLMM(mixed.lmer)



#### calculate SD of each index

key_list <- unique(sub_wek2$key_yr)
key_list


for (key_i in 1:length(key_list)){
  key_n <- key_list[key_i]
  sub_key2 <- subset(sub_wek2, key_yr == key_n)
  plot <- unique(sub_key2$plot)
  par <- unique(sub_key2$parcel)
  treat <- unique(sub_key2$Treat_lvl)
  age <- unique(sub_key2$Age)
  yr <- unique(sub_key2$year)
  north <- unique(sub_key2$north)
  spr_temp <- mean(sub_key2$spr_temp)
  spr_prec <- mean(sub_key2$spr_prec)
  STMean <- mean(sub_key2$STMean)
  SPrec <- mean(sub_key2$SPrec)
  weight <- unique(sub_key2$weight)
  
  # count number of non-NA pixels
  pix_sos <- sum(!is.na(sub_key2$sos))
  pix_eos <- sum(!is.na(sub_key2$eos))
  pix_los <- sum(!is.na(sub_key2$los))
  
  # calculate sd of each index
  sd_sos <- sd(sub_key2$sos)
  sd_eos <- sd(sub_key2$eos)
  sd_los <- sd(sub_key2$los)
  
  # calculate mean of each index
  m_sos <- mean(sub_key2$sos)
  m_eos <- mean(sub_key2$eos)
  m_los <- mean(sub_key2$los)
  
  
  # calculate CV (Coefficient of Variation)
  cv_sos <- sd_sos/m_sos
  cv_eos <- sd_eos/m_eos
  cv_los <- sd_los/m_los
  
  new <- data.frame(key_yr = key_n, plot = plot, parcel = par, year = yr, 
                    Treat_lvl = treat, Age = age, weight = weight,
                    north = north, spr_temp = spr_temp, spr_prec = spr_prec, 
                    STMean = STMean, SPrec = SPrec,
                    pix_sos = pix_sos, pix_eos = pix_eos, pix_los = pix_los,
                    m_sos = m_sos, m_eos = m_eos, m_los = m_los,
                    sd_sos = sd_sos, sd_eos = sd_eos, sd_los = sd_los, 
                    cv_sos = cv_sos, cv_eos = cv_eos, cv_los = cv_los)
  
  if (key_i == 1){
    dat_key2 <- new
  }else{
    dat_key2 <- rbind(dat_key2, new)
  }
  
}

head(dat_key2)
summary(dat_key2)

dat_key2$log_sds <- log(dat_key2$sd_sos)
dat_key2$log_sde <- log(dat_key2$sd_eos)
dat_key2$log_sdl <- log(dat_key2$sd_los)

dat_key2$log_cvs <- log(dat_key2$cv_sos)
dat_key2$log_cve <- log(dat_key2$cv_eos)
dat_key2$log_cvl <- log(dat_key2$cv_los)


dat_test2 <- subset(dat_key2, year != 2023)
range(dat_test$year)

mixed.lmer <- lmer(log_sde ~ Treat_lvl + north + spr_temp + spr_prec  + (1|year), weight = weight,  data = dat_key2)  # intercept
summary(mixed.lmer)

r_sq_val <- r.squaredGLMM(mixed.lmer)
print(r_sq_val)

r.squaredGLMM(mixed.lmer)


plt <- ggplot(
  data = dat_key2
) +
  geom_boxplot(
    aes(x = year, y = sd_eos, group = interaction(year, Treat_lvl, plot), color = Treat_lvl)
  )+ 
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
    title = paste("SD EOS compare", sep = " "),
    y = "DOY", x = "year"
  ) +
  scale_x_continuous(
    breaks = c(2017:2023)
  ) +
  theme_minimal(
    base_size = 20
  ) +
  theme(panel.grid = element_blank())+
  facet_wrap(
    ~plot
  )
plt


plt <- ggplot(
  data = dat_key2
) +
  geom_boxplot(
    aes(x = year, y = sd_eos, group = Treat_lvl, color = Treat_lvl)
  )+ 
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
    title = paste("SD EOS compare", sep = " "),
    y = "DOY", x = "year"
  ) +
  scale_x_continuous(
    breaks = c(2017:2023)
  ) +
  theme_minimal(
    base_size = 20
  ) +
  theme(panel.grid = element_blank())

plt

plt <- ggplot(
  data = dat_key2
) +
  geom_boxplot(
    aes(x = year, y = sd_sos, group = Treat_lvl, color = Treat_lvl)
  )+ 
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
    title = paste("SD SOS compare", sep = " "),
    y = "DOY", x = "year"
  ) +
  scale_x_continuous(
    breaks = c(2017:2023)
  ) +
  theme_minimal(
    base_size = 20
  ) +
  theme(panel.grid = element_blank())

plt
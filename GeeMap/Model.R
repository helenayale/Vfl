library(dplyr)
library(ggpubr)
library(ggplot2)
library(lme4)
library(lmerTest) # package to show p-value

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
dat_pheno_SG <- read.csv('dat_pheno_pix_SG_impu.csv')
dat_pheno_SG$key <- paste(dat_pheno_SG$plot, '_', dat_pheno_SG$parcel, sep = '')
dat_pheno_SG <- merge(dat_pheno_SG, dat_meta, by = 'key')
head(dat_pheno_SG)
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
library(naniar)
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
dat_wek$los <- dat_wek$eos - dat_wek$sos


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
dat_pheno_VPP <- dat_wek

# write.csv(dat_pheno_SG, 'dat_mod_SG_impu.csv')
# write.csv(dat_pheno_DL, 'dat_mod_DL.csv')
# write.csv(dat_wek, 'dat_mod_VPP.csv')

# # load data
# dat_pheno_SG <- read.csv('dat_mod_SG_impu.csv')
# dat_pheno_DL <- read.csv('dat_mod_DL.csv')
# dat_pheno_VPP <- read.csv('dat_mod_VPP.csv')

################################################################################

###################
### define data ###
###################


data_names <- c('dat_pheno_SG', 'dat_pheno_DL', 'dat_pheno_VPP')
title_names <- c('Savitzy-Golay', 'DLogistic', 'WekEO-VPP')
abbr_names <- c('SG', 'DL', 'VPP')

data_i <- 1
eval(parse(text = paste('dat_merge <- ', data_names[data_i], sep = '')))
head(dat_merge)


# 
# mixed.lmer <- lmer(eos ~ Treat * year + (1|plot), data = dat_merge)  # intercept
# 
# summary(mixed.lmer)
# 

#### Hausman Test
#### check fixed/random effect

# library(plm)
# # install.packages('plm') 
# # test year
# fixed <- plm(sos ~ Treat, data = dat_merge, index = c("year"), model = "within")  #fixed model
# random <- plm(sos ~ Treat, data = dat_merge, index = c("year"), model = "random")  #random model
# phtest(fixed,random) #Hausman test
# 
# # If the p-value is significant (for example <0.05) then use fixed effects, if not use random effects. If p value is slightly larger than 0.05, it may still be better to use fixed effects models.
# summary(fixed)
# summary(random)
# 
# # test plot
# fixed <- plm(sos ~ Treat, data = dat_merge, index = c("plot"), model = "within")  #fixed model
# random <- plm(sos ~ Treat, data = dat_merge, index = c("plot"), model = "random")  #random model
# phtest(fixed,random) #Hausman test
# 
# # If the p-value is significant (for example <0.05) then use fixed effects, if not use random effects. If p value is slightly larger than 0.05, it may still be better to use fixed effects models.
# summary(fixed)
# summary(random)


#######################################
##### for SG data, sos remove 2017#####
#######################################
# 
# dat_sos <- subset(dat_merge, dat_merge$year != 2017)
# mixed.lmer <- lmer(sos ~ Treat + north + spr_temp + STEBO + (1|year), data = dat_sos)  # intercept
# summary(mixed.lmer)

# mixed.lmer <- lmer(eos ~ Treat + north + aut_temp + STEBO + (1|year), data = dat_sos)  # intercept
# summary(mixed.lmer)
# 
# mixed.lmer <- lmer(los ~ Treat + north + spr_temp + aut_temp + DWD_LOS  + (1|year), data = dat_sos)  # intercept
# summary(mixed.lmer)


mixed.lmer <- lmer(sos ~ Treat + north + spr_temp + STEBO  + (1|year), data = dat_merge)  # intercept
summary(mixed.lmer)
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



mixed.lmer <- lmer(eos ~ Treat + north + aut_temp + STEBV + (1|year), data = dat_merge)  # intercept
summary(mixed.lmer)



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
                      north = north, STEBO = STEBO, STEBV = STEBV, DWD_LOS = DWD_LOS, 
                      spr_temp = spr_temp, sum_temp = sum_temp, aut_temp = aut_temp, 
                      pix_sos = pix_sos, pix_eos = pix_eos, pix_los = pix_los, win_temp = win_temp,
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


dat_test <- dat_key
head(dat_test)

# 
# dat_test$roh_plot <- dat_test$plot
# dat_test$roh_plot <- replace(dat_test$roh_plot, dat_test$roh_plot == 'Roh620', 'Roh')
# dat_test$roh_plot <- replace(dat_test$roh_plot, dat_test$roh_plot == 'Roh635', 'Roh')
# dat_test$roh_plot <- replace(dat_test$roh_plot, dat_test$roh_plot == 'Roh90', 'Roh')
# dat_test$roh_plot



# # Summarize data
# sum_data <-
#   dat_merge %>%
#   group_by(plot) %>%
#   summarize(m_sos = mean(sos),
#             m_eos = mean(eos),
#             sd_sos = sd(sos),
#             sd_eso = sd(eos), .groups = 'keep')


# basic.lm <- lm(sd_sos ~Treat, dat_test)


#### Hausman Test
#### check fixed/random effect
# 
# library(plm)
# # install.packages('plm') 
# fixed <- plm(log_sds ~ Treat, data = dat_test, index = c("year"), model = "within")  #fixed model
# random <- plm(log_sds ~ Treat, data = dat_test, index = c("year"), model = "random")  #random model
# phtest(fixed,random) #Hausman test
# 
# # If the p-value is significant (for example <0.05) then use fixed effects, if not use random effects. If p value is slightly larger than 0.05, it may still be better to use fixed effects models.
# summary(fixed)
# summary(random)
# 
# # test plot
# fixed <- plm(log_sds ~ Treat, data = dat_test, index = c("plot"), model = "within")  #fixed model
# random <- plm(log_sds ~ Treat, data = dat_test, index = c("plot"), model = "random")  #random model
# phtest(fixed,random) #Hausman test

# mixed.lmer <- lmer(log_sds ~ Treat + year + (1|plot) , data = dat_test) # year as fixed effect
# summary(mixed.lmer)
# 
# mixed.lmer <- lmer(log_sds ~ Treat * year + (1|plot) , data = dat_test) # interaction
# summary(mixed.lmer)

mixed.lmer <- lmer(log_sds ~ Treat + north + spr_temp + STEBO  + (1|year), data = dat_test) # year as random effect
summary(mixed.lmer)



mixed.lmer <- lmer(log_sde ~ Treat + north + aut_temp + STEBV  + (1|year), data = dat_test) # year as random effect
summary(mixed.lmer)



mixed.lmer <- lmer(log_sdl ~ Treat + north + spr_temp + aut_temp  + DWD_LOS + (1|year), data = dat_test) # year as random effect
summary(mixed.lmer)


# plot(mixed.lmer) 
# 
# qqnorm(resid(mixed.lmer))
# qqline(resid(mixed.lmer))



mixed.lmer <- lmer(log_cvs ~ Treat + north + spr_temp + STEBO  + (1|year), data = dat_test) # year as random effect
summary(mixed.lmer)




mixed.lmer <- lmer(log_cve ~ Treat + north + aut_temp + STEBV  + (1|year), data = dat_test) # year as random effect
summary(mixed.lmer)



mixed.lmer <- lmer(log_cvl ~ Treat + north + spr_temp + aut_temp  + DWD_LOS + (1|year), data = dat_test) # year as random effect
summary(mixed.lmer)


# library(cAIC4)
# step <- stepcAIC(mixed.lmer, direction = "backward", trace = TRUE, data = dat_test)

# 
# (mm_plot <- ggplot(dat_test, aes(x = Treat, y = sd_sos, colour = Treat)) +
#     facet_wrap(~year, nrow=3) +   # a panel for each Age
#     geom_point(alpha = 0.5) +
#     theme_classic() +
#     geom_line(data = cbind(dat_test, pred = predict(mixed.lmer)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
#     theme(legend.position = "none",
#           panel.spacing = unit(2, "lines"))  # adding space between panels
# )
# 
# library(ggeffects)
# # Extract the prediction data frame
# pred.mm <- ggpredict(mixed.lmer, terms = c("Treat"))  # this gives overall predictions for the model
# 
# # Plot the predictions 
# 
# (ggplot(pred.mm) + 
#     geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
#                 fill = "lightgrey", alpha = 0.5) +  # error band
#     geom_boxplot(data = dat_test,                      # adding the raw data (scaled values)
#                  aes(x = Treat, y = sd_sos, colour = Age)) + 
#     labs(x = "Treatment", y = "SD of SOS", 
#          title = "Treatment affects SD of SOS") + 
#     theme_minimal()
# )
# 
# 
# ggpredict(mixed.lmer, terms = c("Treat", "Age"), type = "re") %>% 
#   plot() +
#   labs(x = "Treat", y = "SD of SOS", title = "Effect of Treatment on SOS") + 
#   theme_minimal()
# 
# 
# library(stargazer)
# 
# stargazer(mixed.lmer, type = "text",
#           digits = 3,
#           star.cutoffs = c(0.05, 0.01, 0.001),
#           digit.separator = "")
# 
# #### SOS ####
# 
# mixed.lmer <- lmer(log(sd_sos) ~ Treat + new_plot + (1|year), data = dat_test) # year as random effect
# # plot(mixed.lmer)
# 
# # Extract the prediction data frame
# pred.mm <- ggpredict(mixed.lmer, terms = c("Treat"))  # this gives overall predictions for the model
# 
# # Plot the predictions 
# 
# (ggplot(pred.mm) + 
#     geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
#                 fill = "lightgrey", alpha = 0.5) +  # error band
#     geom_boxplot(data = dat_test,                      # adding the raw data (scaled values)
#                  aes(x = Treat, y = sd_sos, color = Treat )) + 
#     labs(x = "Treatment", y = "SD of SOS", 
#          title = "Treatment affects SD of SOS") + 
#     theme_minimal()
# )
# 
# 
# #### EOS ####
# 
# mixed.lmer2 <- lmer(log(sd_eos) ~ Treat + new_plot + (1|year), data = dat_test) # the syntax stays the same, but now the nesting is taken into account
# summary(mixed.lmer2)
# # plot(mixed.lmer2) 
# 
# 
# # Extract the prediction data frame
# pred.mm <- ggpredict(mixed.lmer2, terms = c("Treat"))  # this gives overall predictions for the model
# 
# # Plot the predictions 
# 
# (ggplot(pred.mm) + 
#     geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
#                 fill = "lightgrey", alpha = 0.5) +  # error band
#     geom_boxplot(data = dat_test,                      # adding the raw data (scaled values)
#                  aes(x = Treat, y = sd_eos, colour = Treat)) + 
#     labs(x = "Treatment", y = "SD of EOS", 
#          title = "Treatment affects SD of EOS") + 
#     theme_minimal()
# )
# 
# 
# #### LOS ####
# 
# mixed.lmer3 <- lmer(log(sd_los) ~ Treat + new_plot + (1|year), data = dat_test) # the syntax stays the same, but now the nesting is taken into account
# summary(mixed.lmer3)
# # plot(mixed.lmer3) 
# 
# 
# # Extract the prediction data frame
# pred.mm <- ggpredict(mixed.lmer3, terms = c("Treat"))  # this gives overall predictions for the model
# 
# # Plot the predictions 
# 
# (ggplot(pred.mm) + 
#     geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
#                 fill = "lightgrey", alpha = 0.5) +  # error band
#     geom_boxplot(data = dat_test,                      # adding the raw data (scaled values)
#                  aes(x = Treat, y = sd_los, colour = Treat)) + 
#     labs(x = "Treatment", y = "SD of LOS", 
#          title = "Treatment affects SD of LOS") + 
#     theme_minimal()
# )



################################################################################

####################
####### Loop #######
####################


data_names <- c('dat_pheno_SG', 'dat_pheno_DL', 'dat_pheno_VPP')
title_names <- c('Savitzy-Golay', 'DLogistic', 'WekEO-VPP')
abbr_names <- c('SG', 'DL', 'VPP')

for(data_i in 1:3){
  eval(parse(text = paste('dat_merge <- ', data_names[data_i], sep = '')))
  head(dat_merge)
  
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
    
    neww <- data.frame(key = key_n, plot = plot, parcel = par, year = yr, Treat = treat, Age = age, 
                      north = north, STEBO = STEBO, STEBV = STEBV, DWD_LOS = DWD_LOS, 
                      spr_temp = spr_temp, sum_temp = sum_temp, aut_temp = aut_temp, 
                      pix_sos = pix_sos, pix_eos = pix_eos, pix_los = pix_los, win_temp = win_temp,
                      m_sos = m_sos, m_eos = m_eos, m_los = m_los,
                      sd_sos = sd_sos, sd_eos = sd_eos, sd_los = sd_los, 
                      cv_sos = cv_sos, cv_eos = cv_eos, cv_los = cv_los)
    
    if (key_i == 1){
      dat_key <- neww
    }else{
      dat_key <- rbind(dat_key, neww)
    }
    
  }
  
  
  dat_key$log_sds <- log(dat_key$sd_sos)
  dat_key$log_sde <- log(dat_key$sd_eos)
  dat_key$log_sdl <- log(dat_key$sd_los)
  
  dat_key$log_cvs <- log(dat_key$cv_sos)
  dat_key$log_cve <- log(dat_key$cv_eos)
  dat_key$log_cvl <- log(dat_key$cv_los)
  
  dat_test <- dat_key
  
  ### model sos, eos, los
  var_names <- c('sos', 'eos', 'los')
  for (i in 1:3){
    var <- var_names[i]
    if (i == 1){
      eval(parse(text = paste('mixed.lmer <- lmer(', var, ' ~ Treat + north + spr_temp + STEBO  + (1|year), data = dat_merge)', sep = '')))
    }else if (i == 2){
      eval(parse(text = paste('mixed.lmer <- lmer(', var, ' ~ Treat + north + aut_temp + STEBO  + (1|year), data = dat_merge)', sep = '')))
    }else {
      eval(parse(text = paste('mixed.lmer <- lmer(', var, ' ~ Treat + north + spr_temp + aut_temp + STEBO  + (1|year), data = dat_merge)', sep = '')))
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
    
    if(i == 1){
      spr_temp <- summary(mixed.lmer)$coefficients[5,1]
      p.value <- summary(mixed.lmer)$coefficients[5,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      spr_p <- pvalue
      
      aut_temp <- NA
      aut_p <- NA
      
      DWD <- summary(mixed.lmer)$coefficients[6,1]
      p.value <- summary(mixed.lmer)$coefficients[6,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      DWD_p <- pvalue
    }else if (i == 2){
      spr_temp <- NA
      spr_p <- NA
      
      aut_temp <- summary(mixed.lmer)$coefficients[5,1]
      p.value <- summary(mixed.lmer)$coefficients[5,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      aut_p <- pvalue
      
      DWD <- summary(mixed.lmer)$coefficients[6,1]
      p.value <- summary(mixed.lmer)$coefficients[6,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      DWD_p <- pvalue
    }else{
      spr_temp <- summary(mixed.lmer)$coefficients[5,1]
      p.value <- summary(mixed.lmer)$coefficients[5,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      spr_p <- pvalue
      
      aut_temp <- summary(mixed.lmer)$coefficients[6,1]
      p.value <- summary(mixed.lmer)$coefficients[6,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      aut_p <- pvalue
      
      DWD <- summary(mixed.lmer)$coefficients[7,1]
      p.value <- summary(mixed.lmer)$coefficients[7,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      DWD_p <- pvalue
    }
    
    
    new <- data.frame(Y = var, data =  abbr_names[data_i], intercept = inter, int_p = int_p,
                      F_Grad = F_Grad, F_p = F_p, Z_Baum = Z_Baum, Z_p = Z_p,
                      north = north,north_p = north_p, spr_temp = spr_temp, spr_p = spr_p,
                      aut_temp = aut_temp, aut_p = aut_p, DWD = DWD, DWD_p = DWD_p)
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

  
  # model sds, sde, sdl
  var_names <- c('sds', 'sde', 'sdl')
  for (j in 1:3){
    var <- var_names[j]
    if (j == 1){
      eval(parse(text = paste('mixed.lmer <- lmer(log_', var, ' ~ Treat + north + spr_temp + STEBO  + (1|year), data = dat_test)', sep = '')))
    }else if (j == 2){
      eval(parse(text = paste('mixed.lmer <- lmer(log_', var, ' ~ Treat + north + aut_temp + STEBO  + (1|year), data = dat_test)', sep = '')))
    }else {
      eval(parse(text = paste('mixed.lmer <- lmer(log_', var, ' ~ Treat + north + spr_temp + aut_temp + STEBO  + (1|year), data = dat_test)', sep = '')))
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
    
    if(j == 1){
      spr_temp <- summary(mixed.lmer)$coefficients[5,1]
      p.value <- summary(mixed.lmer)$coefficients[5,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      spr_p <- pvalue
      
      aut_temp <- NA
      aut_p <- NA
      
      DWD <- summary(mixed.lmer)$coefficients[6,1]
      p.value <- summary(mixed.lmer)$coefficients[6,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      DWD_p <- pvalue
    }else if (j == 2){
      spr_temp <- NA
      spr_p <- NA
      
      aut_temp <- summary(mixed.lmer)$coefficients[5,1]
      p.value <- summary(mixed.lmer)$coefficients[5,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      aut_p <- pvalue
      
      DWD <- summary(mixed.lmer)$coefficients[6,1]
      p.value <- summary(mixed.lmer)$coefficients[6,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      DWD_p <- pvalue
    }else{
      spr_temp <- summary(mixed.lmer)$coefficients[5,1]
      p.value <- summary(mixed.lmer)$coefficients[5,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      spr_p <- pvalue
      
      aut_temp <- summary(mixed.lmer)$coefficients[6,1]
      p.value <- summary(mixed.lmer)$coefficients[6,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      aut_p <- pvalue
      
      DWD <- summary(mixed.lmer)$coefficients[7,1]
      p.value <- summary(mixed.lmer)$coefficients[7,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      DWD_p <- pvalue
    }
    
    
    new <- data.frame(Y = var, data =  abbr_names[data_i], intercept = inter, int_p = int_p,
                      F_Grad = F_Grad, F_p = F_p, Z_Baum = Z_Baum, Z_p = Z_p,
                      north = north,north_p = north_p, spr_temp = spr_temp, spr_p = spr_p,
                      aut_temp = aut_temp, aut_p = aut_p, DWD = DWD, DWD_p = DWD_p)
 
    mod_summary <- rbind(mod_summary, new)
    
  }


  
  # model cvs, cve, cvl
  var_names <- c('cvs', 'cve', 'cvl')
  for (k in 1:3){
    var <- var_names[k]
    if (k == 1){
      eval(parse(text = paste('mixed.lmer <- lmer(log_', var, ' ~ Treat + north + spr_temp + STEBO  + (1|year), data = dat_test)', sep = '')))
    }else if (k == 2){
      eval(parse(text = paste('mixed.lmer <- lmer(log_', var, ' ~ Treat + north + aut_temp + STEBO  + (1|year), data = dat_test)', sep = '')))
    }else {
      eval(parse(text = paste('mixed.lmer <- lmer(log_', var, ' ~ Treat + north + spr_temp + aut_temp + STEBO  + (1|year), data = dat_test)', sep = '')))
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
    
    if(k == 1){
      spr_temp <- summary(mixed.lmer)$coefficients[5,1]
      p.value <- summary(mixed.lmer)$coefficients[5,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      spr_p <- pvalue
      
      aut_temp <- NA
      aut_p <- NA
      
      DWD <- summary(mixed.lmer)$coefficients[6,1]
      p.value <- summary(mixed.lmer)$coefficients[6,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      DWD_p <- pvalue
    }else if (k == 2){
      spr_temp <- NA
      spr_p <- NA
      
      aut_temp <- summary(mixed.lmer)$coefficients[5,1]
      p.value <- summary(mixed.lmer)$coefficients[5,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      aut_p <- pvalue
      
      DWD <- summary(mixed.lmer)$coefficients[6,1]
      p.value <- summary(mixed.lmer)$coefficients[6,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      DWD_p <- pvalue
    }else{
      spr_temp <- summary(mixed.lmer)$coefficients[5,1]
      p.value <- summary(mixed.lmer)$coefficients[5,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      spr_p <- pvalue
      
      aut_temp <- summary(mixed.lmer)$coefficients[6,1]
      p.value <- summary(mixed.lmer)$coefficients[6,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      aut_p <- pvalue
      
      DWD <- summary(mixed.lmer)$coefficients[7,1]
      p.value <- summary(mixed.lmer)$coefficients[7,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      DWD_p <- pvalue
    }
    
    
    new <- data.frame(Y = var, data =  abbr_names[data_i], intercept = inter, int_p = int_p,
                      F_Grad = F_Grad, F_p = F_p, Z_Baum = Z_Baum, Z_p = Z_p,
                      north = north,north_p = north_p, spr_temp = spr_temp, spr_p = spr_p,
                      aut_temp = aut_temp, aut_p = aut_p, DWD = DWD, DWD_p = DWD_p)
    
    mod_summary <- rbind(mod_summary, new)
    
  }
  

}


mod_summary

write.csv(mod_summary, 'mod_sum_aut.csv', row.names = FALSE)


################################################################################

###########################
####### Loop Summer #######
###########################


data_names <- c('dat_pheno_SG', 'dat_pheno_DL', 'dat_pheno_VPP')
title_names <- c('Savitzy-Golay', 'DLogistic', 'WekEO-VPP')
abbr_names <- c('SG', 'DL', 'VPP')

for(data_i in 1:3){
  eval(parse(text = paste('dat_merge <- ', data_names[data_i], sep = '')))
  head(dat_merge)
  
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
    
    neww <- data.frame(key = key_n, plot = plot, parcel = par, year = yr, Treat = treat, Age = age, 
                       north = north, STEBO = STEBO, STEBV = STEBV, DWD_LOS = DWD_LOS, 
                       spr_temp = spr_temp, sum_temp = sum_temp, aut_temp = aut_temp, 
                       pix_sos = pix_sos, pix_eos = pix_eos, pix_los = pix_los, win_temp = win_temp,
                       m_sos = m_sos, m_eos = m_eos, m_los = m_los,
                       sd_sos = sd_sos, sd_eos = sd_eos, sd_los = sd_los, 
                       cv_sos = cv_sos, cv_eos = cv_eos, cv_los = cv_los)
    
    if (key_i == 1){
      dat_key <- neww
    }else{
      dat_key <- rbind(dat_key, neww)
    }
    
  }
  
  
  dat_key$log_sds <- log(dat_key$sd_sos)
  dat_key$log_sde <- log(dat_key$sd_eos)
  dat_key$log_sdl <- log(dat_key$sd_los)
  
  dat_key$log_cvs <- log(dat_key$cv_sos)
  dat_key$log_cve <- log(dat_key$cv_eos)
  dat_key$log_cvl <- log(dat_key$cv_los)
  
  dat_test <- dat_key
  
  ### model sos, eos, los
  var_names <- c('sos', 'eos', 'los')
  for (i in 1:3){
    var <- var_names[i]
    if (i == 1){
      eval(parse(text = paste('mixed.lmer <- lmer(', var, ' ~ Treat + north + spr_temp + STEBO  + (1|year), data = dat_merge)', sep = '')))
    }else if (i == 2){
      eval(parse(text = paste('mixed.lmer <- lmer(', var, ' ~ Treat + north + sum_temp + STEBO  + (1|year), data = dat_merge)', sep = '')))
    }else {
      eval(parse(text = paste('mixed.lmer <- lmer(', var, ' ~ Treat + north + spr_temp + sum_temp + STEBO  + (1|year), data = dat_merge)', sep = '')))
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
    
    if(i == 1){
      spr_temp <- summary(mixed.lmer)$coefficients[5,1]
      p.value <- summary(mixed.lmer)$coefficients[5,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      spr_p <- pvalue
      
      sum_temp <- NA
      sum_p <- NA

      DWD <- summary(mixed.lmer)$coefficients[6,1]
      p.value <- summary(mixed.lmer)$coefficients[6,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      DWD_p <- pvalue
    }else if (i == 2){
      spr_temp <- NA
      spr_p <- NA
      
      sum_temp <- summary(mixed.lmer)$coefficients[5,1]
      p.value <- summary(mixed.lmer)$coefficients[5,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      sum_p <- pvalue
      
      DWD <- summary(mixed.lmer)$coefficients[6,1]
      p.value <- summary(mixed.lmer)$coefficients[6,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      DWD_p <- pvalue
    }else{
      spr_temp <- summary(mixed.lmer)$coefficients[5,1]
      p.value <- summary(mixed.lmer)$coefficients[5,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      spr_p <- pvalue
      
      sum_temp <- summary(mixed.lmer)$coefficients[6,1]
      p.value <- summary(mixed.lmer)$coefficients[6,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      sum_p <- pvalue
      
      DWD <- summary(mixed.lmer)$coefficients[7,1]
      p.value <- summary(mixed.lmer)$coefficients[7,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      DWD_p <- pvalue
    }
    
    
    new <- data.frame(Y = var, data =  abbr_names[data_i], intercept = inter, int_p = int_p,
                      F_Grad = F_Grad, F_p = F_p, Z_Baum = Z_Baum, Z_p = Z_p,
                      north = north,north_p = north_p, spr_temp = spr_temp, spr_p = spr_p,
                      sum_temp = sum_temp, sum_p = sum_p, DWD = DWD, DWD_p = DWD_p)
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
  
  
  # model sds, sde, sdl
  var_names <- c('sds', 'sde', 'sdl')
  for (j in 1:3){
    var <- var_names[j]
    if (j == 1){
      eval(parse(text = paste('mixed.lmer <- lmer(log_', var, ' ~ Treat + north + spr_temp + STEBO  + (1|year), data = dat_test)', sep = '')))
    }else if (j == 2){
      eval(parse(text = paste('mixed.lmer <- lmer(log_', var, ' ~ Treat + north + sum_temp + STEBO  + (1|year), data = dat_test)', sep = '')))
    }else {
      eval(parse(text = paste('mixed.lmer <- lmer(log_', var, ' ~ Treat + north + spr_temp + sum_temp + STEBO  + (1|year), data = dat_test)', sep = '')))
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
    
    if(j == 1){
      spr_temp <- summary(mixed.lmer)$coefficients[5,1]
      p.value <- summary(mixed.lmer)$coefficients[5,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      spr_p <- pvalue
      
      sum_temp <- NA
      sum_p <- NA
      
      DWD <- summary(mixed.lmer)$coefficients[6,1]
      p.value <- summary(mixed.lmer)$coefficients[6,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      DWD_p <- pvalue
    }else if (j == 2){
      spr_temp <- NA
      spr_p <- NA
      
      sum_temp <- summary(mixed.lmer)$coefficients[5,1]
      p.value <- summary(mixed.lmer)$coefficients[5,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      sum_p <- pvalue
      
      DWD <- summary(mixed.lmer)$coefficients[6,1]
      p.value <- summary(mixed.lmer)$coefficients[6,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      DWD_p <- pvalue
    }else{
      spr_temp <- summary(mixed.lmer)$coefficients[5,1]
      p.value <- summary(mixed.lmer)$coefficients[5,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      spr_p <- pvalue
      
      sum_temp <- summary(mixed.lmer)$coefficients[6,1]
      p.value <- summary(mixed.lmer)$coefficients[6,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      sum_p <- pvalue
      
      DWD <- summary(mixed.lmer)$coefficients[7,1]
      p.value <- summary(mixed.lmer)$coefficients[7,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      DWD_p <- pvalue
    }
    
    
    new <- data.frame(Y = var, data =  abbr_names[data_i], intercept = inter, int_p = int_p,
                      F_Grad = F_Grad, F_p = F_p, Z_Baum = Z_Baum, Z_p = Z_p,
                      north = north,north_p = north_p, spr_temp = spr_temp, spr_p = spr_p,
                      sum_temp = sum_temp, sum_p = sum_p, DWD = DWD, DWD_p = DWD_p)
    
    mod_summary <- rbind(mod_summary, new)
    
  }
  
  
  
  # model cvs, cve, cvl
  var_names <- c('cvs', 'cve', 'cvl')
  for (k in 1:3){
    var <- var_names[k]
    if (k == 1){
      eval(parse(text = paste('mixed.lmer <- lmer(log_', var, ' ~ Treat + north + spr_temp + STEBO  + (1|year), data = dat_test)', sep = '')))
    }else if (k == 2){
      eval(parse(text = paste('mixed.lmer <- lmer(log_', var, ' ~ Treat + north + sum_temp + STEBO  + (1|year), data = dat_test)', sep = '')))
    }else {
      eval(parse(text = paste('mixed.lmer <- lmer(log_', var, ' ~ Treat + north + spr_temp + sum_temp + STEBO  + (1|year), data = dat_test)', sep = '')))
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
    
    if(k == 1){
      spr_temp <- summary(mixed.lmer)$coefficients[5,1]
      p.value <- summary(mixed.lmer)$coefficients[5,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      spr_p <- pvalue
      
      sum_temp <- NA
      sum_p <- NA
      
      DWD <- summary(mixed.lmer)$coefficients[6,1]
      p.value <- summary(mixed.lmer)$coefficients[6,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      DWD_p <- pvalue
    }else if (k == 2){
      spr_temp <- NA
      spr_p <- NA
      
      sum_temp <- summary(mixed.lmer)$coefficients[5,1]
      p.value <- summary(mixed.lmer)$coefficients[5,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      sum_p <- pvalue
      
      DWD <- summary(mixed.lmer)$coefficients[6,1]
      p.value <- summary(mixed.lmer)$coefficients[6,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      DWD_p <- pvalue
    }else{
      spr_temp <- summary(mixed.lmer)$coefficients[5,1]
      p.value <- summary(mixed.lmer)$coefficients[5,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      spr_p <- pvalue
      
      sum_temp <- summary(mixed.lmer)$coefficients[6,1]
      p.value <- summary(mixed.lmer)$coefficients[6,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      sum_p <- pvalue
      
      DWD <- summary(mixed.lmer)$coefficients[7,1]
      p.value <- summary(mixed.lmer)$coefficients[7,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      DWD_p <- pvalue
    }
    
    
    new <- data.frame(Y = var, data =  abbr_names[data_i], intercept = inter, int_p = int_p,
                      F_Grad = F_Grad, F_p = F_p, Z_Baum = Z_Baum, Z_p = Z_p,
                      north = north,north_p = north_p, spr_temp = spr_temp, spr_p = spr_p,
                      sum_temp = sum_temp, sum_p = sum_p, DWD = DWD, DWD_p = DWD_p)
    
    mod_summary <- rbind(mod_summary, new)
    
  }
  
  
}


mod_summary

write.csv(mod_summary, 'mod_sum_sum.csv', row.names = FALSE)



################################################################################

##################################
####### Loop Summer+Autumn #######
##################################


data_names <- c('dat_pheno_SG', 'dat_pheno_DL', 'dat_pheno_VPP')
title_names <- c('Savitzy-Golay', 'DLogistic', 'WekEO-VPP')
abbr_names <- c('SG', 'DL', 'VPP')

for(data_i in 1:3){
  eval(parse(text = paste('dat_merge <- ', data_names[data_i], sep = '')))
  head(dat_merge)
  
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
    
    neww <- data.frame(key = key_n, plot = plot, parcel = par, year = yr, Treat = treat, Age = age, 
                       north = north, STEBO = STEBO, STEBV = STEBV, DWD_LOS = DWD_LOS, 
                       spr_temp = spr_temp, sum_temp = sum_temp, aut_temp = aut_temp, 
                       pix_sos = pix_sos, pix_eos = pix_eos, pix_los = pix_los, win_temp = win_temp,
                       m_sos = m_sos, m_eos = m_eos, m_los = m_los,
                       sd_sos = sd_sos, sd_eos = sd_eos, sd_los = sd_los, 
                       cv_sos = cv_sos, cv_eos = cv_eos, cv_los = cv_los)
    
    if (key_i == 1){
      dat_key <- neww
    }else{
      dat_key <- rbind(dat_key, neww)
    }
    
  }
  
  
  dat_key$log_sds <- log(dat_key$sd_sos)
  dat_key$log_sde <- log(dat_key$sd_eos)
  dat_key$log_sdl <- log(dat_key$sd_los)
  
  dat_key$log_cvs <- log(dat_key$cv_sos)
  dat_key$log_cve <- log(dat_key$cv_eos)
  dat_key$log_cvl <- log(dat_key$cv_los)
  
  dat_test <- dat_key
  
  ### model sos, eos, los
  var_names <- c('sos', 'eos', 'los')
  for (i in 1:3){
    var <- var_names[i]
    if (i == 1){
      eval(parse(text = paste('mixed.lmer <- lmer(', var, ' ~ Treat + north + spr_temp + STEBO  + (1|year), data = dat_merge)', sep = '')))
    }else if (i == 2){
      eval(parse(text = paste('mixed.lmer <- lmer(', var, ' ~ Treat + north + sum_temp + aut_temp + STEBO  + (1|year), data = dat_merge)', sep = '')))
    }else {
      eval(parse(text = paste('mixed.lmer <- lmer(', var, ' ~ Treat + north + spr_temp + sum_temp + aut_temp + STEBO  + (1|year), data = dat_merge)', sep = '')))
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
    
    if(i == 1){
      spr_temp <- summary(mixed.lmer)$coefficients[5,1]
      p.value <- summary(mixed.lmer)$coefficients[5,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      spr_p <- pvalue
      
      sum_temp <- NA
      sum_p <- NA
      
      aut_temp <- NA
      aut_p <- NA
      
      DWD <- summary(mixed.lmer)$coefficients[6,1]
      p.value <- summary(mixed.lmer)$coefficients[6,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      DWD_p <- pvalue
    }else if (i == 2){
      spr_temp <- NA
      spr_p <- NA
      
      sum_temp <- summary(mixed.lmer)$coefficients[5,1]
      p.value <- summary(mixed.lmer)$coefficients[5,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      sum_p <- pvalue
      
      aut_temp <- summary(mixed.lmer)$coefficients[6,1]
      p.value <- summary(mixed.lmer)$coefficients[6,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      aut_p <- pvalue
      
      DWD <- summary(mixed.lmer)$coefficients[7,1]
      p.value <- summary(mixed.lmer)$coefficients[7,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      DWD_p <- pvalue
    }else{
      spr_temp <- summary(mixed.lmer)$coefficients[5,1]
      p.value <- summary(mixed.lmer)$coefficients[5,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      spr_p <- pvalue
      
      sum_temp <- summary(mixed.lmer)$coefficients[6,1]
      p.value <- summary(mixed.lmer)$coefficients[6,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      sum_p <- pvalue
      
      aut_temp <- summary(mixed.lmer)$coefficients[7,1]
      p.value <- summary(mixed.lmer)$coefficients[7,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      aut_p <- pvalue
      
      DWD <- summary(mixed.lmer)$coefficients[8,1]
      p.value <- summary(mixed.lmer)$coefficients[8,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      DWD_p <- pvalue
    }
    
    
    new <- data.frame(Y = var, data =  abbr_names[data_i], intercept = inter, int_p = int_p,
                      F_Grad = F_Grad, F_p = F_p, Z_Baum = Z_Baum, Z_p = Z_p,
                      north = north,north_p = north_p, spr_temp = spr_temp, spr_p = spr_p,
                      sum_temp = sum_temp, sum_p = sum_p, DWD = DWD, DWD_p = DWD_p)
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
  
  
  # model sds, sde, sdl
  var_names <- c('sds', 'sde', 'sdl')
  for (j in 1:3){
    var <- var_names[j]
    if (j == 1){
      eval(parse(text = paste('mixed.lmer <- lmer(log_', var, ' ~ Treat + north + spr_temp + STEBO  + (1|year), data = dat_test)', sep = '')))
    }else if (j == 2){
      eval(parse(text = paste('mixed.lmer <- lmer(log_', var, ' ~ Treat + north + sum_temp + aut_temp + STEBO  + (1|year), data = dat_test)', sep = '')))
    }else {
      eval(parse(text = paste('mixed.lmer <- lmer(log_', var, ' ~ Treat + north + spr_temp + sum_temp + aut_temp + STEBO  + (1|year), data = dat_test)', sep = '')))
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
    
    if(j == 1){
      spr_temp <- summary(mixed.lmer)$coefficients[5,1]
      p.value <- summary(mixed.lmer)$coefficients[5,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      spr_p <- pvalue
      
      sum_temp <- NA
      sum_p <- NA
      
      aut_temp <- NA
      aut_p <- NA
      
      DWD <- summary(mixed.lmer)$coefficients[6,1]
      p.value <- summary(mixed.lmer)$coefficients[6,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      DWD_p <- pvalue
    }else if (j == 2){
      spr_temp <- NA
      spr_p <- NA
      
      sum_temp <- summary(mixed.lmer)$coefficients[5,1]
      p.value <- summary(mixed.lmer)$coefficients[5,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      sum_p <- pvalue
      
      aut_temp <- summary(mixed.lmer)$coefficients[6,1]
      p.value <- summary(mixed.lmer)$coefficients[6,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      aut_p <- pvalue
      
      DWD <- summary(mixed.lmer)$coefficients[7,1]
      p.value <- summary(mixed.lmer)$coefficients[7,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      DWD_p <- pvalue
    }else{
      spr_temp <- summary(mixed.lmer)$coefficients[5,1]
      p.value <- summary(mixed.lmer)$coefficients[5,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      spr_p <- pvalue
      
      sum_temp <- summary(mixed.lmer)$coefficients[6,1]
      p.value <- summary(mixed.lmer)$coefficients[6,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      sum_p <- pvalue
      
      aut_temp <- summary(mixed.lmer)$coefficients[7,1]
      p.value <- summary(mixed.lmer)$coefficients[7,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      aut_p <- pvalue
      
      DWD <- summary(mixed.lmer)$coefficients[8,1]
      p.value <- summary(mixed.lmer)$coefficients[8,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      DWD_p <- pvalue
    }
    
    
    new <- data.frame(Y = var, data =  abbr_names[data_i], intercept = inter, int_p = int_p,
                      F_Grad = F_Grad, F_p = F_p, Z_Baum = Z_Baum, Z_p = Z_p,
                      north = north,north_p = north_p, spr_temp = spr_temp, spr_p = spr_p,
                      sum_temp = sum_temp, sum_p = sum_p, DWD = DWD, DWD_p = DWD_p)
    
    mod_summary <- rbind(mod_summary, new)
    
  }
  
  
  
  # model cvs, cve, cvl
  var_names <- c('cvs', 'cve', 'cvl')
  for (k in 1:3){
    var <- var_names[k]
    if (k == 1){
      eval(parse(text = paste('mixed.lmer <- lmer(log_', var, ' ~ Treat + north + spr_temp + STEBO  + (1|year), data = dat_test)', sep = '')))
    }else if (k == 2){
      eval(parse(text = paste('mixed.lmer <- lmer(log_', var, ' ~ Treat + north + sum_temp + aut_temp + STEBO  + (1|year), data = dat_test)', sep = '')))
    }else {
      eval(parse(text = paste('mixed.lmer <- lmer(log_', var, ' ~ Treat + north + spr_temp + sum_temp + aut_temp + STEBO  + (1|year), data = dat_test)', sep = '')))
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
    
    if(k == 1){
      spr_temp <- summary(mixed.lmer)$coefficients[5,1]
      p.value <- summary(mixed.lmer)$coefficients[5,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      spr_p <- pvalue
      
      sum_temp <- NA
      sum_p <- NA
      
      aut_temp <- NA
      aut_p <- NA
      
      DWD <- summary(mixed.lmer)$coefficients[6,1]
      p.value <- summary(mixed.lmer)$coefficients[6,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      DWD_p <- pvalue
    }else if (k == 2){
      spr_temp <- NA
      spr_p <- NA
      
      sum_temp <- summary(mixed.lmer)$coefficients[5,1]
      p.value <- summary(mixed.lmer)$coefficients[5,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      sum_p <- pvalue
      
      aut_temp <- summary(mixed.lmer)$coefficients[6,1]
      p.value <- summary(mixed.lmer)$coefficients[6,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      aut_p <- pvalue
      
      DWD <- summary(mixed.lmer)$coefficients[7,1]
      p.value <- summary(mixed.lmer)$coefficients[7,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      DWD_p <- pvalue
    }else{
      spr_temp <- summary(mixed.lmer)$coefficients[5,1]
      p.value <- summary(mixed.lmer)$coefficients[5,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      spr_p <- pvalue
      
      sum_temp <- summary(mixed.lmer)$coefficients[6,1]
      p.value <- summary(mixed.lmer)$coefficients[6,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      sum_p <- pvalue
      
      aut_temp <- summary(mixed.lmer)$coefficients[7,1]
      p.value <- summary(mixed.lmer)$coefficients[7,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      aut_p <- pvalue
      
      DWD <- summary(mixed.lmer)$coefficients[8,1]
      p.value <- summary(mixed.lmer)$coefficients[8,5]
      if (p.value <0.001) {pvalue <- "***"} else if (p.value <0.05) {pvalue <- "**"} else if (p.value <0.01) {pvalue <- "*"}else(pvalue <- "ns")
      DWD_p <- pvalue
    }
    
    
    new <- data.frame(Y = var, data =  abbr_names[data_i], intercept = inter, int_p = int_p,
                      F_Grad = F_Grad, F_p = F_p, Z_Baum = Z_Baum, Z_p = Z_p,
                      north = north,north_p = north_p, spr_temp = spr_temp, spr_p = spr_p,
                      sum_temp = sum_temp, sum_p = sum_p, DWD = DWD, DWD_p = DWD_p)
    
    mod_summary <- rbind(mod_summary, new)
    
  }
  
  
}


mod_summary

write.csv(mod_summary, 'mod_sum_sum.csv', row.names = FALSE)



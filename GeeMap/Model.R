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

dat_pheno_SG <- read.csv('dat_pheno_pix_SG.csv')
dat_pheno_SG$key <- paste(dat_pheno_SG$plot, '_', dat_pheno_SG$parcel, sep = '')
dat_pheno_SG <- merge(dat_pheno_SG, dat_meta, by = 'key')
head(dat_pheno_SG)

dat_pheno_DL <- read.csv('dat_pheno_pix_DL.csv')
dat_pheno_DL$key <- paste(dat_pheno_DL$plot, '_', dat_pheno_DL$parcel, sep = '')
dat_pheno_DL <- merge(dat_pheno_DL, dat_meta, by = 'key')
library(naniar)
dat_pheno_DL <- dat_pheno_DL %>% replace_with_na_at(.vars = 'eos', condition = ~.x < 210) # remove outliers in eos, DL
head(dat_pheno_DL)

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
###################


data_names <- c('dat_pheno_SG', 'dat_pheno_DL', 'dat_wek')
title_names <- c('Savitzy-Golay', 'DLogistic', 'WekEO-VPP')
abbr_names <- c('SG', 'DL', 'VPP')
data_i <- 1
eval(parse(text = paste('dat_merge <- ', data_names[data_i], sep = '')))
head(dat_merge)


#### merge plots into two
dat_merge$new_plot <- dat_merge$plot
dat_merge$new_plot <- replace(dat_merge$new_plot, dat_merge$new_plot == 'Loh', 'Roh')
dat_merge$new_plot <- replace(dat_merge$new_plot, dat_merge$new_plot == 'Roh620', 'Roh')
dat_merge$new_plot <- replace(dat_merge$new_plot, dat_merge$new_plot == 'Roh635', 'Roh')
dat_merge$new_plot <- replace(dat_merge$new_plot, dat_merge$new_plot == 'Roh90', 'Roh')
dat_merge$new_plot


# 
# mixed.lmer <- lmer(eos ~ Treat * year + (1|plot), data = dat_merge)  # intercept
# 
# summary(mixed.lmer)
# 

#### Hausman Test
#### check fixed/random effect

library(plm)
# install.packages('plm') 
fixed <- plm(sos ~ Treat, data = dat_merge, index = c("year"), model = "within")  #fixed model
random <- plm(sos ~ Treat, data = dat_merge, index = c("year"), model = "random")  #random model
phtest(fixed,random) #Hausman test

# If the p-value is significant (for example <0.05) then use fixed effects, if not use random effects. If p value is slightly larger than 0.05, it may still be better to use fixed effects models.
summary(fixed)
summary(random)


mixed.lmer <- lmer(los ~ Treat + new_plot + (1|year), data = dat_merge)  # intercept

summary(mixed.lmer)




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

dat_key$log_sds <- log(dat_key$sd_sos)
dat_key$log_sde <- log(dat_key$sd_eos)
dat_key$log_sdl <- log(dat_key$sd_los)

dat_key$log_cvs <- log(dat_key$cv_sos)
dat_key$log_cve <- log(dat_key$cv_eos)
dat_key$log_cvl <- log(dat_key$cv_los)

#### merge plots into two
dat_key$new_plot <- dat_key$plot
dat_key$new_plot <- replace(dat_key$new_plot, dat_key$new_plot == 'Loh', 'Roh')
dat_key$new_plot <- replace(dat_key$new_plot, dat_key$new_plot == 'Roh620', 'Roh')
dat_key$new_plot <- replace(dat_key$new_plot, dat_key$new_plot == 'Roh635', 'Roh')
dat_key$new_plot <- replace(dat_key$new_plot, dat_key$new_plot == 'Roh90', 'Roh')
dat_key$new_plot


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

library(plm)
# install.packages('plm') 
fixed <- plm(log_sds ~ Treat, data = dat_test, index = c("year"), model = "within")  #fixed model
random <- plm(log_sds ~ Treat, data = dat_test, index = c("year"), model = "random")  #random model
phtest(fixed,random) #Hausman test

# If the p-value is significant (for example <0.05) then use fixed effects, if not use random effects. If p value is slightly larger than 0.05, it may still be better to use fixed effects models.
summary(fixed)
summary(random)


# mixed.lmer <- lmer(log_sds ~ Treat + year + (1|plot) , data = dat_test) # year as fixed effect
# summary(mixed.lmer)
# 
# mixed.lmer <- lmer(log_sds ~ Treat * year + (1|plot) , data = dat_test) # interaction
# summary(mixed.lmer)

mixed.lmer <- lmer(log_cvs ~ Treat + new_plot + (1|year), data = dat_test) # year as random effect
summary(mixed.lmer)

plot(mixed.lmer) 

qqnorm(resid(mixed.lmer))
qqline(resid(mixed.lmer))

# library(cAIC4)
# step <- stepcAIC(mixed.lmer, direction = "backward", trace = TRUE, data = dat_test)


(mm_plot <- ggplot(dat_test, aes(x = Treat, y = sd_sos, colour = Treat)) +
    facet_wrap(~year, nrow=3) +   # a panel for each Age
    geom_point(alpha = 0.5) +
    theme_classic() +
    geom_line(data = cbind(dat_test, pred = predict(mixed.lmer)), aes(y = pred), size = 1) +  # adding predicted line from mixed model 
    theme(legend.position = "none",
          panel.spacing = unit(2, "lines"))  # adding space between panels
)

library(ggeffects)
# Extract the prediction data frame
pred.mm <- ggpredict(mixed.lmer, terms = c("Treat"))  # this gives overall predictions for the model

# Plot the predictions 

(ggplot(pred.mm) + 
    geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                fill = "lightgrey", alpha = 0.5) +  # error band
    geom_boxplot(data = dat_test,                      # adding the raw data (scaled values)
                 aes(x = Treat, y = sd_sos, colour = Age)) + 
    labs(x = "Treatment", y = "SD of SOS", 
         title = "Treatment affects SD of SOS") + 
    theme_minimal()
)


ggpredict(mixed.lmer, terms = c("Treat", "Age"), type = "re") %>% 
  plot() +
  labs(x = "Treat", y = "SD of SOS", title = "Effect of Treatment on SOS") + 
  theme_minimal()


library(stargazer)

stargazer(mixed.lmer, type = "text",
          digits = 3,
          star.cutoffs = c(0.05, 0.01, 0.001),
          digit.separator = "")

#### SOS ####

mixed.lmer <- lmer(log(sd_sos) ~ Treat + new_plot + (1|year), data = dat_test) # year as random effect
# plot(mixed.lmer)

# Extract the prediction data frame
pred.mm <- ggpredict(mixed.lmer, terms = c("Treat"))  # this gives overall predictions for the model

# Plot the predictions 

(ggplot(pred.mm) + 
    geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                fill = "lightgrey", alpha = 0.5) +  # error band
    geom_boxplot(data = dat_test,                      # adding the raw data (scaled values)
                 aes(x = Treat, y = sd_sos, color = Treat )) + 
    labs(x = "Treatment", y = "SD of SOS", 
         title = "Treatment affects SD of SOS") + 
    theme_minimal()
)


#### EOS ####

mixed.lmer2 <- lmer(log(sd_eos) ~ Treat + new_plot + (1|year), data = dat_test) # the syntax stays the same, but now the nesting is taken into account
summary(mixed.lmer2)
# plot(mixed.lmer2) 


# Extract the prediction data frame
pred.mm <- ggpredict(mixed.lmer2, terms = c("Treat"))  # this gives overall predictions for the model

# Plot the predictions 

(ggplot(pred.mm) + 
    geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                fill = "lightgrey", alpha = 0.5) +  # error band
    geom_boxplot(data = dat_test,                      # adding the raw data (scaled values)
                 aes(x = Treat, y = sd_eos, colour = Treat)) + 
    labs(x = "Treatment", y = "SD of EOS", 
         title = "Treatment affects SD of EOS") + 
    theme_minimal()
)


#### LOS ####

mixed.lmer3 <- lmer(log(sd_los) ~ Treat + new_plot + (1|year), data = dat_test) # the syntax stays the same, but now the nesting is taken into account
summary(mixed.lmer3)
# plot(mixed.lmer3) 


# Extract the prediction data frame
pred.mm <- ggpredict(mixed.lmer3, terms = c("Treat"))  # this gives overall predictions for the model

# Plot the predictions 

(ggplot(pred.mm) + 
    geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                fill = "lightgrey", alpha = 0.5) +  # error band
    geom_boxplot(data = dat_test,                      # adding the raw data (scaled values)
                 aes(x = Treat, y = sd_los, colour = Treat)) + 
    labs(x = "Treatment", y = "SD of LOS", 
         title = "Treatment affects SD of LOS") + 
    theme_minimal()
)


library(dplyr)
library(ggpubr)
library(ggplot2)
library(lme4)

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
    
    new <- data.frame(key = key_n, plot = plot, parcel = par, year = yr, Treat = treat, Age = age, pix_sos = pix_sos, pix_eos = pix_eos, pix_los = pix_los, sd_sos = sd_sos, sd_eos = sd_eos, sd_los = sd_los)
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


dat_test <- dat_key
head(dat_test)


# # Summarize data
# sum_data <-
#   dat_merge %>%
#   group_by(plot) %>%
#   summarize(m_sos = mean(sos),
#             m_eos = mean(eos),
#             sd_sos = sd(sos),
#             sd_eso = sd(eos), .groups = 'keep')


# basic.lm <- lm(sd_sos ~Treat, dat_test)

library(lmerTest) # package to show p-value

mixed.glmer <- glmer(sd_sos ~ Treat + (1|Age) + (1|plot) + (1|year), data = dat_test, family = 'logit') # intercept


summary(mixed.glmer)
plot(mixed.glmer) 

qqnorm(resid(mixed.lmer))
qqline(resid(mixed.lmer))

library(cAIC4)
step <- stepcAIC(mixed.lmer, direction = "backward", trace = TRUE, data = dat_test)


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

mixed.lmer <- lmer(sd_sos ~Treat + Age + (1|plot) + (1|year), data = dat_test) # the syntax stays the same, but now the nesting is taken into account
# summary(mixed.lmer)
# plot(mixed.lmer)

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


#### EOS ####

mixed.lmer2 <- lmer(sd_eos ~Treat + Age + (1|plot) + (1|year), data = dat_test) # the syntax stays the same, but now the nesting is taken into account
summary(mixed.lmer2)
# plot(mixed.lmer2) 


# Extract the prediction data frame
pred.mm <- ggpredict(mixed.lmer2, terms = c("Treat"))  # this gives overall predictions for the model

# Plot the predictions 

(ggplot(pred.mm) + 
    geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                fill = "lightgrey", alpha = 0.5) +  # error band
    geom_boxplot(data = dat_test,                      # adding the raw data (scaled values)
                 aes(x = Treat, y = sd_eos, colour = Age)) + 
    labs(x = "Treatment", y = "SD of EOS", 
         title = "Treatment affects SD of EOS") + 
    theme_minimal()
)


#### LOS ####

mixed.lmer3 <- lmer(sd_los ~Treat + Age + (1|plot) + (1|year), data = dat_test) # the syntax stays the same, but now the nesting is taken into account
summary(mixed.lmer3)
# plot(mixed.lmer3) 


# Extract the prediction data frame
pred.mm <- ggpredict(mixed.lmer3, terms = c("Treat"))  # this gives overall predictions for the model

# Plot the predictions 

(ggplot(pred.mm) + 
    geom_ribbon(aes(x = x, ymin = predicted - std.error, ymax = predicted + std.error), 
                fill = "lightgrey", alpha = 0.5) +  # error band
    geom_boxplot(data = dat_test,                      # adding the raw data (scaled values)
                 aes(x = Treat, y = sd_los, colour = Age)) + 
    labs(x = "Treatment", y = "SD of LOS", 
         title = "Treatment affects SD of LOS") + 
    theme_minimal()
)

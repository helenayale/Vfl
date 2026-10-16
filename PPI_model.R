library(dplyr)
library(ggpubr)
library(ggplot2)
library(lme4)
library(lmerTest) # package to show p-value
library(naniar)
library(MuMIn)

setwd('D:/GIS_Data/Vfl-oak/WekEO/')
field_names <- c('Gei', 'Loh', 'Roh90', 'Roh620', 'Roh635')

# dat_all <- read.csv('Dat_int_pix.csv')
# head(dat_all)
# 
# dat_raw <- read.csv('Dat_all_pix_dc.csv')
# head(dat_raw)

dat_meta <- read.csv('meta_data2.csv')
dat_meta

#### Load data
load('dat_PPI.rda')
head(dat_all)

dat_all$key <- paste(dat_all$ID, '_', dat_all$Parcel, sep = '')
head(dat_all)

dat_merge <- merge(dat_all, dat_meta, by = 'key')
head(dat_merge)

dat_merge$key_yr <- paste(dat_merge$key, '_', dat_merge$year, sep = '')
head(dat_merge)

# library(dplyr)
# max_by_year <- dat_merge %>%
#   group_by(year) %>%
#   filter(PPI == max(PPI)) %>%
#   select(year, DOY, PPI) %>%
#   arrange(year) %>%
#   ungroup()
# print(max_by_year)


plt <- ggplot(
  data = dat_merge
) +
  geom_line (
    aes(x = DOY, y = PPI, group = interaction(year, ID), color = year)
  )+
  labs(
    title = "PPI",
    y = "PPI", x = "DOY"
  ) +
  theme_minimal(
    base_size = 15
  ) +
  facet_wrap(
    ~ ID
  ) +
  theme(panel.grid = element_blank())
plt

#### load DWD data
load('dat_dwd_2024.rda')
dat_dwd$key <- paste(dat_dwd$ID, '_', dat_dwd$Parcel, sep = '')
dat_dwd$key_yr <- paste(dat_dwd$key, '_', dat_dwd$year, sep = '')
head(dat_dwd)

#### var to be added
dat_dwd_m <- dat_dwd[,c('key_yr', 'spr_temp', 'sum_temp', 'aut_temp', 'win_chill')]
head(dat_dwd_m)

### load DWD monthly data
load('dat_dwd_month.rda')
dat_month$key <- paste(dat_month$ID, '_', dat_month$Parcel, sep = '')
dat_month$key_yr <- paste(dat_month$key, '_', dat_month$year, sep = '')
head(dat_month)

#### var to be added
dat_month_m <- dat_month[,c('key_yr', 'temp_03', 'temp_04', 'temp_05', 'temp_06', 'temp_09', 'temp_10',
                            'prec_03', 'prec_04', 'prec_05', 'prec_06', 'prec_09', 'prec_10',
                            'sund_03', 'sund_04', 'sund_05', 'sund_06', 'sund_09', 'sund_10')]
head(dat_month_m)

# sort(unique(dat_merge$DOY))
# annual_dates <- aggregate(DOY ~ year, data = dat_merge, 
#                           FUN = function(x) sort(unique(x)))
# print(annual_dates)

# select DOY on beginning/ending of a month
# select from DOY 1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335
# March DOY 91
# April DOY 121
# June DOY 182
# Sept DOY 274
# Oct DOY 305

DOY_list <- c(91, 121, 182, 274, 305)
name_list <- c('pre', 'sos', 'peak', 'eos', 'post')

for (yr in 2017:2024){
  dat_yr <- subset(dat_merge, year == yr, select = c('ID', 'Parcel', 'pix', 'Treat', 'year', 'DOY', 'PPI')) 
  for (i_doy in 1:length(DOY_list)){
    name <- name_list[i_doy]
    if (yr == 2020 | yr == 2024){
      d <- DOY_list[i_doy] + 1
    } else {
      d <- DOY_list[i_doy] 
    }
    dat <- subset(dat_yr, DOY == d)
    if(i_doy == 1){
      dat_doy <- data.frame(ID = dat$ID, Parcel = dat$Parcel, pix = dat$pix, year = dat$year, Treat = dat$Treat)
      eval(parse(text = paste('dat_doy$PPI_', name, '<- dat$PPI', sep = '')))
    } else {
      eval(parse(text = paste('dat_doy$PPI_', name, '<- dat$PPI', sep = '')))
    }
  }
  if (yr == 2017){
    dat_ppi <- dat_doy
  } else {
    dat_ppi <- rbind(dat_ppi, dat_doy)
  }
}

head(dat_ppi)


dat_ppi$key <- paste(dat_ppi$ID, '_', dat_ppi$Parcel, sep = '')
dat_ppi$key_yr <- paste(dat_ppi$key, '_', dat_ppi$year, sep = '')
dat_model <- merge(dat_ppi, dat_dwd_m, by = 'key_yr')
dat_model <- merge(dat_model, dat_month_m, by = 'key_yr')
head(dat_model)

save(dat_model, file = 'dat_model_raw.rda')

load('dat_model_raw.rda')

# normalize 
dat_model$spr_temp <- scale(dat_model$spr_temp)
dat_model$sum_temp <- scale(dat_model$sum_temp)
dat_model$aut_temp <- scale(dat_model$aut_temp)
dat_model$win_chill <- scale(dat_model$win_chill)

dat_model$temp_03 <- scale(dat_model$temp_03)
dat_model$temp_04 <- scale(dat_model$temp_04)
dat_model$temp_05 <- scale(dat_model$temp_05)
dat_model$temp_06 <- scale(dat_model$temp_06)
dat_model$temp_09 <- scale(dat_model$temp_09)
dat_model$temp_10 <- scale(dat_model$temp_10)

dat_model$prec_03 <- scale(dat_model$prec_03)
dat_model$prec_04 <- scale(dat_model$prec_04)
dat_model$prec_05 <- scale(dat_model$prec_05)
dat_model$prec_06 <- scale(dat_model$prec_06)
dat_model$prec_09 <- scale(dat_model$prec_09)
dat_model$prec_10 <- scale(dat_model$prec_10)

dat_model$sund_03 <- scale(dat_model$sund_03)
dat_model$sund_04 <- scale(dat_model$sund_04)
dat_model$sund_05 <- scale(dat_model$sund_05)
dat_model$sund_06 <- scale(dat_model$sund_06)
dat_model$sund_09 <- scale(dat_model$sund_09)
dat_model$sund_10 <- scale(dat_model$sund_10)

head(dat_model)

Treat_v <- dat_model$Treat
Treat_v[Treat_v== "A-Grad"] <- 'Low'
Treat_v[Treat_v == "F-Grad" ] <- 'Medium'
Treat_v[Treat_v == "Z-Baum" ] <- 'High'


dat_model$Treat_lvl <- factor(Treat_v, levels = c('Low', 'Medium', 'High'))
head(dat_model)

save(dat_model, file = 'dat_PPI_model.rda')

load('dat_PPI_model.rda')
head(dat_model)

count <- dat_model %>% 
  group_by(Treat_lvl = Treat_lvl)  %>% 
  count()
count

sum <- sum(count$n)

W_A <- sum/count$n[1]
W_F <- sum/count$n[2]
W_Z <- sum/count$n[3]

dat_model$weight[dat_model$Treat_lvl == 'Low'] <- W_A
dat_model$weight[dat_model$Treat_lvl == 'Medium'] <- W_F
dat_model$weight[dat_model$Treat_lvl == 'High'] <- W_Z

head(dat_model)

library(lme4)
library(lmerTest)

mixed.lmer <- lmer(PPI_pre ~ Treat_lvl + temp_03 + prec_03 + sund_03 + win_chill + (1|ID) + (1|year), data = dat_model)
summary(mixed.lmer)

r_sq_val <- r.squaredGLMM(mixed.lmer)
print(r_sq_val)


mixed.lmer <- lmer(PPI_sos ~ Treat_lvl + temp_04 + prec_04 + sund_04 + win_chill + (1|ID) + (1|year), data = dat_model)
summary(mixed.lmer)

r.squaredGLMM(mixed.lmer)

mixed.lmer <- lmer(PPI_peak ~ Treat_lvl + temp_06 + prec_06 + sund_06 + (1|ID) + (1|year), data = dat_model)
summary(mixed.lmer)

r.squaredGLMM(mixed.lmer)

mixed.lmer <- lmer(PPI_eos ~ Treat_lvl + temp_09 + prec_09 + sund_09 + (1|ID) + (1|year), data = dat_model)
summary(mixed.lmer)

r.squaredGLMM(mixed.lmer)

mixed.lmer <- lmer(PPI_post ~ Treat_lvl + temp_10 + prec_10 + sund_10 + (1|ID) + (1|year), data = dat_model)
summary(mixed.lmer)

r.squaredGLMM(mixed.lmer)
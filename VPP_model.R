library(dplyr)
library(ggpubr)
library(ggplot2)
library(lme4)
library(lmerTest) # package to show p-value
library(naniar)
library(MuMIn)

setwd('D:/GIS_Data/Vfl-oak/WekEO/')
load('dat_wek_2024.rda')

dat_meta <- read.csv('meta_data2.csv')

dat_all$key <- paste(dat_all$ID, '_', dat_all$Parcel, sep = '')

dat_all$los <- dat_all$eos - dat_all$sos

dat_wek <- merge(dat_all, dat_meta, by = 'key')

Treat_v <- dat_wek$Treat
Treat_v[Treat_v== "A-Grad"] <- 'Low'
Treat_v[Treat_v == "F-Grad" ] <- 'Medium'
Treat_v[Treat_v == "Z-Baum" ] <- 'High'


dat_wek$Treat_lvl <- factor(Treat_v, levels = c('Low', 'Medium', 'High'))
dat_wek

sha.test <- shapiro.test(dat_wek[dat_wek$ID %in% c('Gei'),]$sos)
if (sha.test$p.value > 0.05) {norm <- "normal distribution"} else{norm <- "non-normal distribution"}
norm
sha.test$p.value


key_list <- unique(dat_wek$key)

for (key_i in 1:length(key_list)){
  key_n <- key_list[key_i]
  sub_key <- subset(dat_wek, key == key_n)
  plot <- unique(sub_key$ID)
  par <- unique(sub_key$Parcel)
  treat <- unique(sub_key$Treat_lvl)
  for (yr in 2017:2024){
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
    
    
    new <- data.frame(key = key_n, plot = plot, parcel = par, year = yr, Treat = treat, 
                      n_sos = pix_sos, n_eos = pix_eos, n_los = pix_los,
                      sd_sos = sd_sos, sd_eos = sd_eos, sd_los = sd_los,
                      m_sos = m_sos, m_eos = m_eos, m_los = m_los)
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

dat_key$cv_sos <- dat_key$sd_sos/dat_key$m_sos
dat_key$cv_eos <- dat_key$sd_eos/dat_key$m_eos
dat_key$cv_los <- dat_key$sd_los/dat_key$m_los
dat_key

dat_key$log_sds <- log(dat_key$sd_sos)
dat_key$log_sde <- log(dat_key$sd_eos)
dat_key$log_sdl <- log(dat_key$sd_los)

dat_key$log_cvs <- log(dat_key$cv_sos)
dat_key$log_cve <- log(dat_key$cv_eos)
dat_key$log_cvl <- log(dat_key$cv_los)


load('dat_dwd_2024.rda')
dat_dwd$key <- paste(dat_dwd$ID, '_', dat_dwd$Parcel, sep = '')
dat_dwd

dat_dwd <- dat_dwd[, c('key', 'year', 'spr_temp', 'sum_temp', 'aut_temp', 'win_chill', 'spr_prec', 'aut_prec', 'spr_sund', 'aut_sund', 'stebo', 'stebv', 'spr_DI')]
dat_dwd

dat_dwd$key_yr <- paste(dat_dwd$key, '_', dat_dwd$year, sep = '')
dat_wek$key_yr <- paste(dat_wek$key, '_', dat_wek$year, sep = '')
dat_key$key_yr <- paste(dat_key$key, '_', dat_key$year, sep = '')

dat_dwd_add <- dat_dwd[, c('key_yr', 'spr_temp', 'sum_temp', 'aut_temp', 'win_chill', 'spr_prec', 'aut_prec', 'spr_sund', 'aut_sund', 'stebo', 'stebv', 'spr_DI')]
dat_dwd_add

dat_wek <- merge(dat_wek, dat_dwd_add, by = 'key_yr')
dat_wek

# normalize 
dat_wek$spr_temp <- scale(dat_wek$spr_temp)
dat_wek$sum_temp <- scale(dat_wek$sum_temp)
dat_wek$aut_temp <- scale(dat_wek$aut_temp)
dat_wek$win_chill <- scale(dat_wek$win_chill)
dat_wek$spr_prec <- scale(dat_wek$spr_prec)
dat_wek$aut_prec <- scale(dat_wek$aut_prec)
dat_wek$spr_sund <- scale(dat_wek$spr_sund)
dat_wek$aut_sund <- scale(dat_wek$aut_sund)

head(dat_wek)

dat_key <- merge(dat_key, dat_dwd_add, by = 'key_yr')
dat_key

# normalize 
dat_key$spr_temp <- scale(dat_key$spr_temp)
dat_key$sum_temp <- scale(dat_key$sum_temp)
dat_key$aut_temp <- scale(dat_key$aut_temp)
dat_key$win_chill <- scale(dat_key$win_chill)
dat_key$spr_prec <- scale(dat_key$spr_prec)
dat_key$aut_prec <- scale(dat_key$aut_prec)
dat_key$spr_sund <- scale(dat_key$spr_sund)
dat_key$aut_sund <- scale(dat_key$aut_sund)

head(dat_key)


### weight

head(dat_wek)

save(dat_wek, file = 'dat_VPP_model.rda')
save(dat_key, file = 'dat_VPP_SD_model.rda')

#save(dat_wek, file = 'dat_VPP_model_win_1.rda')
#save(dat_key, file = 'dat_VPP_SD_model_win_1.rda')

############ Modeling #############

load('dat_VPP_model.rda')
load('dat_VPP_SD_model.rda')

#load('dat_VPP_model_win_1.rda')
#load('dat_VPP_SD_model_win_1.rda')

head(dat_wek)

### data prep

colnames(dat_wek) <- c('key_yr', 'key', 'plot', 'Field', 'Parcel', 'Interne', 'year',
                       'pix', 'sos', 'eos', 'los', 'Treat_orig', 'Age', 'treat', 
                       'spr_temp', 'sum_temp', 'aut_temp', 'win_chill', 
                       'spr_prec', 'aut_prec', 'spr_sund', 'aut_sund',
                       'stebo', 'stebv', 'spr_DI')

count <- dat_wek  %>% 
  group_by(treat = treat)  %>% 
  count()
count

sum <- sum(count$n)

W_A <- sum/count$n[1]
W_F <- sum/count$n[2]
W_Z <- sum/count$n[3]

dat_wek$weight[dat_wek$treat == 'Low'] <- W_A
dat_wek$weight[dat_wek$treat == 'Medium'] <- W_F
dat_wek$weight[dat_wek$treat == 'High'] <- W_Z

head(dat_wek)


# tr <- dat_wek$plot
# tr[tr == 'Gei'] <- 'Pure'
# tr[tr == 'Loh'] <- 'Mix'
# tr[tr == 'Roh620'] <- 'Mix'
# tr[tr == 'Roh635'] <- 'Mix'
# tr[tr == 'Roh90'] <- 'Mix'
# 
# dat_wek$comp <- factor(tr, levels = c('Pure', 'Mix'))
# head(dat_wek)


library(lme4)
library(lmerTest)

### SOS

mixed.lmer <- lmer(sos ~ treat + spr_temp + spr_prec + spr_sund + win_chill + (1|year) + (1|plot), data = dat_wek)
summary(mixed.lmer)

library(MuMIn)
r_sq_val <- r.squaredGLMM(mixed.lmer)
print(r_sq_val)

### EOS

mixed.lmer <- lmer(eos ~ treat + aut_temp + aut_prec + aut_sund  + (1|year)  + (1|plot), data = dat_wek)  # intercept
summary(mixed.lmer)

r_sq_val <- r.squaredGLMM(mixed.lmer)
print(r_sq_val)


### LOS

mixed.lmer <- lmer(los ~ treat + spr_temp + spr_prec + spr_sund + win_chill  + aut_temp + aut_prec + aut_sund + (1|year)  + (1|plot), data = dat_wek)  # intercept
summary(mixed.lmer)

r_sq_val <- r.squaredGLMM(mixed.lmer)
print(r_sq_val)


### SDS

head(dat_key)

dat_sds <- dat_key[, c('log_sds', 'plot', 'year', 'Treat', 'spr_temp', 'win_chill', 'spr_prec', 'spr_sund')]
colnames(dat_sds) <- c('sds', 'plot', 'year', 'treat', 'spr_temp', 'win_chill', 'spr_prec', 'spr_sund')


mixed.lmer <- lmer(sds ~ treat + spr_temp + spr_prec + spr_sund + win_chill + (1|year) + (1|plot), data = dat_sds)  # intercept
summary(mixed.lmer)

r_sq_val <- r.squaredGLMM(mixed.lmer)
print(r_sq_val)

### SDE

dat_sde <- dat_key[, c('log_sde', 'plot', 'year', 'Treat', 'aut_temp', 'aut_prec', 'aut_sund', 'sum_temp')]
colnames(dat_sde) <- c('sde', 'plot', 'year', 'treat', 'aut_temp', 'aut_prec', 'aut_sund', 'sum_temp')
dat_sde

mixed.lmer <- lmer(sde ~ treat + aut_temp + aut_prec + aut_sund + sum_temp + (1|year) + (1|plot), data = dat_sde)  # intercept
summary(mixed.lmer)

r_sq_val <- r.squaredGLMM(mixed.lmer)
print(r_sq_val)



### SDL

dat_sdl <- dat_key[, c('log_sdl', 'plot', 'year', 'Treat',  'spr_temp', 'spr_prec', 'spr_sund', 'win_chill', 'aut_temp', 'aut_prec', 'aut_sund', 'sum_temp')]
colnames(dat_sdl) <- c('sdl', 'plot', 'year', 'treat',  'spr_temp', 'spr_prec', 'spr_sund', 'win_chill', 'aut_temp', 'aut_prec', 'aut_sund', 'sum_temp')
dat_sdl

mixed.lmer <- lmer(sdl ~ treat + spr_temp + spr_prec + spr_sund + win_chill + aut_temp + aut_prec + aut_sund + sum_temp + (1|year) + (1|plot), data = dat_sdl)  # intercept
summary(mixed.lmer)

r_sq_val <- r.squaredGLMM(mixed.lmer)
print(r_sq_val)





#### plot-wise SOS & drought index

head(dat_wek)

dat_DI <- dat_wek %>%
  group_by(year, plot) %>%
  summarise(
    DI = mean(spr_DI, na.rm = TRUE),
    Prec = mean(spr_prec, na.rm = TRUE),
    Temp = mean(spr_temp, na.rm = TRUE)
  )
dat_DI

plt <- ggplot(
  data = dat_wek[dat_wek$plot %in% c('Gei', 'Roh90', 'Roh620'),]
) +
  geom_boxplot(
    aes(x = year, y = sos, group = interaction(year, plot, treat), color = treat)
  ) +
  geom_line(
    data = dat_DI[dat_DI$plot %in% c('Gei', 'Roh90', 'Roh620'),], aes(x = year, y = DI * 3.28 + 80, group = plot)
  ) +
  scale_x_continuous(
    breaks = c(2017:2024)
  ) +
  scale_y_continuous(
    name = "DOY",
    sec.axis = sec_axis(~./3.28 - 80/3.28 , name = "mm/°C")
  ) +
  scale_color_manual(
    values = c("#00BFC4", "#E7B800", "#F8786D")
  ) +
  labs(
    title = "Interannual Comparison SOS",
    y = "DOY", x = "Year"
  ) +
  theme_minimal(
    base_size = 15
  ) +
  facet_wrap(
    ~plot
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(panel.grid = element_blank())
plt

range(dat_wek$sos)
range(dat_DI$Prec * 20 + 120)

plt <- ggplot(
  data = dat_wek[dat_wek$ID %in% c('Gei', 'Roh90', 'Roh620'),]
) +
  geom_boxplot(
    aes(x = year, y = sos, group = interaction(year, ID, Treat_lvl), color = Treat_lvl)
  ) +
  geom_line(
    data = dat_DI[dat_DI$ID %in% c('Gei', 'Roh90', 'Roh620'),], aes(x = year, y = Prec * 20 + 90, group = ID)
  ) +
  scale_x_continuous(
    breaks = c(2017:2024)
  ) +
  scale_y_continuous(
    name = "DOY",
    sec.axis = sec_axis(~./ 20 - 90/20 , name = "mm")
  ) +
  scale_color_manual(
    values = c("#00BFC4", "#E7B800", "#F8786D")
  ) +
  labs(
    title = "Interannual Comparison SOS",
    y = "DOY", x = "Year"
  ) +
  theme_minimal(
    base_size = 15
  ) +
  facet_wrap(
    ~ID
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(panel.grid = element_blank())
plt

mean(dat_wek$sos)
mean(dat_wek$eos)


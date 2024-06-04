library(ggplot2)

setwd('D:/GIS_Data/Vfl-oak/GEEMap/')
field_names <- c('Gei', 'Loh', 'Roh90', 'Roh620', 'Roh635')

dat_pheno <- read.csv('WekEO.csv')
head(dat_pheno)


dat_pheno$key <- paste(dat_pheno$ID, '_', dat_pheno$Parcel, sep = '')
head(dat_pheno)

dat_meta <- read.csv('meta_data.csv')
dat_meta

dat_merge <- merge(dat_pheno, dat_meta, by = 'key')
dat_merge
head(dat_merge)


plt <- ggplot(
  data = dat_merge
) +
  geom_boxplot(
    aes(x = year, y = SOSD, group = interaction(year, Treat), color = Treat)
  )+ 
  geom_boxplot(
    aes(x = year, y = EOSD, group = interaction(year, Treat), color = Treat)
  )+ 
  # geom_hline(
  #   aes(yintercept = 200),
  #   color = "grey",
  #   linetype = "solid",
  #   linewidth = 1
  # )+
  geom_vline(
    aes(xintercept = 2017.45),
    color = "grey",
    linetype = "dotted",
    linewidth = .8
  ) +
  geom_vline(
    aes(xintercept = 2018.45),
    color = "grey",
    linetype = "dotted",
    linewidth = .8
  ) +
  geom_vline(
    aes(xintercept = 2019.45),
    color = "grey",
    linetype = "dotted",
    linewidth = .8
  ) +
  geom_vline(
    aes(xintercept = 2020.45),
    color = "grey",
    linetype = "dotted",
    linewidth = .8
  ) +
  geom_vline(
    aes(xintercept = 2021.45),
    color = "grey",
    linetype = "dotted",
    linewidth = .8
  ) +
  labs(
    title = paste("Boxplot of SOS EOS (Copernicus VPP Product)", sep = " "),
    y = "DOY", x = "Year"
  ) +
  annotate(
    "text", x = 2022.8, y = c(115, 280), label = c("SOS", "EOS")
  )+
  scale_x_continuous(
    breaks = c(2017:2022)
  ) +
  # facet_wrap(
  #   ~  field
  # )+
  theme_classic()
plt


##### Standard Deviation #####

key_list <- unique(dat_merge$key)

dat_merge$LOS <- dat_merge$EOSD - dat_merge$SOSD

for (key_i in 1:length(key_list)){
  key_n <- key_list[key_i]
  sub_key <- subset(dat_merge, key == key_n)
  plot <- unique(sub_key$ID)
  par <- unique(sub_key$Parcel)
  treat <- unique(sub_key$Treat)
  for (yr in 2017:2022){
    sub_yr <- subset(sub_key, year == yr)
    # # count number of non-NA pixels
    # pix_sos <- sum(!is.na(sub_yr$SOSD))
    # pix_eos <- sum(!is.na(sub_yr$EOSD))
    # pix_los <- sum(!is.na(sub_yr$LOS))
    # pix_max <- sum(!is.na(sub_yr$MAXV))
    
    # calculate sd of each index
    sd_sos <- sd(sub_yr$SOSD)
    sd_eos <- sd(sub_yr$EOSD)
    sd_los <- sd(sub_yr$LOS)
    sd_max <- sd(sub_yr$MAXV)
    
    # calculate mean of each index
    m_sos <- mean(sub_yr$SOSD)
    m_eos <- mean(sub_yr$EOSD)
    m_los <- mean(sub_yr$LOS)
    m_max <- mean(sub_yr$MAXV)


    new <- data.frame(key = key_n, plot = plot, parcel = par, year = yr, Treat = treat, 
                      sd_sos = sd_sos, sd_eos = sd_eos, sd_los = sd_los, sd_max = sd_max, 
                      m_sos = m_sos, m_eos = m_eos, m_los = m_los, m_max = m_max)
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

sd_list <- c('sos', 'eos', 'los')

for (sd_i in 1:length(sd_list)){
  sd_n <- sd_list[sd_i]
  eval(parse(text = paste('p <- ggplot(data = dat_key, mapping = aes(x = Treat, y  = sd_', sd_n, ', group = Treat, color = Treat)) + geom_boxplot()+
                          labs(title = paste("boxplot of sd ", sd_n, sep = ""),  y = sd_n, x = "DOY") + theme_classic()', sep = '')))
  eval(parse(text = paste('ggsave("box_sd_',sd_n, '_VPP.png", p)', sep = '')))
}

ggplot(data = dat_key, mapping = aes(x = Treat, y  = sd_sos, group = Treat, color = Treat)) + geom_boxplot()+
  labs(title = 'boxplot of sd sos',  y = "SOS", x = "DOY") + theme_classic()

ggplot(data = dat_key, mapping = aes(x = Treat, y  = sd_los, group = Treat, color = Treat)) + geom_boxplot()+
  labs(title = 'boxplot of sd eos',  y = "LOS", x = "DOY") + theme_classic()


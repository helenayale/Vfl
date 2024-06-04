library(ggplot2)

setwd('D:/GIS_Data/Vfl-oak/GEEMap/')
field_names <- c('Gei', 'Loh', 'Roh90', 'Roh620', 'Roh635')

dat_pheno <- read.csv('dat_pheno_pix_SG.csv')
head(dat_pheno)

dat_meta <- read.csv('meta_data.csv')
dat_meta

dat_pheno$key <- paste(dat_pheno$plot, '_', dat_pheno$parcel, sep = '')
head(dat_pheno)

dat_merge <- merge(dat_pheno, dat_meta, by = 'key')
dat_merge
head(dat_merge)

dat_merge$field <- substr(dat_merge$plot,1,3)
head(dat_merge)

# pheno_list <- c('sos', 'eos', 'los')
# 
# for (pheno_i in 1:length(pheno_list)){
#   pheno <- pheno_list[pheno_i]
#   eval(parse(text = paste('p <- ggplot(data = dat_merge, mapping = aes(x = Treat, y = ', pheno, ', group = Treat, color = Treat)) + geom_boxplot() +  
#                           labs(title = paste("boxplot of ", pheno, sep = ""),  y = pheno, x = "DOY") + theme_classic()', sep = '')))
#   ggsave(paste('box_', pheno, '_SG', '.png', sep = ''), p)
# }



# plt <- ggplot(
#   data = dat_merge
# ) +
#   geom_boxplot(
#     aes(x = Treat, y = sos, group = Treat, color = Treat)
#   )+ 
#   geom_boxplot(
#     aes(x = Treat, y = eos, group = Treat, color = Treat)
#   )+ 
#   geom_hline(
#     aes(yintercept = 200),
#     color = "grey",
#     linetype = "dotted",
#     linewidth = .8
#   )+
#   labs(
#     title = paste("Boxplot of SOS EOS (Savitzy-Golay)", sep = " "),
#     y = "DOY", x = "Year"
#   ) +
#   # annotate(
#   #   "text", x = 1, y = c(190, 210), label = c("SOS", "EOS")
#   # )+
#   facet_wrap(
#     ~  field
#   )+
#   theme_classic()
# plt
# ggplot(data = dat_merge, mapping = aes(x = Treat, y = eos, group = Treat, color = Treat)) + geom_boxplot()
# ggplot(data = dat_merge, mapping = aes(x = Treat, y = los, group = Treat, color = Treat)) + geom_boxplot()
# ggplot(data = dat_merge, mapping = aes(x = Treat, y = sos, group = Treat, color = Treat)) + geom_boxplot()  + facet_wrap(~ plot)

plt <- ggplot(
  data = dat_merge
) +
  geom_boxplot(
    aes(x = year, y = sos, group = interaction(year, Treat), color = Treat)
  )+ 
  geom_boxplot(
    aes(x = year, y = eos, group = interaction(year, Treat), color = Treat)
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
  title = paste("Boxplot of SOS EOS (Savitzy-Golay)", sep = " "),
  y = "DOY", x = "Year"
) +
  annotate(
    "text", x = 2022.8, y = c(120, 280), label = c("SOS", "EOS")
  )+
  scale_x_continuous(
    breaks = c(2017:2022)
  ) +
  # facet_wrap(
  #   ~  field
  # )+
  theme_classic()
plt

# 
# library(naniar)
# dat_merge <- dat_merge %>% replace_with_na_at(.vars = 'eos', condition = ~.x < 210)
# dat_merge <- dat_merge %>% replace_with_na_at(.vars = 'los', condition = ~.x < 90)
# # min(dat_merge$eos, na.rm = TRUE)

# for (pheno_i in 1:length(pheno_list)){
#   pheno <- pheno_list[pheno_i]
#   eval(parse(text = paste('p <- ggplot(data = dat_merge, mapping = aes(x = Treat, y = ', pheno, ', group = Treat, color = Treat)) + geom_boxplot() +
#                           labs(title = paste("boxplot of ", pheno, sep = ""),  y = pheno, x = "DOY") + theme_classic()', sep = '')))
#   ggsave(paste('box_', pheno, '_out', '.png', sep = ''), p)
# }
# 



key_list <- unique(dat_merge$key)

for (key_i in 1:length(key_list)){
  key_n <- key_list[key_i]
  sub_key <- subset(dat_merge, key == key_n)
  plot <- unique(sub_key$plot)
  par <- unique(sub_key$parcel)
  treat <- unique(sub_key$Treat)
  for (yr in 2017:2022){
    sub_yr <- subset(sub_key, year == yr)
    # count number of non-NA pixels
    pix_sos <- sum(!is.na(sub_yr$sos))
    pix_eos <- sum(!is.na(sub_yr$eos))
    pix_los <- sum(!is.na(sub_yr$los))
    pix_gu <- sum(!is.na(sub_yr$greenup))
    pix_max <- sum(!is.na(sub_yr$max))
    
    # calculate sd of each index
    sd_sos <- sd(sub_yr$sos)
    sd_eos <- sd(sub_yr$eos)
    sd_los <- sd(sub_yr$los)
    sd_gu <- sd(sub_yr$greenup)
    sd_max <- sd(sub_yr$max)
    
    new <- data.frame(key = key_n, plot = plot, parcel = par, year = yr, Treat = treat, pix_sos = pix_sos, pix_eos = pix_eos, pix_los = pix_los, pix_gu = pix_gu, pix_max = pix_max, sd_sos = sd_sos, sd_eos = sd_eos, sd_los = sd_los, sd_gu = sd_gu, sd_max = sd_max)
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
dat_key[dat_key$pix_eos > 10, ]

sd_list <- c('sos', 'eos', 'los')

for (sd_i in 1:length(sd_list)){
  sd_n <- sd_list[sd_i]
  eval(parse(text = paste('dat_pix <- dat_key[dat_key$pix_', sd_n, '>10,]', sep = '')))
  eval(parse(text = paste('p <- ggplot(data = dat_pix, mapping = aes(x = Treat, y  = sd_', sd_n, ', group = Treat, color = Treat)) + geom_boxplot()+  
                          labs(title = paste("boxplot of sd ", sd_n, sep = ""),  y = sd_n, x = "DOY") + theme_classic()', sep = '')))
  eval(parse(text = paste('ggsave("box_sd_',sd_n, '_SG.png", p)', sep = '')))
}




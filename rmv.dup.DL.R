setwd('D:/GIS_Data/Vfl-oak/GEEMap/')

dat_pheno_DL <- read.csv('dat_pheno_pix_DL_orig.csv')
head(dat_pheno_DL)

dat_pheno_DL <- dat_pheno_DL[!duplicated(dat_pheno_DL), ]

write.csv(dat_pheno_DL, 'dat_pheno_pix_DL.csv', row.names = FALSE)

# install.packages("readxl")
library(readxl)

my_dat <- read_excel("meta_data.xls")
my_dat

write.csv(my_dat, 'meta_data.csv', row.names = FALSE)

dat_meta <- read.csv('meta_data.csv')
dat_meta


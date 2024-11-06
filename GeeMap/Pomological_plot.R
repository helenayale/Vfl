## install.packages(devtools)
# # TV shows
# devtools::install_github("Ryo-N7/tvthemes")
# # Pomological
# devtools::install_github("gadenbuie/ggpomological")

library(ggplot2)
library(ggpomological)
library(dplyr)

# Base plot
plt <- ggplot(
  data = sub_dat
) +
  geom_line(
    aes(x = DOY, y = ndvi),
    color = 'lightblue',
    size = 1.5
  ) + 
  geom_point(
    aes(x = DOY, y = orig), shape = 2, color = "#919c4c", size = 3
  ) +
  geom_vline(
    aes(xintercept = sos),
    color = "grey",
    linetype = "dotted",
    size = 1
  ) +
  geom_vline(
    aes(xintercept = eos),
    color = "grey",
    linetype = "dotted",
    size = 1
  ) +
  geom_hline(
    aes(yintercept = max),
    color = "#fd8f24",
    linetype = "dashed",
    size = .8
  ) +
  labs(
    title = paste("NDVI curve", yr, ',', plot_n, ' Par', par_i, 'Pix', pix_i, ', DLogistic', sep = " "),
    y = "NDVI", x = "DOY"
  ) +
  theme_pomological_plain( 
    base_size = 20
  )

plt



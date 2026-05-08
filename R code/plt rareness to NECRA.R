library(ggplot2)
library(latex2exp)
library(extrafont)

loadfonts()
fonts()
par(family = "LM Roman 10")
all_results <- read.csv(file = "all_results.csv")

box_width <- 0.2
# box_width_position <- box_width*c(-1.5,-0.5,0.5,1.5)
box_width_position <- box_width * c(-0.5, 0.5)
colors <- ggsci::pal_d3()(2)

tbl <- read.csv(file = "rareness vs NECRA.csv")
NECRAs <- t(sapply(unique(tbl$rareness), function(m) {
  colMeans(tbl[tbl$rareness == m, 1:2])
}))
NECRAs <- as.data.frame(NECRAs)
NECRAs$rareness <- unique(tbl$rareness)
NECRAs$rareness <- as.factor(NECRAs$rareness)
NECRAs_reshaped <- reshape2::melt(NECRAs)
colnames(NECRAs_reshaped) <- c("rareness", "method", "NECRA")

plt_rareness <- ggplot() +
  geom_line(mapping = aes(
    x = NECRAs_reshaped$rareness,
    y = NECRAs_reshaped$NECRA,
    group = NECRAs_reshaped$method,
    color = NECRAs_reshaped$method
  )) +
  geom_point(mapping = aes(
    x = NECRAs_reshaped$rareness,
    y = NECRAs_reshaped$NECRA,
    fill = NECRAs_reshaped$method
  ), shape = 21, size = 3, stroke = 0.25) +
  ggsci::scale_color_d3(
    name = "Method",
    labels = c(TeX("CL"), TeX("RW"))
  ) +
  ggsci::scale_fill_d3(
    name = "Method",
    labels = c(TeX("CL"), TeX("RW"))
  ) +
  scale_y_continuous(name = "NECRA", labels = scales::scientific, limits = c(0, NA)) +
  scale_x_discrete(name = expression(italic("RR"))) +
  theme_bw() +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.85, 0.80),
    legend.background = element_rect(colour = "black", linewidth = 0.25)
  )
plt_rareness

ggsave(
  filename = "plt rareness.png",
  plot = plt_rareness,
  device = "png",
  width = 4,
  height = 3,
  dpi = 300
)

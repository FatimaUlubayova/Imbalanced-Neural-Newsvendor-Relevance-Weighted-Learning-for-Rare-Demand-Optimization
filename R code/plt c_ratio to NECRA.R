library(ggplot2)
library(latex2exp)
library(extrafont)
library(cowplot)
library(grid)
library(gridExtra)
library(ggsignif)

loadfonts()
fonts()
par(family = "LM Roman 10")
all_results <- read.csv(file = "all_results.csv")

box_width <- 0.2
# box_width_position <- box_width*c(-1.5,-0.5,0.5,1.5)
box_width_position <- box_width * c(-0.5, 0.5)
colors <- ggsci::pal_d3()(2)

### c_ratio general line plot ####
plt_c_ratio <- ggplot() +
  stat_summary(
    mapping = aes(
      x = all_results$c_ratio*10,
      y = all_results$NECRA,
      z = all_results$method,
      color = all_results$method,
    ),
    shape = 21,
    geom = "line",
    stroke = 0.25
  ) +
  stat_summary(
    mapping = aes(
      x = as.factor(all_results$c_ratio),
      y = all_results$NECRA,
      z = all_results$method,
      fill = all_results$method
    ),
    shape = 21,
    stroke = 0.25, show.legend = FALSE
  ) +
  ggsci::scale_fill_d3(name = "Method",
                       labels = c(TeX("CL"), TeX("RW"))) +
  ggsci::scale_color_d3(name = "Method",
                        labels = c(TeX("CL"), TeX("RW"))) +
  scale_y_continuous(
    name = "NECRA",
    limits = c(0, NA),
    oob = scales::oob_squish_infinite
  ) +
  scale_x_discrete(
    name = TeX("\\it{$c_r$}"),
    guide = guide_axis(angle = 0),
    breaks = unique(all_results$c_ratio)
  ) +
  theme_bw() +
  theme(
    text = element_text(size = 12),
    legend.position = "inside",
    legend.position.inside = c(0.85, 0.2),
    legend.background = element_rect(colour = "black", linewidth = 0.25)
  )

plt_c_ratio

ggsave(
  filename = "plt c_ratio general line.png",
  plot = plt_c_ratio,
  device = "png",
  width = 4,
  height = 3,
  dpi = 300
)
########################################

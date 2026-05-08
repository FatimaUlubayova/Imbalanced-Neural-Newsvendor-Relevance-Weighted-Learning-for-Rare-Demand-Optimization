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


### p to NECRA n e gore ####
ns <- all_results$n
ns <- arules::discretize(ns, breaks = 3)

attr(ns, "discretized:break")
# levels(ns) <- c("[250,750)", "[750,1250)", "[1250,1750)", "[1750,2000]")
levels(ns) <- c("[250,750)", "[750,1500)", "[1500,2000]")
table(ns)

ps <- all_results$p_imp
ps <- arules::discretize(ps, breaks = 3)

attr(ps, "discretized:break")

ps
ns

ps_ns <- paste0("$\\it{p}$:$", ps,"$-$", "\\it{n}$:$\\", ns, "$")

ps_ns <- factor(ps_ns, levels = unique(ps_ns)[c(1,4,7,2,5,8,3,6,9)])
levels(ps_ns) <- sub(pattern = "\\[", replacement = "\\\\[", x = levels(ps_ns))
levels(ps_ns) <- sub(pattern = "\\]", replacement = "\\\\]", x = levels(ps_ns))
levels(ps_ns) <- sub(pattern = "0]", replacement = "\\\\]", x = levels(ps_ns))


p_values <- sapply(levels(ps_ns), function(m) {
  wilcox.test(x = all_results$NECRA[ps_ns == m &
                                      all_results$method == "classical"],
              y = all_results$NECRA[ps_ns == m &
                                      all_results$method == "phiWeighted"])$p.value
})

TeX(levels(ps_ns))

plt_n <- ggplot() +
  geom_errorbar(
    mapping = aes(
      x = all_results$NECRA,
      y = ps_ns,
      z = ns,
      fill = all_results$method
    ),
    stat = "boxplot",
    outliers = FALSE,
    width = 0.35,
    position = position_dodge(width = 0.75),
    linewidth = 0.25
  ) +
  geom_boxplot(
    mapping = aes(
      x = all_results$NECRA,
      y = ps_ns,
      z = ns,
      fill = all_results$method
    ),
    outliers = FALSE,
    width = 0.75,
    linewidth = 0.25,
    alpha = 1
  ) +
  ggsci::scale_fill_d3(name = "Method",
                       labels = c(TeX("CL"), TeX("RW"))) +
  scale_x_continuous(
    name = "NECRA",
    limits = c(0, NA),
    oob = scales::oob_squish_infinite
  ) +
  scale_y_discrete(name = "", guide = guide_axis(angle = 0),
                   # labels = TeX(levels(ps_ns))) +
                   labels = NULL) +
  geom_line(mapping = aes(
    y = 1:(2*length(unique(ps_ns)))*0.5 + 0.25, x = 87000,
    group = factor(rep(letters[1:length(unique(ps_ns))], each = 2))
  ), linewidth = 0.25) +
  geom_line(mapping = aes(
    y = rep(1:(2*length(unique(ps_ns)))*0.5 + 0.25, each = 2),
    x = rep(c(86000, 87000), times = length(unique(ps_ns))*2),
    group = rep(letters[1:(length(unique(ps_ns))*2)], each = 2)
  ), linewidth = 0.25) +
  geom_text(mapping = aes(
    y = 1:(length(unique(ps_ns))), x = 90000
  ), label = "***", angle = 90, size = 4) +

  ##
  geom_tile(
    mapping = aes(x = rep(c(-17500, -35000), times = length(levels(ps_ns))),
                  y = rep(seq(length(levels(ps_ns)
                  )), each = 2)),
    fill = "blue",
    alpha = 0,
    color = "black"
  ) +
  geom_text(
    mapping = aes(x = rep(c(-17500, -35000), each = length(levels(ps_ns))),
                  y = rep(seq(length(levels(ps_ns)
                  )), times = 2),
                  label = c(
                    rep(levels(ns), times = length(levels(ps))),
                    rep(levels(ps), each = length(levels(ns)))
                  )
    ),
    color = "black", size = 2.5
  ) +
  geom_text(
    mapping = aes(x = c(-17500, -35000),
                  y = length(levels(ps_ns)) + 0.8
    ),
    label = c(
      TeX("\\it{$n$}"),
      TeX("\\it{$p$}")
    ),
    color = "black", size = 3
  ) +
  coord_cartesian(xlim = c(0, 94000), ylim = c(1,length(levels(ps_ns))), clip = "off") +
  ##

  theme_bw() +
  labs(title = TeX("***\\it{$p_W$}<0.001")) +
  theme(text = element_text(size = 12), plot.caption = element_text(hjust = 0),
        plot.title = element_text(size = 7))
plt_n


ggsave(
  filename = paste0("plt p to NECRA n.png"),
  plot = plt_n,
  device = "png",
  width = 8,
  height = 8*0.5,
  dpi = 300
)
###########################



### p to NECRA sd_err e gore ####
sd_errs <- all_results$sd_err
# ns <- arules::discretize(ns, breaks = 4)

# attr(ns, "discretized:break")
# # levels(ns) <- c("[250,750)", "[750,1250)", "[1250,1750)", "[1750,2000]")
# levels(ns) <- c("[250,750)", "[750,1500)", "[1500,2000]")
# table(ns)

ps <- all_results$p_imp
ps <- arules::discretize(ps, breaks = 3)

attr(ps, "discretized:break")

ps
sd_errs <- as.factor(sd_errs)

ps_sd_errs <- paste0("$\\it{p}$:$", ps,"$-$", "\\sigma_{\\epsilon}$:", sd_errs)
ps_sd_errs <- factor(ps_sd_errs, levels = unique(ps_sd_errs)[c(1,4,7,10,2,5,8,11,3,6,9,12)])

p_values <- sapply(levels(ps_sd_errs), function(m) {
  wilcox.test(x = all_results$NECRA[ps_sd_errs == m &
                                      all_results$method == "classical"],
              y = all_results$NECRA[ps_sd_errs == m &
                                      all_results$method == "phiWeighted"],
              paired = TRUE)$p.value
})

levels(ps_sd_errs) <- sub(pattern = "\\[", replacement = "\\\\[", x = levels(ps_sd_errs))
levels(ps_sd_errs) <- sub(pattern = "\\]", replacement = "\\\\]", x = levels(ps_sd_errs))
TeX(levels(ps_sd_errs))

plt_sd_err <- ggplot() +
  geom_errorbar(
    mapping = aes(
      x = all_results$NECRA,
      y = ps_sd_errs,
      z = sd_errs,
      fill = all_results$method
    ),
    stat = "boxplot",
    outliers = FALSE,
    width = 0.35,
    position = position_dodge(width = 0.75),
    linewidth = 0.25
  ) +
  geom_boxplot(
    mapping = aes(
      x = all_results$NECRA,
      y = ps_sd_errs,
      z = sd_errs,
      fill = all_results$method
    ),
    outliers = FALSE,
    width = 0.75,
    linewidth = 0.25,
    alpha = 1
  ) +
  ggsci::scale_fill_d3(name = "Method",
                       labels = c(TeX("CL"), TeX("RW"))) +
  scale_x_continuous(
    name = "NECRA",
    limits = c(0, NA),
    oob = scales::oob_squish_infinite
  ) +
  scale_y_discrete(name = "", guide = guide_axis(angle = 0),
                   # labels = TeX(levels(ps_sd_errs))) +
                   labels = NULL) +
  geom_line(mapping = aes(
    y = 1:(2*length(unique(ps_sd_errs)))*0.5 + 0.25, x = 87000,
    group = factor(rep(letters[1:length(unique(ps_sd_errs))], each = 2))
  ), linewidth = 0.25) +
  geom_line(mapping = aes(
    y = rep(1:(2*length(unique(ps_sd_errs)))*0.5 + 0.25, each = 2),
    x = rep(c(86000, 87000), times = length(unique(ps_sd_errs))*2),
    group = rep(letters[1:(length(unique(ps_sd_errs))*2)], each = 2)
  ), linewidth = 0.25) +
  geom_text(mapping = aes(
    y = 1:(length(unique(ps_sd_errs))), x = 90000
  ), label = "***", angle = 90, size = 4) +

  ##
  geom_tile(
    mapping = aes(x = rep(c(-17500, -35000), times = length(levels(ps_sd_errs))),
                  y = rep(seq(length(levels(ps_sd_errs)
                  )), each = 2)),
    fill = "blue",
    alpha = 0,
    color = "black"
  ) +
  geom_text(
    mapping = aes(x = rep(c(-17500, -35000), each = length(levels(ps_sd_errs))),
                  y = rep(seq(length(levels(ps_sd_errs)
                  )), times = 2),
                  label = c(
                    rep(levels(sd_errs), times = length(levels(ps))),
                    rep(levels(ps), each = length(levels(sd_errs)))
                  )
    ),
    color = "black", size = 2.5
  ) +
  geom_text(
    mapping = aes(x = c(-17500, -35000),
                  y = length(levels(ps_sd_errs)) + 0.8
    ),
    label = c(
      TeX("\\it{$\\sigma_{\\epsilon}$}"),
      TeX("\\it{$p$}")
    ),
    color = "black", size = 3
  ) +
  coord_cartesian(xlim = c(0, 94000), ylim = c(1,length(levels(ps_sd_errs))), clip = "off") +
  ##
  theme_bw() +
  labs(title = TeX("***\\it{$p_W$}<0.001")) +
  theme(text = element_text(size = 12), plot.caption = element_text(hjust = 0),
        plot.title = element_text(size = 7))
plt_sd_err

ggsave(
  filename = paste0("plt p to NECRA sd_err.png"),
  plot = plt_sd_err,
  device = "png",
  width = 8,
  height = 8*0.5,
  dpi = 300
)
##################################

### p to NECRA c_ratio ya gore ####
c_ratios <- all_results$c_ratio
c_ratios <- arules::discretize(c_ratios, breaks = 3)

attr(c_ratios, "discretized:break")
# levels(c_ratios) <- c("[250,750)", "[750,1250)", "[1250,1750)", "[1750,2000]")
levels(c_ratios) <- c("[0.1,0.367)", "[0.367,0.633)", "[0.633,0.9]")
table(c_ratios)

ps <- all_results$p_imp
ps <- arules::discretize(ps, breaks = 3)

attr(ps, "discretized:break")

ps
c_ratios

ps_c_ratios <- paste0("$\\it{p}$:$", ps,"$-$", "\\it{c_r}$:\\", c_ratios)
ps_c_ratios <- factor(ps_c_ratios, levels = unique(ps_c_ratios)[c(1,4,7,2,5,8,3,6,9)])

levels(ps_c_ratios) <- sub(pattern = "\\[", replacement = "\\\\[", x = levels(ps_c_ratios))
levels(ps_c_ratios) <- sub(pattern = "\\]", replacement = "\\\\]", x = levels(ps_c_ratios))
levels(ps_c_ratios) <- sub(pattern = "9]", replacement = "9\\\\]", x = levels(ps_c_ratios))
TeX(levels(ps_c_ratios))


p_values <- sapply(levels(ps_c_ratios), function(m) {
  wilcox.test(x = all_results$NECRA[ps_c_ratios == m &
                                      all_results$method == "classical"],
              y = all_results$NECRA[ps_c_ratios == m &
                                      all_results$method == "phiWeighted"],
              paired = TRUE)$p.value
})

plt_c_ratio <- ggplot() +
  geom_errorbar(
    mapping = aes(
      x = all_results$NECRA,
      y = ps_c_ratios,
      z = c_ratios,
      fill = all_results$method
    ),
    stat = "boxplot",
    outliers = FALSE,
    width = 0.35,
    position = position_dodge(width = 0.75),
    linewidth = 0.25
  ) +
  geom_boxplot(
    mapping = aes(
      x = all_results$NECRA,
      y = ps_c_ratios,
      z = c_ratios,
      fill = all_results$method
    ),
    outliers = FALSE,
    width = 0.75,
    linewidth = 0.25,
    alpha = 1
  ) +
  ggsci::scale_fill_d3(name = "Method",
                       labels = c(TeX("CL"), TeX("RW"))) +
  scale_x_continuous(
    name = "NECRA",
    limits = c(0, NA),
    oob = scales::oob_squish_infinite
  ) +
  scale_y_discrete(name = "",
                   guide = guide_axis(angle = 0),
                   # labels = TeX(levels(ps_c_ratios))) +
                   labels = NULL) +
  geom_line(mapping = aes(
    x = 91000, y = 1:(2*length(unique(ps_c_ratios)))*0.5 + 0.25,
    group = factor(rep(letters[1:length(unique(ps_c_ratios))], each = 2))
  ), linewidth = 0.25) +
  geom_line(mapping = aes(
    x = rep(c(90000, 91000), times = length(unique(ps_c_ratios))*2),
    y = rep(1:(2*length(unique(ps_c_ratios)))*0.5 + 0.25, each = 2),
    group = rep(letters[1:(length(unique(ps_c_ratios))*2)], each = 2)
  ), linewidth = 0.25) +
  geom_text(mapping = aes(
    x = 94000, y = 1:(length(unique(ps_c_ratios)))
  ), label = "***", angle = 90, size = 4) +
  geom_tile(
    mapping = aes(x = rep(c(-17500, -35000), times = length(levels(ps_c_ratios))),
                  y = rep(seq(length(levels(ps_c_ratios)
                  )), each = 2)),
    fill = "blue",
    alpha = 0,
    color = "black"
  ) +
  geom_text(
    mapping = aes(x = rep(c(-17500, -35000), each = length(levels(ps_c_ratios))),
                  y = rep(seq(length(levels(ps_c_ratios)
                  )), times = 2),
                  label = c(
                    rep(levels(c_ratios), times = 3),
                    rep(levels(ps), each = 3)
                  )
                  ),
    color = "black", size = 2.5
  ) +
  geom_text(
    mapping = aes(x = c(-17500, -35000),
                  y = length(levels(ps_c_ratios)) + 0.8
    ),
    label = c(
      TeX("\\it{$c_r$}"),
      TeX("\\it{$p$}")
    ),
    color = "black", size = 3
  ) +
  coord_cartesian(xlim = c(0, 94000), ylim = c(1,length(levels(ps_c_ratios))), clip = "off") +
  theme_bw() +
  labs(title = TeX("***\\it{$p_W$}<0.001")) +
  theme(plot.title = element_text(size = 7),
        axis.line = element_line(linewidth = 0.25),
        plot.margin = margin(l = 65, t = 5))
plt_c_ratio


ggsave(
  filename = paste0("plt p to NECRA c_ratio.png"),
  plot = plt_c_ratio,
  device = "png",
  width = 8,
  height = 8*0.5,
  dpi = 300
)
##################################



### p general line plot ####

plt_p <- ggplot() +
  stat_summary(
    mapping = aes(
      x = all_results$p_imp*0.5,
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
      x = as.factor(all_results$p_imp),
      y = all_results$NECRA,
      z = all_results$method,
      fill = all_results$method
    ),
    shape = 21,
    stroke = 0.25
  ) +
  ggsci::scale_fill_d3(name = "Method",
                       labels = c(TeX("CL"), TeX("RW"))) +
  ggsci::scale_color_d3(name = "Method",
                        labels = c(TeX("CL"), TeX("RW"))) +
  scale_y_continuous(
    name = "",
    limits = c(0, NA),
    oob = scales::oob_squish_infinite
  ) +
  scale_x_discrete(
    name = TeX("\\it{p}"),
    guide = guide_axis(angle = 0),
    breaks = 2:20
  ) +
  theme_bw() +
  theme(text = element_text(size = 12))

plt_p

ggsave(
  filename = "plt p general line.png",
  plot = plt_p,
  device = "png",
  width = 8,
  height = 8*0.5,
  dpi = 300
)
########################################


### four in one ####
plt_all <- ggpubr::ggarrange(
  plt_n + labs(caption = "a)") + theme(plot.caption = element_text(hjust = 0.5)),
  plt_sd_err + labs(caption = "b)") + theme(plot.caption = element_text(hjust = 0.5)),
  plt_c_ratio + labs(caption = "c)") + theme(plot.caption = element_text(hjust = 0.5)),
  plt_p + labs(caption = "d)") + theme(plot.caption = element_text(hjust = 0.5)) +
    annotate(geom = "text", x = -2, y = 17000, label = "NECRA", angle = 90) +
    coord_cartesian(xlim = c(1, 10), clip = "off"),
  ncol = 2,
  nrow = 2,
  common.legend = TRUE,
  legend = "top",
  align = "hv"
)
plt_all
ggsave(filename = "plt all for p.png", plot = plt_all, device = "png",
       width = 10, height = 10*0.8, dpi = 300)


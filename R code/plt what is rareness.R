set.seed(2)
c_u <- 0.3
c_o <- 0.7
library(ImbRegSamp)
library(FatihResearch)
library(ggplot2)
library(cowplot)
library(latex2exp)
# torch <- reticulate::import(module = "torch")
library(extrafont)

loadfonts()
fonts()
par(family = "LM Roman 10")

### n 250 ####
data <-
  f_simulation_reg_nonlinear_Fatih(
    p_imp = 1,
    p_nonImp = 0,
    n = 250,
    mean_err = 0,
    sd_err = 4,
    imbalanced = TRUE,
    homoscedastic = TRUE
  )
x <- data[, 1, drop = FALSE]
y <- data[, 2]

m_ai <- analyzeImbalance(x = x, y = y, thresh_rel = 0.5)

length(m_ai$x_original)
length(m_ai$y_original)
m_ai$groups_original

plt_point <- ggplot() +
  geom_point(
    mapping = aes(
      x = m_ai$x_original,
      y = m_ai$y_original,
      fill = m_ai$groups_original
    ),
    shape = 21,
    size = 3,
    stroke = 0.25
  ) +
  scale_fill_manual(
    name = "",
    values = ggsci::pal_d3()(3),
    labels = c("LR", "NR", "UR")
  ) +
  geom_hline(
    yintercept = c(m_ai$min_y_upperRare, m_ai$max_y_lowerRare),
    linewidth = 0.25
  ) +
  scale_x_continuous(name = TeX("\\it{x}")) +
  scale_y_continuous(name = "", breaks = NULL) +
  labs(caption = "c)") +
  theme_bw() +
  theme(legend.position = "top",
        plot.caption = element_text(hjust = 0.5, vjust = -1),
        text = element_text(family = "LM Roman 10"))
plt_point

plt_box <- ggplot() +
  geom_boxplot(mapping = aes(x = 1, y = data[, 2]), fill = "orange") +
  scale_x_continuous(
    name = TeX("\\it{x}"),
    breaks = NULL,
    expand = c(0.5, 0)
  ) +
  scale_y_continuous(name = TeX("\\it{d}")) +
  labs(caption = "a)") +
  theme_bw() +
  theme(plot.margin = margin(
    t = 5,
    r = 0,
    b = 20,
    l = 0
  ),
  plot.caption = element_text(hjust = 0.5, vjust = -7),
  text = element_text(family = "LM Roman 10"))
plt_box

y_grid <- seq(min(y), max(y), 0.1)

m_rel_grid <- relevance_PCHIP(y = y, y_new = y_grid)
m_rel <- relevance_PCHIP(y = y, y_new = m_ai$y_original)

plt_rel <- ggplot() +
  geom_line(mapping = aes(x = y_grid,
                          y = m_rel_grid$rel)) +
  geom_point(
    mapping = aes(
      x = m_ai$y_original,
      y = m_rel$rel,
      fill = m_ai$groups_original
    ),
    shape = 21,
    size = 3,
    stroke = 0.25
  ) +
  scale_fill_manual(
    name = "",
    values = ggsci::pal_d3()(3),
    labels = c("LR", "NR", "UR")
  ) +
  geom_hline(yintercept = 0.5, linewidth = 0.25) +
  scale_x_continuous(name = "", breaks = NULL) +
  # scale_y_continuous(name = TeX("\\it{\\phi(d)}")) +
  scale_y_continuous(name = expression(italic("\U03D5(d)"))) +
  labs(caption = "b)") +
  coord_flip() +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.margin = margin(
      t = 5,
      r = 0,
      b = 2,
      l = 0
    ),
    plot.caption = element_text(hjust = 0.5, vjust = 1),
    text = element_text(family = "LM Roman 10"))
plt_rel

plt_legend <-
  get_plot_component(plt_point, "guide-box", return_all = TRUE)
plt_point <- plt_point + theme(legend.position = "none")

plt_explainRare <- plot_grid(
  plt_legend[[4]],
  plot_grid(
    plt_box,
    plt_rel,
    plt_point,
    nrow = 1,
    ncol = 3,
    rel_widths = c(0.2, 1, 1)
  ),
  nrow = 2,
  ncol = 1,
  rel_heights = c(0.1, 1)
)

ggsave(
  filename = "plt explain rare.png",
  plot = plt_explainRare,
  device = "png",
  width = 11,
  height = 3.25,
  dpi = 300
)

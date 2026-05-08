set.seed(2)
c_u <- 0.3
c_o <- 0.7
library(ImbRegSamp)
library(FatihResearch)
library(cowplot)
# torch <- reticulate::import(module = "torch")

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
x <- data[,1,drop = FALSE]
y <- data[,2]

m_rel <- relevance_PCHIP(y = y)
m_ai <- analyzeImbalance(x = x, y = y)

m_lm_CL <- lm(formula = y~x, data = data.frame(x = x, y = y))
m_lm_RW <- lm(formula = y~x, weights = m_rel$rel, data = data.frame(x = x, y = y))

x_grid <- seq(min(x), max(x), 0.01)
pred_CL <- predict(object = m_lm_CL, newdata = data.frame(x = x_grid), interval = "confidence", level = 0.95)
pred_RW <- predict(object = m_lm_RW, newdata = data.frame(x = x_grid), interval = "confidence", level = 0.95)

M_pred_CL <- sapply(1:1000, function(m) {
  ii <- sample(1:length(y), replace = TRUE, size = length(y))
  xx <- x[ii,,drop = FALSE]
  yy <- y[ii]

  m_lm_CL <- lm(formula = y~x, data = data.frame(x = xx, y = yy))
  pred_CL <- predict(object = m_lm_CL, newdata = data.frame(x = x_grid))
  return(pred_CL)
})
pred_CL <- t(apply(M_pred_CL, 1, function(m) c(mean(m), quantile(m, 0.025), quantile(m, 0.975))))

M_pred_RW <- sapply(1:1000, function(m) {
  ii <- sample(1:length(y), replace = TRUE, size = length(y))
  xx <- x[ii,,drop = FALSE]
  yy <- y[ii]
  rrel <- m_rel$rel[ii]
  m_lm_RW <- lm(formula = y~x, data = data.frame(x = xx, y = yy), weights = rrel)
  pred_RW <- predict(object = m_lm_RW, newdata = data.frame(x = x_grid))
  return(pred_RW)
})
pred_RW <- t(apply(M_pred_RW, 1, function(m) c(mean(m), quantile(m, 0.025), quantile(m, 0.975))))


plt_lm <- ggplot() +
  geom_ribbon(mapping = aes(
    x = rep(x_grid, 2), ymax = c(pred_CL[,3], pred_RW[,3]),
    ymin = c(pred_CL[,2], pred_RW[,2]), fill = factor(rep(c("CL","RW"), each = length(x_grid)))
  ), alpha = 0.4) +
  scale_fill_manual(name = "Methods", values = c("red", "blue"), labels = c("CL", "RW")) +
  ggnewscale::new_scale_fill() +
  geom_point(mapping = aes(
    x = m_ai$x_original,
    y = m_ai$y_original,
    fill = m_ai$groups_original
  ), shape = 21, size = 2, stroke = 0.25) +
  scale_fill_manual(name = "", values = ggsci::pal_d3()(3), labels = c("LR", "NR", "UR")) +
  geom_line(mapping = aes(
    x = rep(x_grid, 2), y = c(pred_CL[,1], pred_RW[,1]), color = factor(rep(c("CL","RW"), each = length(x_grid)))
  )) +
  scale_color_manual(name = "Methods", values = c("red", "blue")) +
  scale_x_continuous(name = TeX("\\it{x}")) +
  scale_y_continuous(name = TeX("\\it{d}")) +
  labs(caption = "a)") +
  theme_bw() +
  theme(legend.position = "top", text = element_text(size = 12), plot.caption = element_text(hjust = 0.5))


m_NNET_CL <-
  f_NNET_newswendor(
    x_train = x,
    y_train = y,
    x_test = as.matrix(x_grid),
    w = NULL,
    n_epochInitialTraining = 500
  )
m_NNET_RW <-
  f_NNET_newswendor(
    x_train = x,
    y_train = y,
    x_test = as.matrix(x_grid),
    w = m_rel$rel,
    n_epochInitialTraining = 500
  )

plt_nnet <- ggplot() +
  geom_ribbon(mapping = aes(
    x = rep(x_grid, 2), ymax = c(m_NNET_CL$pred_upper, m_NNET_RW$pred_upper),
    ymin = c(m_NNET_CL$pred_lower, m_NNET_RW$pred_lower), fill = factor(rep(c("CL","RW"), each = length(x_grid)))
  ), alpha = 0.4) +
  scale_fill_manual(name = "Methods", values = c("red", "blue"), labels = c("CL", "RW")) +
  ggnewscale::new_scale_fill() +
  geom_point(mapping = aes(
    x = m_ai$x_original,
    y = m_ai$y_original,
    fill = m_ai$groups_original
  ), shape = 21, size = 2, stroke = 0.25) +
  scale_fill_manual(name = "", values = ggsci::pal_d3()(3), labels = c("LR", "NR", "UR")) +
  geom_line(mapping = aes(
    x = rep(x_grid, 2), y = c(m_NNET_CL$pred, m_NNET_RW$pred), color = factor(rep(c("CL","RW"), each = length(x_grid)))
  )) +
  scale_color_manual(name = "Methods", values = c("red", "blue")) +
  scale_x_continuous(name = TeX("\\it{x}")) +
  scale_y_continuous(name = TeX("\\it{d}")) +
  labs(caption = "b)") +
  theme_bw() +
  theme(legend.position = "top", text = element_text(size = 12), plot.caption = element_text(hjust = 0.5))

plt_all <- ggpubr::ggarrange(
  plt_lm, plt_nnet, common.legend = TRUE
)
ggsave(filename = "plt comparison of CL and RW.png", plot = plt_all, device = "png",
       width = 6, height = 3, dpi = 300)


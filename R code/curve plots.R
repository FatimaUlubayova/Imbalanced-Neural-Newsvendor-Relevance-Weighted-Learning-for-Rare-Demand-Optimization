c_u <- 0.3
c_o <- 0.7
library(ImbRegSamp)
library(FatihResearch)
# torch <- reticulate::import(module = "torch")

### n 250 ####
data <-
  f_simulation_reg_nonlinear_Fatih(
    p_imp = 10,
    p_nonImp = 0,
    n = 250,
    mean_err = 0,
    sd_err = 1,
    imbalanced = TRUE,
    homoscedastic = TRUE
  )

p <- ncol(data) - 1
n <- nrow(data)
x <- data[, 1:p, drop = FALSE]
y <- data[, p + 1]

m_rel <- relevance_PCHIP(y = y, y_new = y)

i_train <- caret::createDataPartition(y = y, times = 1, p = 0.8, list = FALSE)

x_train <- x[i_train,]
y_train <- y[i_train]

x_test <- x[-i_train,]
y_test <- y[-i_train]

m_rel <- relevance_PCHIP(y = y, y_new = y)
m_rel_train <- relevance_PCHIP(y = y, y_new = y_train)
m_rel_test <- relevance_PCHIP(y = y, y_new = y_test)


pred_classical <- f_NNET_newswendor(
  x_train = x_train,
  y_train = y_train,
  x_test = x_test,
  w = NULL, n_epochInitialTraining = 500,
  c_u = c_u,
  c_o = c_o
)

pred_phiWeighted <- f_NNET_newswendor(
  x_train = x_train,
  y_train = y_train,
  x_test = x_test,
  w = m_rel_train$rel, n_epochInitialTraining = 500,
  c_u = c_u,
  c_o = c_o
)

w <- m_rel_test$rel

NECR <- function(pred, truth, w, c_u = 1, c_o = 1, coef = 1, ...) {
  T_E_grid <- seq(0,1, length = 1000)

  NECRs <- sapply(T_E_grid, function(m) {
    pp <- pred[w > m]
    tt <- truth[w > m]

    sum((c_o * pmax(pp - tt, 0)*abs(pp - tt)^coef +
           c_u * pmax(tt - pp, 0)*abs(pp - tt)^coef))
  })

  return(list(
    T_E_grid = T_E_grid,
    NECR = NECRs
  ))
}

NECR_classical <- NECR(
  pred = pred_classical$pred,
  truth = y_test,
  w = m_rel_test$rel,
  c_u = c_u,
  c_o = c_o
)
NECR_phiWeighted <- NECR(
  pred = pred_phiWeighted$pred,
  truth = y_test,
  w = m_rel_test$rel,
  c_u = c_u,
  c_o = c_o
)

colors <- ggsci::pal_d3()(2)

NECRA_classical <- metricReg_newsvendor(
  pred = pred_classical$pred,
  truth = y_test,
  w = m_rel_test$rel,
  c_u = c_u,
  c_o = c_o
)
NECRA_phiWeighted <- metricReg_newsvendor(
  pred = pred_phiWeighted$pred,
  truth = y_test,
  w = m_rel_test$rel,
  c_u = c_u,
  c_o = c_o
)

plt_n250 <- local({
  NECR_classical <- NECR_classical
  NECR_phiWeighted <- NECR_phiWeighted
  NECRA_classical <- NECRA_classical
  NECRA_phiWeighted <- NECRA_phiWeighted

  ggplot() +
    geom_step(
      mapping = aes(x = NECR_classical$T_E_grid, y = NECR_classical$NECR),
      color = colors[1]
    ) +
    geom_step(
      mapping = aes(x = NECR_phiWeighted$T_E_grid, y = NECR_phiWeighted$NECR),
      color = colors[2]
    ) +
    geom_step(mapping = aes(
      x = c(100, 101),
      y = c(100, 101),
      color = factor(c("A", "B"), levels = c("A", "B"))
    )) +
    scale_color_manual(name = "", values = colors, labels = c("CL", "RW")) +
    geom_label(mapping = aes(x = 0.3, y = diff(range(
      NECR_classical$NECR
    )) * 0.1),
    label = TeX(paste0("\\it{$n = $}", n, ", \\it{$c_u =$}", c_u, ", \\it{$c_o =}$", c_o))) +
    # geom_label(mapping = aes(x = 0.2, y = diff(range(
    #   NECR_classical$NECR
    # )) * 0.3),
    # label = TeX(paste0("\\it{$NECRA_{Cl} = $}", formatC(NECRA_classical, digits = 2, format = "f"),
    #                    ", \\it{$NECRA_{\\phi} = $}", formatC(NECRA_phiWeighted, digits = 2, format = "f")))) +
    scale_y_continuous(name = TeX("\\it{$NECR_t$}"), breaks = NULL) +
    scale_x_continuous(name = TeX("\\it{$t$}"), limits = c(0, 1)) +
    theme_bw(base_line_size = 0) +
    theme(legend.position = "top")
})

mean(NECR_classical$T_E_grid*NECR_classical$NECR)
mean(NECR_phiWeighted$T_E_grid*NECR_phiWeighted$NECR)


##########

### n 750 ####

data <-
  f_simulation_reg_nonlinear_Fatih(
    p_imp = 10,
    p_nonImp = 0,
    n = 750,
    mean_err = 0,
    sd_err = 1,
    imbalanced = TRUE,
    homoscedastic = TRUE
  )
p <- ncol(data) - 1
n <- nrow(data)
x <- data[, 1:p, drop = FALSE]
y <- data[, p + 1]

m_rel <- relevance_PCHIP(y = y, y_new = y)

i_train <- caret::createDataPartition(y = y, times = 1, p = 0.8, list = FALSE)

x_train <- x[i_train,]
y_train <- y[i_train]

x_test <- x[-i_train,]
y_test <- y[-i_train]

m_rel <- relevance_PCHIP(y = y, y_new = y)
m_rel_train <- relevance_PCHIP(y = y, y_new = y_train)
m_rel_test <- relevance_PCHIP(y = y, y_new = y_test)

pred_classical <- f_NNET_newswendor(
  x_train = x_train,
  y_train = y_train,
  x_test = x_test,
  w = NULL, n_epochInitialTraining = 500,
  c_u = c_u,
  c_o = c_o
)

pred_phiWeighted <- f_NNET_newswendor(
  x_train = x_train,
  y_train = y_train,
  x_test = x_test,
  w = m_rel_train$rel, n_epochInitialTraining = 500,
  c_u = c_u,
  c_o = c_o
)

w <- m_rel_test$rel

NECR <- function(pred, truth, w, c_u = 1, c_o = 1, coef = 1, ...) {
  T_E_grid <- seq(0,1, length = 1000)

  NECRs <- sapply(T_E_grid, function(m) {
    pp <- pred[w > m]
    tt <- truth[w > m]

    sum((c_o * pmax(pp - tt, 0)*abs(pp - tt)^coef +
           c_u * pmax(tt - pp, 0)*abs(pp - tt)^coef))
  })

  return(list(
    T_E_grid = T_E_grid,
    NECR = NECRs
  ))
}

NECR_classical <- NECR(
  pred = pred_classical$pred,
  truth = y_test,
  w = m_rel_test$rel,
  c_u = c_u,
  c_o = c_o
)
NECR_phiWeighted <- NECR(
  pred = pred_phiWeighted$pred,
  truth = y_test,
  w = m_rel_test$rel,
  c_u = c_u,
  c_o = c_o
)

colors <- ggsci::pal_d3()(2)

plt_n750 <- local({
  NECR_classical <- NECR_classical
  NECR_phiWeighted <- NECR_phiWeighted
  NECRA_classical <- NECRA_classical
  NECRA_phiWeighted <- NECRA_phiWeighted

  ggplot() +
    geom_step(
      mapping = aes(x = NECR_classical$T_E_grid, y = NECR_classical$NECR),
      color = colors[1]
    ) +
    geom_step(
      mapping = aes(x = NECR_phiWeighted$T_E_grid, y = NECR_phiWeighted$NECR),
      color = colors[2]
    ) +
    geom_step(mapping = aes(
      x = c(100, 101),
      y = c(100, 101),
      color = factor(c("A", "B"), levels = c("A", "B"))
    )) +
    scale_color_manual(name = "", values = colors, labels = c("CL", "RW")) +
    geom_label(mapping = aes(x = 0.3, y = diff(range(
      NECR_classical$NECR
    )) * 0.1),
    label = TeX(paste0("\\it{$n = $}", n, ", \\it{$c_u =$}", c_u, ", \\it{$c_o =}$", c_o))) +
    # geom_label(mapping = aes(x = 0.2, y = diff(range(
    #   NECR_classical$NECR
    # )) * 0.3),
    # label = TeX(paste0("\\it{$NECRA_{Cl} = $}", formatC(NECRA_classical, digits = 2, format = "f"),
    #                    ", \\it{$NECRA_{\\phi} = $}", formatC(NECRA_phiWeighted, digits = 2, format = "f")))) +
    scale_y_continuous(name = TeX("\\it{$NECR_t$}"), breaks = NULL) +
    scale_x_continuous(name = TeX("\\it{$t$}"), limits = c(0, 1)) +
    theme_bw() +
    theme(
      legend.position = "inside",
      legend.position.inside = c(0.8, 0.9),
      legend.background = element_rect(fill = rgb(
        red = 0,
        green = 0,
        blue = 0,
        alpha = 0
      )),
      legend.box.background = element_rect(),
      legend.title = element_text(size = 0),
      legend.box.margin = margin(t = -9, r = 0, b = -3, l = 0)
    )
})
plt_n750
ggsave(
  filename = paste0("plt NECR curve n ", 750, " c_u", c_u, " c_o", c_o, ".png"),
  plot = plt_n750,
  device = "png",
  width = 3.5,
  height = 3.5
)


mean(NECR_classical$T_E_grid*NECR_classical$NECR)
mean(NECR_phiWeighted$T_E_grid*NECR_phiWeighted$NECR)

NECRA_classical <- metricReg_newsvendor(
  pred = pred_classical$pred,
  truth = y_test,
  w = m_rel_test$rel,
  c_u = c_u,
  c_o = c_o
)
NECRA_phiWeighted <- metricReg_newsvendor(
  pred = pred_phiWeighted$pred,
  truth = y_test,
  w = m_rel_test$rel,
  c_u = c_u,
  c_o = c_o
)
##########


### n 2000 ####

data <-
  f_simulation_reg_nonlinear_Fatih(
    p_imp = 10,
    p_nonImp = 0,
    n = 2000,
    mean_err = 0,
    sd_err = 1,
    imbalanced = TRUE,
    homoscedastic = TRUE
  )
p <- ncol(data) - 1
n <- nrow(data)
x <- data[, 1:p, drop = FALSE]
y <- data[, p + 1]

m_rel <- relevance_PCHIP(y = y, y_new = y)

i_train <- caret::createDataPartition(y = y, times = 1, p = 0.8, list = FALSE)

x_train <- x[i_train,]
y_train <- y[i_train]

x_test <- x[-i_train,]
y_test <- y[-i_train]

m_rel <- relevance_PCHIP(y = y, y_new = y)
m_rel_train <- relevance_PCHIP(y = y, y_new = y_train)
m_rel_test <- relevance_PCHIP(y = y, y_new = y_test)

pred_classical <- f_NNET_newswendor(
  x_train = x_train,
  y_train = y_train,
  x_test = x_test,
  w = NULL, n_epochInitialTraining = 500,
  c_u = c_u,
  c_o = c_o
)

pred_phiWeighted <- f_NNET_newswendor(
  x_train = x_train,
  y_train = y_train,
  x_test = x_test,
  w = m_rel_train$rel, n_epochInitialTraining = 500,
  c_u = c_u,
  c_o = c_o
)

w <- m_rel_test$rel

NECR <- function(pred, truth, w, c_u = 1, c_o = 1, coef = 1, ...) {
  T_E_grid <- seq(0,1, length = 1000)

  NECRs <- sapply(T_E_grid, function(m) {
    pp <- pred[w > m]
    tt <- truth[w > m]

    sum((c_o * pmax(pp - tt, 0)*abs(pp - tt)^coef +
           c_u * pmax(tt - pp, 0)*abs(pp - tt)^coef))
  })

  return(list(
    T_E_grid = T_E_grid,
    NECR = NECRs
  ))
}

NECR_classical <- NECR(
  pred = pred_classical$pred,
  truth = y_test,
  w = m_rel_test$rel,
  c_u = c_u,
  c_o = c_o
)
NECR_phiWeighted <- NECR(
  pred = pred_phiWeighted$pred,
  truth = y_test,
  w = m_rel_test$rel,
  c_u = c_u,
  c_o = c_o
)

colors <- ggsci::pal_d3()(2)

plt_n2000 <- local({
  NECR_classical <- NECR_classical
  NECR_phiWeighted <- NECR_phiWeighted
  NECRA_classical <- NECRA_classical
  NECRA_phiWeighted <- NECRA_phiWeighted

  ggplot() +
    geom_step(
      mapping = aes(x = NECR_classical$T_E_grid, y = NECR_classical$NECR),
      color = colors[1]
    ) +
    geom_step(
      mapping = aes(x = NECR_phiWeighted$T_E_grid, y = NECR_phiWeighted$NECR),
      color = colors[2]
    ) +
    geom_step(mapping = aes(
      x = c(100, 101),
      y = c(100, 101),
      color = factor(c("A", "B"), levels = c("A", "B"))
    )) +
    scale_color_manual(name = "", values = colors, labels = c("CL", "RW")) +
    geom_label(mapping = aes(x = 0.3, y = diff(range(
      NECR_classical$NECR
    )) * 0.1),
    label = TeX(paste0("\\it{$n = $}", n, ", \\it{$c_u =$}", c_u, ", \\it{$c_o =}$", c_o))) +
    # geom_label(mapping = aes(x = 0.2, y = diff(range(
    #   NECR_classical$NECR
    # )) * 0.3),
    # label = TeX(paste0("\\it{$NECRA_{Cl} = $}", formatC(NECRA_classical, digits = 2, format = "f"),
    #                    ", \\it{$NECRA_{\\phi} = $}", formatC(NECRA_phiWeighted, digits = 2, format = "f")))) +
    scale_y_continuous(name = TeX("\\it{$NECR_t$}"), breaks = NULL) +
    scale_x_continuous(name = TeX("\\it{$t$}"), limits = c(0, 1)) +
    theme_bw(base_line_size = 0) +
    theme(legend.position = "top")
})

mean(NECR_classical$T_E_grid*NECR_classical$NECR)
mean(NECR_phiWeighted$T_E_grid*NECR_phiWeighted$NECR)

NECRA_classical <- metricReg_newsvendor(
  pred = pred_classical$pred,
  truth = y_test,
  w = m_rel_test$rel,
  c_u = c_u,
  c_o = c_o
)
NECRA_phiWeighted <- metricReg_newsvendor(
  pred = pred_phiWeighted$pred,
  truth = y_test,
  w = m_rel_test$rel,
  c_u = c_u,
  c_o = c_o
)
##########



plt_list <- list(plt_n250, plt_n750, plt_n2000)


legend <- get_plot_component(plt_list[[1]], "guide-box", return_all = TRUE)
for (i in 1:length(plt_list)) {
  # plt_list[[i]] <- plt_list[[i]] + theme(legend.position = "none") + coord_polar()
  plt_list[[i]] <- plt_list[[i]] + theme(legend.position = "none")
}

plt_together <- plot_grid(
  legend[[4]],
  plot_grid(plotlist = plt_list, nrow = 1, ncol = 3),
  nrow = 2, ncol = 1,
  rel_heights = c(0.1, 0.9)
)
plt_together

ggsave(
  filename = paste0("plt NECR curve together", "c_u", c_u, "c_o", c_o, ".png"),
  plot = plt_together,
  device = "png",
  width = 11,
  height = 3.5,
  dpi = 300
)

####################################################################################


c_u <- 0.5
c_o <- 0.5

### n 250 ####
data <-
  f_simulation_reg_nonlinear_Fatih(
    p_imp = 10,
    p_nonImp = 0,
    n = 250,
    mean_err = 0,
    sd_err = 1,
    imbalanced = TRUE,
    homoscedastic = TRUE
  )
p <- ncol(data) - 1
n <- nrow(data)
x <- data[, 1:p, drop = FALSE]
y <- data[, p + 1]

m_rel <- relevance_PCHIP(y = y, y_new = y)

i_train <- caret::createDataPartition(y = y, times = 1, p = 0.8, list = FALSE)

x_train <- x[i_train,]
y_train <- y[i_train]

x_test <- x[-i_train,]
y_test <- y[-i_train]

m_rel <- relevance_PCHIP(y = y, y_new = y)
m_rel_train <- relevance_PCHIP(y = y, y_new = y_train)
m_rel_test <- relevance_PCHIP(y = y, y_new = y_test)

pred_classical <- f_NNET_newswendor(
  x_train = x_train,
  y_train = y_train,
  x_test = x_test,
  w = NULL, n_epochInitialTraining = 500,
  c_u = c_u,
  c_o = c_o
)

pred_phiWeighted <- f_NNET_newswendor(
  x_train = x_train,
  y_train = y_train,
  x_test = x_test,
  w = m_rel_train$rel, n_epochInitialTraining = 500,
  c_u = c_u,
  c_o = c_o
)

w <- m_rel_test$rel

NECR <- function(pred, truth, w, c_u = 1, c_o = 1, coef = 1, ...) {
  T_E_grid <- seq(0,1, length = 1000)

  NECRs <- sapply(T_E_grid, function(m) {
    pp <- pred[w > m]
    tt <- truth[w > m]

    sum((c_o * pmax(pp - tt, 0)*abs(pp - tt)^coef +
           c_u * pmax(tt - pp, 0)*abs(pp - tt)^coef))
  })

  return(list(
    T_E_grid = T_E_grid,
    NECR = NECRs
  ))
}

NECR_classical <- NECR(
  pred = pred_classical$pred,
  truth = y_test,
  w = m_rel_test$rel,
  c_u = c_u,
  c_o = c_o
)
NECR_phiWeighted <- NECR(
  pred = pred_phiWeighted$pred,
  truth = y_test,
  w = m_rel_test$rel,
  c_u = c_u,
  c_o = c_o
)

colors <- ggsci::pal_d3()(2)

NECRA_classical <- metricReg_newsvendor(
  pred = pred_classical$pred,
  truth = y_test,
  w = m_rel_test$rel,
  c_u = c_u,
  c_o = c_o
)
NECRA_phiWeighted <- metricReg_newsvendor(
  pred = pred_phiWeighted$pred,
  truth = y_test,
  w = m_rel_test$rel,
  c_u = c_u,
  c_o = c_o
)

plt_n250 <- local({
  NECR_classical <- NECR_classical
  NECR_phiWeighted <- NECR_phiWeighted
  NECRA_classical <- NECRA_classical
  NECRA_phiWeighted <- NECRA_phiWeighted

  ggplot() +
    geom_step(
      mapping = aes(x = NECR_classical$T_E_grid, y = NECR_classical$NECR),
      color = colors[1]
    ) +
    geom_step(
      mapping = aes(x = NECR_phiWeighted$T_E_grid, y = NECR_phiWeighted$NECR),
      color = colors[2]
    ) +
    geom_step(mapping = aes(
      x = c(100, 101),
      y = c(100, 101),
      color = factor(c("A", "B"), levels = c("A", "B"))
    )) +
    scale_color_manual(name = "", values = colors, labels = c("CL", "RW")) +
    geom_label(mapping = aes(x = 0.3, y = diff(range(
      NECR_classical$NECR
    )) * 0.1),
    label = TeX(paste0("\\it{$n = $}", n, ", \\it{$c_u =$}", c_u, ", \\it{$c_o =}$", c_o))) +
    # geom_label(mapping = aes(x = 0.2, y = diff(range(
    #   NECR_classical$NECR
    # )) * 0.3),
    # label = TeX(paste0("\\it{$NECRA_{Cl} = $}", formatC(NECRA_classical, digits = 2, format = "f"),
    #                    ", \\it{$NECRA_{\\phi} = $}", formatC(NECRA_phiWeighted, digits = 2, format = "f")))) +
        scale_y_continuous(name = TeX("\\it{$NECR_t$}"), breaks = NULL) +
    scale_x_continuous(name = TeX("\\it{$t$}"), limits = c(0, 1)) +
    theme_bw(base_line_size = 0) +
    theme(legend.position = "top")
})

mean(NECR_classical$T_E_grid*NECR_classical$NECR)
mean(NECR_phiWeighted$T_E_grid*NECR_phiWeighted$NECR)


##########

### n 750 ####

data <-
  f_simulation_reg_nonlinear_Fatih(
    p_imp = 10,
    p_nonImp = 0,
    n = 750,
    mean_err = 0,
    sd_err = 1,
    imbalanced = TRUE,
    homoscedastic = TRUE
  )
p <- ncol(data) - 1
n <- nrow(data)
x <- data[, 1:p, drop = FALSE]
y <- data[, p + 1]

m_rel <- relevance_PCHIP(y = y, y_new = y)

i_train <- caret::createDataPartition(y = y, times = 1, p = 0.8, list = FALSE)

x_train <- x[i_train,]
y_train <- y[i_train]

x_test <- x[-i_train,]
y_test <- y[-i_train]

m_rel <- relevance_PCHIP(y = y, y_new = y)
m_rel_train <- relevance_PCHIP(y = y, y_new = y_train)
m_rel_test <- relevance_PCHIP(y = y, y_new = y_test)

pred_classical <- f_NNET_newswendor(
  x_train = x_train,
  y_train = y_train,
  x_test = x_test,
  w = NULL, n_epochInitialTraining = 500,
  c_u = c_u,
  c_o = c_o
)

pred_phiWeighted <- f_NNET_newswendor(
  x_train = x_train,
  y_train = y_train,
  x_test = x_test,
  w = m_rel_train$rel, n_epochInitialTraining = 500,
  c_u = c_u,
  c_o = c_o
)

w <- m_rel_test$rel

NECR <- function(pred, truth, w, c_u = 1, c_o = 1, coef = 1, ...) {
  T_E_grid <- seq(0,1, length = 1000)

  NECRs <- sapply(T_E_grid, function(m) {
    pp <- pred[w > m]
    tt <- truth[w > m]

    sum((c_o * pmax(pp - tt, 0)*abs(pp - tt)^coef +
           c_u * pmax(tt - pp, 0)*abs(pp - tt)^coef))
  })

  return(list(
    T_E_grid = T_E_grid,
    NECR = NECRs
  ))
}

NECR_classical <- NECR(
  pred = pred_classical$pred,
  truth = y_test,
  w = m_rel_test$rel,
  c_u = c_u,
  c_o = c_o
)
NECR_phiWeighted <- NECR(
  pred = pred_phiWeighted$pred,
  truth = y_test,
  w = m_rel_test$rel,
  c_u = c_u,
  c_o = c_o
)

colors <- ggsci::pal_d3()(2)

plt_n750 <- local({
  NECR_classical <- NECR_classical
  NECR_phiWeighted <- NECR_phiWeighted
  NECRA_classical <- NECRA_classical
  NECRA_phiWeighted <- NECRA_phiWeighted

  ggplot() +
    geom_step(
      mapping = aes(x = NECR_classical$T_E_grid, y = NECR_classical$NECR),
      color = colors[1]
    ) +
    geom_step(
      mapping = aes(x = NECR_phiWeighted$T_E_grid, y = NECR_phiWeighted$NECR),
      color = colors[2]
    ) +
    geom_step(mapping = aes(
      x = c(100, 101),
      y = c(100, 101),
      color = factor(c("A", "B"), levels = c("A", "B"))
    )) +
    scale_color_manual(name = "", values = colors, labels = c("CL", "RW")) +
    geom_label(mapping = aes(x = 0.3, y = diff(range(
      NECR_classical$NECR
    )) * 0.1),
    label = TeX(paste0("\\it{$n = $}", n, ", \\it{$c_u =$}", c_u, ", \\it{$c_o =}$", c_o))) +
    # geom_label(mapping = aes(x = 0.2, y = diff(range(
    #   NECR_classical$NECR
    # )) * 0.3),
    # label = TeX(paste0("\\it{$NECRA_{Cl} = $}", formatC(NECRA_classical, digits = 2, format = "f"),
    #                    ", \\it{$NECRA_{\\phi} = $}", formatC(NECRA_phiWeighted, digits = 2, format = "f")))) +
        scale_y_continuous(name = TeX("\\it{$NECR_t$}"), breaks = NULL) +
    scale_x_continuous(name = TeX("\\it{$t$}"), limits = c(0, 1)) +
    theme_bw(base_line_size = 0) +
    theme(legend.position = "top")
})

mean(NECR_classical$T_E_grid*NECR_classical$NECR)
mean(NECR_phiWeighted$T_E_grid*NECR_phiWeighted$NECR)

NECRA_classical <- metricReg_newsvendor(
  pred = pred_classical$pred,
  truth = y_test,
  w = m_rel_test$rel,
  c_u = c_u,
  c_o = c_o
)
NECRA_phiWeighted <- metricReg_newsvendor(
  pred = pred_phiWeighted$pred,
  truth = y_test,
  w = m_rel_test$rel,
  c_u = c_u,
  c_o = c_o
)
##########


### n 2000 ####

data <-
  f_simulation_reg_nonlinear_Fatih(
    p_imp = 10,
    p_nonImp = 0,
    n = 2000,
    mean_err = 0,
    sd_err = 1,
    imbalanced = TRUE,
    homoscedastic = TRUE
  )
p <- ncol(data) - 1
n <- nrow(data)
x <- data[, 1:p, drop = FALSE]
y <- data[, p + 1]

m_rel <- relevance_PCHIP(y = y, y_new = y)

i_train <- caret::createDataPartition(y = y, times = 1, p = 0.8, list = FALSE)

x_train <- x[i_train,]
y_train <- y[i_train]

x_test <- x[-i_train,]
y_test <- y[-i_train]

m_rel <- relevance_PCHIP(y = y, y_new = y)
m_rel_train <- relevance_PCHIP(y = y, y_new = y_train)
m_rel_test <- relevance_PCHIP(y = y, y_new = y_test)

pred_classical <- f_NNET_newswendor(
  x_train = x_train,
  y_train = y_train,
  x_test = x_test,
  w = NULL, n_epochInitialTraining = 500,
  c_u = c_u,
  c_o = c_o
)

pred_phiWeighted <- f_NNET_newswendor(
  x_train = x_train,
  y_train = y_train,
  x_test = x_test,
  w = m_rel_train$rel, n_epochInitialTraining = 500,
  c_u = c_u,
  c_o = c_o
)

w <- m_rel_test$rel

NECR <- function(pred, truth, w, c_u = 1, c_o = 1, coef = 1, ...) {
  T_E_grid <- seq(0,1, length = 1000)

  NECRs <- sapply(T_E_grid, function(m) {
    pp <- pred[w > m]
    tt <- truth[w > m]

    sum((c_o * pmax(pp - tt, 0)*abs(pp - tt)^coef +
           c_u * pmax(tt - pp, 0)*abs(pp - tt)^coef))
  })

  return(list(
    T_E_grid = T_E_grid,
    NECR = NECRs
  ))
}

NECR_classical <- NECR(
  pred = pred_classical$pred,
  truth = y_test,
  w = m_rel_test$rel,
  c_u = c_u,
  c_o = c_o
)
NECR_phiWeighted <- NECR(
  pred = pred_phiWeighted$pred,
  truth = y_test,
  w = m_rel_test$rel,
  c_u = c_u,
  c_o = c_o
)

colors <- ggsci::pal_d3()(2)

plt_n2000 <- local({
  NECR_classical <- NECR_classical
  NECR_phiWeighted <- NECR_phiWeighted
  NECRA_classical <- NECRA_classical
  NECRA_phiWeighted <- NECRA_phiWeighted

  ggplot() +
    geom_step(
      mapping = aes(x = NECR_classical$T_E_grid, y = NECR_classical$NECR),
      color = colors[1]
    ) +
    geom_step(
      mapping = aes(x = NECR_phiWeighted$T_E_grid, y = NECR_phiWeighted$NECR),
      color = colors[2]
    ) +
    geom_step(mapping = aes(
      x = c(100, 101),
      y = c(100, 101),
      color = factor(c("A", "B"), levels = c("A", "B"))
    )) +
    scale_color_manual(name = "", values = colors, labels = c("CL", "RW")) +
    geom_label(mapping = aes(x = 0.3, y = diff(range(
      NECR_classical$NECR
    )) * 0.1),
    label = TeX(paste0("\\it{$n = $}", n, ", \\it{$c_u =$}", c_u, ", \\it{$c_o =}$", c_o))) +
    # geom_label(mapping = aes(x = 0.2, y = diff(range(
    #   NECR_classical$NECR
    # )) * 0.3),
    # label = TeX(paste0("\\it{$NECRA_{Cl} = $}", formatC(NECRA_classical, digits = 2, format = "f"),
    #                    ", \\it{$NECRA_{\\phi} = $}", formatC(NECRA_phiWeighted, digits = 2, format = "f")))) +
        scale_y_continuous(name = TeX("\\it{$NECR_t$}"), breaks = NULL) +
    scale_x_continuous(name = TeX("\\it{$t$}"), limits = c(0, 1)) +
    theme_bw(base_line_size = 0) +
    theme(legend.position = "top")
})

mean(NECR_classical$T_E_grid*NECR_classical$NECR)
mean(NECR_phiWeighted$T_E_grid*NECR_phiWeighted$NECR)

NECRA_classical <- metricReg_newsvendor(
  pred = pred_classical$pred,
  truth = y_test,
  w = m_rel_test$rel,
  c_u = c_u,
  c_o = c_o
)
NECRA_phiWeighted <- metricReg_newsvendor(
  pred = pred_phiWeighted$pred,
  truth = y_test,
  w = m_rel_test$rel,
  c_u = c_u,
  c_o = c_o
)
##########


plt_list <- list(plt_n250, plt_n750, plt_n2000)


legend <- get_plot_component(plt_list[[1]], "guide-box", return_all = TRUE)
for (i in 1:length(plt_list)) {
  # plt_list[[i]] <- plt_list[[i]] + theme(legend.position = "none") + coord_polar()
  plt_list[[i]] <- plt_list[[i]] + theme(legend.position = "none")
}

plt_together <- plot_grid(
  legend[[4]],
  plot_grid(plotlist = plt_list, nrow = 1, ncol = 3),
  nrow = 2, ncol = 1,
  rel_heights = c(0.1, 0.9)
)
plt_together

ggsave(
  filename = paste0("plt NECR curve together", "c_u", c_u, "c_o", c_o, ".png"),
  plot = plt_together,
  device = "png",
  width = 11,
  height = 3.5,
  dpi = 300
)


##################################################################################


c_u <- 0.1
c_o <- 0.9

### n 250 ####
data <-
  f_simulation_reg_nonlinear_Fatih(
    p_imp = 10,
    p_nonImp = 0,
    n = 250,
    mean_err = 0,
    sd_err = 1,
    imbalanced = TRUE,
    homoscedastic = TRUE
  )
p <- ncol(data) - 1
n <- nrow(data)
x <- data[, 1:p, drop = FALSE]
y <- data[, p + 1]

m_rel <- relevance_PCHIP(y = y, y_new = y)

i_train <- caret::createDataPartition(y = y, times = 1, p = 0.8, list = FALSE)

x_train <- x[i_train,]
y_train <- y[i_train]

x_test <- x[-i_train,]
y_test <- y[-i_train]

m_rel <- relevance_PCHIP(y = y, y_new = y)
m_rel_train <- relevance_PCHIP(y = y, y_new = y_train)
m_rel_test <- relevance_PCHIP(y = y, y_new = y_test)

pred_classical <- f_NNET_newswendor(
  x_train = x_train,
  y_train = y_train,
  x_test = x_test,
  w = NULL, n_epochInitialTraining = 500,
  c_u = c_u,
  c_o = c_o
)

pred_phiWeighted <- f_NNET_newswendor(
  x_train = x_train,
  y_train = y_train,
  x_test = x_test,
  w = m_rel_train$rel, n_epochInitialTraining = 500,
  c_u = c_u,
  c_o = c_o
)

w <- m_rel_test$rel

NECR <- function(pred, truth, w, c_u = 1, c_o = 1, coef = 1, ...) {
  T_E_grid <- seq(0,1, length = 1000)

  NECRs <- sapply(T_E_grid, function(m) {
    pp <- pred[w > m]
    tt <- truth[w > m]

    sum((c_o * pmax(pp - tt, 0)*abs(pp - tt)^coef +
           c_u * pmax(tt - pp, 0)*abs(pp - tt)^coef))
  })

  return(list(
    T_E_grid = T_E_grid,
    NECR = NECRs
  ))
}

NECR_classical <- NECR(
  pred = pred_classical$pred,
  truth = y_test,
  w = m_rel_test$rel,
  c_u = c_u,
  c_o = c_o
)
NECR_phiWeighted <- NECR(
  pred = pred_phiWeighted$pred,
  truth = y_test,
  w = m_rel_test$rel,
  c_u = c_u,
  c_o = c_o
)

colors <- ggsci::pal_d3()(2)

NECRA_classical <- metricReg_newsvendor(
  pred = pred_classical$pred,
  truth = y_test,
  w = m_rel_test$rel,
  c_u = c_u,
  c_o = c_o
)
NECRA_phiWeighted <- metricReg_newsvendor(
  pred = pred_phiWeighted$pred,
  truth = y_test,
  w = m_rel_test$rel,
  c_u = c_u,
  c_o = c_o
)

plt_n250 <- local({
  NECR_classical <- NECR_classical
  NECR_phiWeighted <- NECR_phiWeighted
  NECRA_classical <- NECRA_classical
  NECRA_phiWeighted <- NECRA_phiWeighted

  ggplot() +
    geom_step(
      mapping = aes(x = NECR_classical$T_E_grid, y = NECR_classical$NECR),
      color = colors[1]
    ) +
    geom_step(
      mapping = aes(x = NECR_phiWeighted$T_E_grid, y = NECR_phiWeighted$NECR),
      color = colors[2]
    ) +
    geom_step(mapping = aes(
      x = c(100, 101),
      y = c(100, 101),
      color = factor(c("A", "B"), levels = c("A", "B"))
    )) +
    scale_color_manual(name = "", values = colors, labels = c("CL", "RW")) +
    geom_label(mapping = aes(x = 0.3, y = diff(range(
      NECR_classical$NECR
    )) * 0.1),
    label = TeX(paste0("\\it{$n = $}", n, ", \\it{$c_u =$}", c_u, ", \\it{$c_o =}$", c_o))) +
    # geom_label(mapping = aes(x = 0.2, y = diff(range(
    #   NECR_classical$NECR
    # )) * 0.3),
    # label = TeX(paste0("\\it{$NECRA_{Cl} = $}", formatC(NECRA_classical, digits = 2, format = "f"),
    #                    ", \\it{$NECRA_{\\phi} = $}", formatC(NECRA_phiWeighted, digits = 2, format = "f")))) +
        scale_y_continuous(name = TeX("\\it{$NECR_t$}"), breaks = NULL) +
    scale_x_continuous(name = TeX("\\it{$t$}"), limits = c(0, 1)) +
    theme_bw(base_line_size = 0) +
    theme(legend.position = "top")
})

mean(NECR_classical$T_E_grid*NECR_classical$NECR)
mean(NECR_phiWeighted$T_E_grid*NECR_phiWeighted$NECR)


##########

### n 750 ####

data <-
  f_simulation_reg_nonlinear_Fatih(
    p_imp = 10,
    p_nonImp = 0,
    n = 750,
    mean_err = 0,
    sd_err = 1,
    imbalanced = TRUE,
    homoscedastic = TRUE
  )
p <- ncol(data) - 1
n <- nrow(data)
x <- data[, 1:p, drop = FALSE]
y <- data[, p + 1]

m_rel <- relevance_PCHIP(y = y, y_new = y)

i_train <- caret::createDataPartition(y = y, times = 1, p = 0.8, list = FALSE)

x_train <- x[i_train,]
y_train <- y[i_train]

x_test <- x[-i_train,]
y_test <- y[-i_train]

m_rel <- relevance_PCHIP(y = y, y_new = y)
m_rel_train <- relevance_PCHIP(y = y, y_new = y_train)
m_rel_test <- relevance_PCHIP(y = y, y_new = y_test)

pred_classical <- f_NNET_newswendor(
  x_train = x_train,
  y_train = y_train,
  x_test = x_test,
  w = NULL, n_epochInitialTraining = 500,
  c_u = c_u,
  c_o = c_o
)

pred_phiWeighted <- f_NNET_newswendor(
  x_train = x_train,
  y_train = y_train,
  x_test = x_test,
  w = m_rel_train$rel, n_epochInitialTraining = 500,
  c_u = c_u,
  c_o = c_o
)

w <- m_rel_test$rel

NECR <- function(pred, truth, w, c_u = 1, c_o = 1, coef = 1, ...) {
  T_E_grid <- seq(0,1, length = 1000)

  NECRs <- sapply(T_E_grid, function(m) {
    pp <- pred[w > m]
    tt <- truth[w > m]

    sum((c_o * pmax(pp - tt, 0)*abs(pp - tt)^coef +
           c_u * pmax(tt - pp, 0)*abs(pp - tt)^coef))
  })

  return(list(
    T_E_grid = T_E_grid,
    NECR = NECRs
  ))
}

NECR_classical <- NECR(
  pred = pred_classical$pred,
  truth = y_test,
  w = m_rel_test$rel,
  c_u = c_u,
  c_o = c_o
)
NECR_phiWeighted <- NECR(
  pred = pred_phiWeighted$pred,
  truth = y_test,
  w = m_rel_test$rel,
  c_u = c_u,
  c_o = c_o
)

colors <- ggsci::pal_d3()(2)

plt_n750 <- local({
  NECR_classical <- NECR_classical
  NECR_phiWeighted <- NECR_phiWeighted
  NECRA_classical <- NECRA_classical
  NECRA_phiWeighted <- NECRA_phiWeighted

  ggplot() +
    geom_step(
      mapping = aes(x = NECR_classical$T_E_grid, y = NECR_classical$NECR),
      color = colors[1]
    ) +
    geom_step(
      mapping = aes(x = NECR_phiWeighted$T_E_grid, y = NECR_phiWeighted$NECR),
      color = colors[2]
    ) +
    geom_step(mapping = aes(
      x = c(100, 101),
      y = c(100, 101),
      color = factor(c("A", "B"), levels = c("A", "B"))
    )) +
    scale_color_manual(name = "", values = colors, labels = c("CL", "RW")) +
    geom_label(mapping = aes(x = 0.3, y = diff(range(
      NECR_classical$NECR
    )) * 0.1),
    label = TeX(paste0("\\it{$n = $}", n, ", \\it{$c_u =$}", c_u, ", \\it{$c_o =}$", c_o))) +
    # geom_label(mapping = aes(x = 0.2, y = diff(range(
    #   NECR_classical$NECR
    # )) * 0.3),
    # label = TeX(paste0("\\it{$NECRA_{Cl} = $}", formatC(NECRA_classical, digits = 2, format = "f"),
    #                    ", \\it{$NECRA_{\\phi} = $}", formatC(NECRA_phiWeighted, digits = 2, format = "f")))) +
        scale_y_continuous(name = TeX("\\it{$NECR_t$}"), breaks = NULL) +
    scale_x_continuous(name = TeX("\\it{$t$}"), limits = c(0, 1)) +
    theme_bw(base_line_size = 0) +
    theme(legend.position = "top")
})

mean(NECR_classical$T_E_grid*NECR_classical$NECR)
mean(NECR_phiWeighted$T_E_grid*NECR_phiWeighted$NECR)

NECRA_classical <- metricReg_newsvendor(
  pred = pred_classical$pred,
  truth = y_test,
  w = m_rel_test$rel,
  c_u = c_u,
  c_o = c_o
)
NECRA_phiWeighted <- metricReg_newsvendor(
  pred = pred_phiWeighted$pred,
  truth = y_test,
  w = m_rel_test$rel,
  c_u = c_u,
  c_o = c_o
)
##########


### n 2000 ####

data <-
  f_simulation_reg_nonlinear_Fatih(
    p_imp = 10,
    p_nonImp = 0,
    n = 2000,
    mean_err = 0,
    sd_err = 1,
    imbalanced = TRUE,
    homoscedastic = TRUE
  )
p <- ncol(data) - 1
n <- nrow(data)
x <- data[, 1:p, drop = FALSE]
y <- data[, p + 1]

m_rel <- relevance_PCHIP(y = y, y_new = y)

i_train <- caret::createDataPartition(y = y, times = 1, p = 0.8, list = FALSE)

x_train <- x[i_train,]
y_train <- y[i_train]

x_test <- x[-i_train,]
y_test <- y[-i_train]

m_rel <- relevance_PCHIP(y = y, y_new = y)
m_rel_train <- relevance_PCHIP(y = y, y_new = y_train)
m_rel_test <- relevance_PCHIP(y = y, y_new = y_test)

pred_classical <- f_NNET_newswendor(
  x_train = x_train,
  y_train = y_train,
  x_test = x_test,
  w = NULL, n_epochInitialTraining = 500,
  c_u = c_u,
  c_o = c_o
)

pred_phiWeighted <- f_NNET_newswendor(
  x_train = x_train,
  y_train = y_train,
  x_test = x_test,
  w = m_rel_train$rel, n_epochInitialTraining = 500,
  c_u = c_u,
  c_o = c_o
)

w <- m_rel_test$rel

NECR <- function(pred, truth, w, c_u = 1, c_o = 1, coef = 1, ...) {
  T_E_grid <- seq(0,1, length = 1000)

  NECRs <- sapply(T_E_grid, function(m) {
    pp <- pred[w > m]
    tt <- truth[w > m]

    sum((c_o * pmax(pp - tt, 0)*abs(pp - tt)^coef +
           c_u * pmax(tt - pp, 0)*abs(pp - tt)^coef))
  })

  return(list(
    T_E_grid = T_E_grid,
    NECR = NECRs
  ))
}

NECR_classical <- NECR(
  pred = pred_classical$pred,
  truth = y_test,
  w = m_rel_test$rel,
  c_u = c_u,
  c_o = c_o
)
NECR_phiWeighted <- NECR(
  pred = pred_phiWeighted$pred,
  truth = y_test,
  w = m_rel_test$rel,
  c_u = c_u,
  c_o = c_o
)

colors <- ggsci::pal_d3()(2)

plt_n2000 <- local({
  NECR_classical <- NECR_classical
  NECR_phiWeighted <- NECR_phiWeighted
  NECRA_classical <- NECRA_classical
  NECRA_phiWeighted <- NECRA_phiWeighted

  ggplot() +
    geom_step(
      mapping = aes(x = NECR_classical$T_E_grid, y = NECR_classical$NECR),
      color = colors[1]
    ) +
    geom_step(
      mapping = aes(x = NECR_phiWeighted$T_E_grid, y = NECR_phiWeighted$NECR),
      color = colors[2]
    ) +
    geom_step(mapping = aes(
      x = c(100, 101),
      y = c(100, 101),
      color = factor(c("A", "B"), levels = c("A", "B"))
    )) +
    scale_color_manual(name = "", values = colors, labels = c("CL", "RW")) +
    geom_label(mapping = aes(x = 0.3, y = diff(range(
      NECR_classical$NECR
    )) * 0.1),
    label = TeX(paste0("\\it{$n = $}", n, ", \\it{$c_u =$}", c_u, ", \\it{$c_o =}$", c_o))) +
    # geom_label(mapping = aes(x = 0.2, y = diff(range(
    #   NECR_classical$NECR
    # )) * 0.3),
    # label = TeX(paste0("\\it{$NECRA_{Cl} = $}", formatC(NECRA_classical, digits = 2, format = "f"),
    #                    ", \\it{$NECRA_{\\phi} = $}", formatC(NECRA_phiWeighted, digits = 2, format = "f")))) +
    scale_y_continuous(name = TeX("\\it{$NECR_t$}"), breaks = NULL) +
    scale_x_continuous(name = TeX("\\it{$t$}"), limits = c(0, 1)) +
    theme_bw(base_line_size = 0) +
    theme(legend.position = "top")
})

mean(NECR_classical$T_E_grid*NECR_classical$NECR)
mean(NECR_phiWeighted$T_E_grid*NECR_phiWeighted$NECR)

NECRA_classical <- metricReg_newsvendor(
  pred = pred_classical$pred,
  truth = y_test,
  w = m_rel_test$rel,
  c_u = c_u,
  c_o = c_o
)
NECRA_phiWeighted <- metricReg_newsvendor(
  pred = pred_phiWeighted$pred,
  truth = y_test,
  w = m_rel_test$rel,
  c_u = c_u,
  c_o = c_o
)
##########


plt_list <- list(plt_n250, plt_n750, plt_n2000)


legend <- get_plot_component(plt_list[[1]], "guide-box", return_all = TRUE)
for (i in 1:length(plt_list)) {
  # plt_list[[i]] <- plt_list[[i]] + theme(legend.position = "none") + coord_polar()
  plt_list[[i]] <- plt_list[[i]] + theme(legend.position = "none")
}

plt_together <- plot_grid(
  legend[[4]],
  plot_grid(plotlist = plt_list, nrow = 1, ncol = 3),
  nrow = 2, ncol = 1,
  rel_heights = c(0.1, 0.9)
)
plt_together

ggsave(
  filename = paste0("plt NECR curve together", "c_u", c_u, "c_o", c_o, ".png"),
  plot = plt_together,
  device = "png",
  width = 11,
  height = 3.5,
  dpi = 300
)


library(caret)
torch <- reticulate::import("torch")
source(file = "f_createNNModel.R")
source(file = "f_NNET_newswendor.R")
source(file = "f_simulation_reg_linear.R")
source(file = "f_simulation_reg_nonlinear_Zhu.R")
source(file = "f_simulation_reg_nonlinear_Fatih.R")
source(file = "f_calculatePerformances_regression.R")
library(FatihResearch)
library(ImbRegSamp)
library(parallel)

p_imps <- seq(2, 20, 2) #
# ns <- c(250, 750, 2000) # ok
ns <- seq(250, 2000, 250)
sd_errs <- c(0.25, 0.5, 0.75, 1) # c(1)
n_iter <- 50 # 50
c_ratios <- seq(0.1, 0.9, 0.1) # seq(0.1, 0.9, 0.1)
# c_ratios <- 0.4
k_fold <- 5 # 5
k_rep <- 1 # 1

cl <- makeCluster(10)

trial_grid <- expand.grid(
  p_imp = p_imps,
  n = ns,
  sd_err = sd_errs,
  c_ratio = c_ratios
)

all_results <- matrix(data = NA, nrow = 0, ncol = 6)
colnames(all_results) <- c("p_imp", "n", "sd", "c_ratio", "method", "NECRA")

all_results_classical <- all_results_phiWeighted <- all_results_SMOTER <- all_results_SMOGN <- all_results
NECRA_classical <- NECRA_phiWeighted <- NECRA_SMOTER <- NECRA_SMOGN <- c()

counter <- 1

for (trial in 1:nrow(trial_grid)) {
  tr <- trial_grid[trial, ]
  c_u <- c(tr[, 4])
  c_o <- 1 - c_u

  clusterExport(cl = cl, "f_simulation_reg_nonlinear_Fatih")
  clusterExport(cl = cl, "tr")
  clusterExport(cl = cl, "relevance_PCHIP")
  clusterExport(cl = cl, "f_NNET_newswendor")
  clusterExport(cl = cl, "f_createNNModel")
  clusterExport(cl = cl, "resamplerReg_SMOTER")
  clusterExport(cl = cl, "resamplerReg_SMOGN")
  # clusterExport(cl = cl, "torch")
  clusterExport(cl = cl, "c_u")
  clusterExport(cl = cl, "c_o")
  clusterExport(cl = cl, "metricReg_newsvendor")

  perf_all <- parLapply(cl = cl, X = 1:n_iter, fun = function(i) {
    data <-
      f_simulation_reg_nonlinear_Fatih(
        p_imp = tr[, 1],
        p_nonImp = 0,
        n = tr[, 2],
        mean_err = 0,
        sd_err = tr[, 3],
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
      w = NULL,
      c_u = c_u,
      c_o = c_o
    )

    pred_phiWeighted <- f_NNET_newswendor(
      x_train = x_train,
      y_train = y_train,
      x_test = x_test,
      w = m_rel_train$rel,
      c_u = c_u,
      c_o = c_o
    )
#
#     m_SMOTE <- resamplerReg_SMOTER(x = x_train, y = y_train, phi = m_rel_train$rel)
#     m_SMOGN <- resamplerReg_SMOGN(x = x_train, y = y_train, phi = m_rel_train$rel)
#
#
#     pred_SMOTER <- f_NNET_newswendor(
#       x_train = m_SMOTE$x,
#       y_train = m_SMOTE$y,
#       x_test = x_test,
#       w = NULL,
#       c_u = c_u,
#       c_o = c_o
#     )
#
#     pred_SMOGN <- f_NNET_newswendor(
#       x_train = m_SMOGN$x,
#       y_train = m_SMOGN$y,
#       x_test = x_test,
#       w = NULL,
#       c_u = c_u,
#       c_o = c_o
#     )

    perf_classical <- metricReg_newsvendor(pred = pred_classical$pred, truth = y_test, w = m_rel_test$rel, c_u = c_u, c_o = c_o)
    perf_phiWeighted <- metricReg_newsvendor(pred = pred_phiWeighted$pred, truth = y_test, w = m_rel_test$rel, c_u = c_u, c_o = c_o)
    # perf_SMOTER <- metricReg_newsvendor(pred = pred_SMOTER$pred, truth = y_test, w = m_rel_test$rel, c_u = c_u, c_o = c_o)
    # perf_SMOGN <- metricReg_newsvendor(pred = pred_SMOGN$pred, truth = y_test, w = m_rel_test$rel, c_u = c_u, c_o = c_o)

    cbind(
      tr,
      method = c("classical", "phiWeighted"),
      # NECRA = c(perf_classical, perf_phiWeighted, perf_SMOTER, perf_SMOGN)
      NECRA = c(perf_classical, perf_phiWeighted)
    )
  })

  perf_all <- do.call(rbind, perf_all)

  all_results <- rbind(all_results, perf_all)

  cat("\r", formatC(
    trial / (nrow(trial_grid)) * 100,
    digits = 2,
    format = "f"
  ), "  ")

  write.csv(x = all_results,
            file = "all_results.csv",
            row.names = FALSE)
}

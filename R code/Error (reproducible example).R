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

p_imps <- 10 #
ns <- c(250, 750, 2000) # ok
sd_errs <- c(1) # c(1)
n_iter <- 5 # 50
c_ratios <- seq(0, 1, 0.1) # seq(0.1, 0.9, 0.1)
k_fold <- 5 # 5
k_rep <- 1 # 1

cl <- makeCluster(5)

trial_grid <- expand.grid(
  p_imp = p_imps,
  n = ns,
  sd_err = sd_errs,
  c_ratio = c_ratios
)

all_results <- matrix(data = NA, nrow = 0, ncol = 5)
colnames(all_results) <- c("p_imp", "n", "sd", "c_ratio", "NCRA")

all_results_classical <- all_results_phiWeighted <- all_results_SMOTER <- all_results_SMOGN <- all_results
NCRA_classical <- NCRA_phiWeighted <- NCRA_SMOTER <- NCRA_SMOGN <- c()

counter <- 1

for (trial in 1:nrow(trial_grid)) {
  tr <- trial_grid[trial, ]
  c_u <- c(tr[, 4])
  c_o <- 1 - c_u

  for (i in 1:n_iter) {
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


    i_train_listi_train_list <- caret::createMultiFolds(y = y, k = k_fold, times = k_rep)

    clusterExport(cl = cl, "f_NNET_newswendor")
    clusterExport(cl = cl, "resamplerReg_SMOTER")
    clusterExport(cl = cl, "resamplerReg_SMOGN")
    clusterExport(cl = cl, "resamplerReg_NORES")
    clusterExport(cl = cl, "f_createNNModel")
    clusterExport(cl = cl, "c_u")
    clusterExport(cl = cl, "c_o")
    clusterExport(cl = cl, "metricReg_all")
    clusterExport(cl = cl, "i_train_list")


    perf_classical <- f_calculatePerformances_regression(
      x = x,
      y = y,
      resample = FALSE,
      newsvendor = TRUE,
      sampleWeighted = TRUE,
      w = NULL,
      f_reg = f_NNET_newswendor,
      f_rsmp = resamplerReg_NORES,
      parallel = TRUE,
      cl = cl,
      i_train_list = i_train_list,
      c_u = c_u,
      c_o = c_o
    )

    future::Future(expr = {
      perf_classical <- f_calculatePerformances_regression(
        x = x,
        y = y,
        resample = FALSE,
        newsvendor = TRUE,
        sampleWeighted = TRUE,
        w = NULL,
        f_reg = f_NNET_newswendor,
        f_rsmp = resamplerReg_NORES,
        parallel = TRUE,
        cl = cl,
        i_train_list = i_train_list,
        c_u = c_u,
        c_o = c_o
      )
    })

    perf_phiWeighted <- f_calculatePerformances_regression(
      x = x,
      y = y,
      resample = FALSE,
      newsvendor = TRUE,
      sampleWeighted = TRUE,
      w = m_rel$rel,
      f_reg = f_NNET_newswendor,
      f_rsmp = resamplerReg_NORES,
      parallel = TRUE,
      cl = cl,
      i_train_list = i_train_list,
      c_u = c_u,
      c_o = c_o
    )

    perf_SMOTER <- f_calculatePerformances_regression(
      x = x,
      y = y,
      resample = FALSE,
      newsvendor = TRUE,
      sampleWeighted = TRUE,
      w = NULL,
      f_reg = f_NNET_newswendor,
      f_rsmp = resamplerReg_SMOTER,
      parallel = TRUE,
      cl = cl,
      i_train_list = i_train_list,
      c_u = c_u,
      c_o = c_o
    )

    perf_SMOGN <- f_calculatePerformances_regression(
      x = x,
      y = y,
      resample = FALSE,
      newsvendor = TRUE,
      sampleWeighted = TRUE,
      w = m_rel$rel,
      f_reg = f_NNET_newswendor,
      f_rsmp = resamplerReg_SMOGN,
      parallel = TRUE,
      cl = cl,
      i_train_list = i_train_list,
      c_u = c_u,
      c_o = c_o
    )

    all_results_classical <- rbind(all_results_classical, cbind(tr, NCRA = perf_classical[, "newsvendor"]))
    all_results_phiWeighted <- rbind(all_results_phiWeighted,
                                     cbind(tr, NCRA = perf_phiWeighted[, "newsvendor"]))
    all_results_SMOTER <- rbind(all_results_SMOTER, cbind(tr, NCRA = perf_SMOTER[, "newsvendor"]))
    all_results_SMOGN <- rbind(all_results_SMOGN, cbind(tr, NCRA = perf_SMOGN[, "newsvendor"]))

    cat("\r", formatC(
      counter / (nrow(trial_grid) * n_iter) * 100,
      digits = 2,
      format = "f"
    ), "  ")
    counter <- counter + 1
  }


  write.csv(x = all_results_classical,
            file = "all_results_classical.csv",
            row.names = FALSE)
  write.csv(x = all_results_phiWeighted,
            file = "all_results_phiWeighted.csv",
            row.names = FALSE)
  write.csv(x = all_results_SMOTER,
            file = "all_results_SMOTER.csv",
            row.names = FALSE)
  write.csv(x = all_results_SMOGN,
            file = "all_results_SMOGN.csv",
            row.names = FALSE)
}

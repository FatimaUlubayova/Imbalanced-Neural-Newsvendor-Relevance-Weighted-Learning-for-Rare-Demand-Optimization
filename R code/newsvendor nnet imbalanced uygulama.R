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
p_nonImps <- 0 # ok
ns <- c(250, 750, 2000) # ok
mean_errs <- 0 # 0
sd_errs <- c(1) # c(1)
n_iter <- 5 # 50
c_ratios <- seq(0, 1, 0.1) # seq(0.1, 0.9, 0.1)
t_es <- c(0.5) # c(0.5)
names_resReg <- c("NORES", "SMOTER", "SMOGN") # c("NORES", "SMOTER", "SMOGN")
k_fold <- 5 # 5
k_rep <- 1 # 1

cl <- makeCluster(10)

trial_grid <- expand.grid(
  p_imp = p_imps,
  p_nonImp = p_nonImps,
  n = ns,
  mean_err = mean_errs,
  sd_err = sd_errs,
  c_ratio = c_ratios,
  t_e = t_es,
  resReg = names_resReg,
  methods = c("classical", "phiWeighted")
)

trial_grid <- trial_grid[!(trial_grid$resReg != "NORES" & trial_grid$methods == "phiWeighted"),]

all_results <- matrix(data = NA, nrow = nrow(trial_grid), ncol = 9)
colnames(all_results) <- c("Mean", "SE", "min", "max", "median", "q0.25", "q0.75", "q0.025", "q0.975")

counter <- 1

for (i in 1:nrow(trial_grid)) {
  tr <- trial_grid[i,]
  c_u <- c(tr[,6])
  c_o <- 1 - c_u

  t_e <- c(tr[,7])
  f_rsmp <- getFunction(paste0("resamplerReg_", as.character(tr[,8])))
  f_reg <- f_NNET_newswendor

  NCRA_mean_all <- c()

  for (iter in 1:n_iter) {
    data <-
      f_simulation_reg_nonlinear_Fatih(
        p_imp = tr[, 1],
        p_nonImp = tr[, 2],
        n = tr[, 3],
        mean_err = tr[, 4],
        sd_err = tr[, 5],
        imbalanced = TRUE,
        homoscedastic = TRUE
      )
    p <- ncol(data) - 1
    n <- nrow(data)
    x <- data[, 1:p, drop = FALSE]
    y <- data[, p + 1]

    i_train_list <- caret::createMultiFolds(y = y, k = k_fold, times = k_rep)
    # m_rel <- relevance_density(y = y)
    m_rel <- relevance_PCHIP(y = y)
    # m_rel <- relevance_sigmoid(y = y, k = 0.5, delta = 0.1)

    if (tr[,9] == "phiWeighted") {
      w <- m_rel$rel
    } else {
      w <- NULL
    }
    doResample <- ifelse(tr[,8] == "NORES", FALSE, TRUE)

    clusterExport(cl = cl, "f_rsmp")
    clusterExport(cl = cl, "f_reg")
    clusterExport(cl = cl, "c_u")
    clusterExport(cl = cl, "c_o")
    clusterExport(cl = cl, "doResample")
    clusterExport(cl = cl, "w")
    clusterExport(cl = cl, "metricReg_all")
    clusterExport(cl = cl, "f_NNET_newswendor")
    clusterExport(cl = cl, "f_createNNModel")
    clusterExport(cl = cl, "t_e")
    clusterExport(cl = cl, "resamplerReg_SMOTER")
    clusterExport(cl = cl, "resamplerReg_SMOGN")

    perf <- f_calculatePerformances_regression(
      x = x,
      y = y,
      resample = doResample,
      newsvendor = TRUE,
      sampleWeighted = TRUE,
      w = w,
      # w = m_rel$rel,
      thresh_rel = t_e,
      f_reg = f_reg,
      f_rsmp = f_rsmp,
      parallel = TRUE,
      cl = cl,
      i_train_list = i_train_list,
      # i_train_list = FALSE,
      c_u = c_u,
      c_o = c_o
    )
    cat("\r", formatC(counter / (nrow(trial_grid)*n_iter)*100, digits = 2, format = "f"), "  ")
    NCRA_mean_all[iter] <- mean(perf[,"newsvendor"])
    counter <- counter + 1
  }

  all_results[i,1] <- mean(NCRA_mean_all)
  all_results[i,2] <- sd(NCRA_mean_all)
  all_results[i,3] <- min(NCRA_mean_all)
  all_results[i,4] <- max(NCRA_mean_all)
  all_results[i,5] <- quantile(NCRA_mean_all, 0.5)
  all_results[i,6] <- quantile(NCRA_mean_all, 0.25)
  all_results[i,7] <- quantile(NCRA_mean_all, 0.75)
  all_results[i,8] <- quantile(NCRA_mean_all, 0.025)
  all_results[i,9] <- quantile(NCRA_mean_all, 0.975)

  # cat(paste0(paste0(names(tr), " = ", unlist(tr))), "\n", sep = "|")
  # cat(paste0("Mean NCRA = ", round(all_results[i,1], 2)), "|",
  #     paste0("SE NCRA = ", round(all_results[i,2], 2)), "\n")

  write.csv(x = cbind(trial_grid, all_results), file = "all results.csv", row.names = FALSE)

}

all_all_results <- cbind(trial_grid, all_results)
all_all_results <- all_all_results[complete.cases(all_all_results),]


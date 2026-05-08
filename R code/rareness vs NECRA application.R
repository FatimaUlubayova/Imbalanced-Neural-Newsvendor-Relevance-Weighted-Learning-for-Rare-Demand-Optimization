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

c_u <- 0.2
c_o <- 0.8

k_fold <- 5
k_rep <- 1

n_iter <- 10
coef_rareness <- seq(0, 1, 0.1)

M_NECRA <- matrix(data = NA, nrow = length(coef_rareness)*n_iter, ncol = 4)
colnames(M_NECRA) <- c("NECRA_CL", "NECRA_PW", "iter", "rareness")
M_NECRA <- as.data.frame(M_NECRA)
M_NECRA$iter <- rep(1:n_iter, each = length(coef_rareness))
M_NECRA$rareness <- coef_rareness

for (iter in 1:n_iter) {
  data <-
    f_simulation_reg_nonlinear_Fatih(
      p_imp = 10,
      n = 1000,
      sd_err = 1,
      imbalanced = TRUE,
      p_nonImp = 0
    )

  p <- ncol(data) - 1
  x <- data[,1:p]
  y <- data[,p + 1]


  diff_upper <- max(y) - median(y)
  diff_lower <- median(y) - min(y)

  NECRA_CL_all <- c()
  NECRA_PW_all <- c()

  for (cr in coef_rareness) {
    S_points <- c(
      median(y) - diff_lower*cr,
      median(y),
      median(y) + diff_upper*cr
    )
    m_rel <- relevance_PCHIP(y = y, y_new = y, S_points = S_points)

    i_train_list <- createMultiFolds(y = y, k = k_fold, times = k_rep)

    NECRA_CL_cv <- c()
    NECRA_PW_cv <- c()

    for (i in 1:length(i_train_list)) {
      i_train <- i_train_list[[i]]

      x_train <- x[i_train,,drop = FALSE]
      y_train <- y[i_train]

      x_test <- x[-i_train,, drop = FALSE]
      y_test <- y[-i_train]

      w_train <- m_rel$rel[i_train]
      w_test <- m_rel$rel[-i_train]

      m_CL <- f_NNET_newswendor(
        x_train = x_train,
        y_train = y_train,
        x_test = x_test,
        w = NULL,
        c_u = c_u,
        c_o = c_o,
        S_points = S_points
      )

      m_PW <- f_NNET_newswendor(
        x_train = x_train,
        y_train = y_train,
        x_test = x_test,
        w = w_train,
        c_u = c_u,
        c_o = c_o,
        S_points = S_points
      )

      NECRA_CL_cv[i] <- metricReg_newsvendor(
        pred = m_CL$pred,
        truth = y_test,
        w = w_test + 1e-6,
        c_u = c_u,
        c_o = c_o
      )

      NECRA_PW_cv[i] <- metricReg_newsvendor(
        pred = m_PW$pred,
        truth = y_test,
        w = w_test + 1e-6,
        c_u = c_u,
        c_o = c_o
      )
    }

    NECRA_CL_all <- c(NECRA_CL_all, mean(NECRA_CL_cv))
    NECRA_PW_all <- c(NECRA_PW_all, mean(NECRA_PW_cv))
  }

  M_NECRA$NECRA_CL[M_NECRA$iter == iter] <- NECRA_CL_all
  M_NECRA$NECRA_PW[M_NECRA$iter == iter] <- NECRA_PW_all

  plot(coef_rareness, NECRA_CL_all, type = "l", col = "red")
  lines(coef_rareness, NECRA_PW_all, col = "blue")
  points(coef_rareness, NECRA_CL_all, col = "red")
  points(coef_rareness, NECRA_PW_all, col = "blue")

  print(iter)
}


write.csv(x = M_NECRA, file = "rareness vs NECRA.csv", row.names = FALSE)

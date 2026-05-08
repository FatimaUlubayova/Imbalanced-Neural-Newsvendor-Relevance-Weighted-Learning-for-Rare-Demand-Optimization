f_simulation_reg_nonlinear_Fatih <-
  function(p_imp,
           p_nonImp = 0,
           n,
           mean_err = 0,
           sd_err = 1,
           imbalanced = FALSE,
           homoscedastic = TRUE) {
    x_imp <- matrix(data = rnorm(p_imp * n),
                    nrow = n,
                    ncol = p_imp)
    x_nonImp <-
      matrix(data = rnorm(p_nonImp * n),
             nrow = n,
             ncol = p_nonImp)
    x <- cbind(x_imp, x_nonImp)

    Beta <- c(5, rep(2, p_imp), rep(0, p_nonImp))
    err <- rnorm(n, mean = mean_err, sd = sd_err)

    if (imbalanced) {
      BetaTx <- cbind(1, x^3) %*% Beta
      y <- BetaTx + err
    } else {
      BetaTx <- cbind(1, x) %*% Beta
      y <- BetaTx + err
    }

    return(cbind(x, y))
  }



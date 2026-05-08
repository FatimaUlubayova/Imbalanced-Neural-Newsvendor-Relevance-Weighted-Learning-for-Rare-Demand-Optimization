f_simulation_reg_nonlinear_Zhu <-
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

    Beta <- c(runif(1, 1, 2), runif(p_imp, 1, 4), rep(0, p_nonImp))
    err <- rnorm(n, mean = mean_err, sd = sd_err)

    BetaTx <- cbind(1, x) %*% Beta

    if (imbalanced) {
      y <-
        10 + sin(2 * BetaTx) + 2 * exp(-16 * BetaTx ^ 2) + if (homoscedastic)
          err
      else
        exp(BetaTx) * err
      mean_y <- mean(y)
      sd_y <- sd(y)

      q4 <- quantile(y, 0.75)
      y[y > q4] <-
        y[y > q4] + sort(rnorm(sum(y > q4), mean = mean_y / sqrt(10), sd = sd_y)) + rnorm(sum(y > q4))

      q2 <- quantile(y, 0.25)
      y[y < q2] <-
        y[y < q2] - sort(rnorm(sum(y < q2), mean = mean_y / sqrt(10), sd = sd_y)) + rnorm(sum(y < q2))
    } else {
      y <-
        10 + sin(2 * BetaTx) + 2 * exp(-16 * BetaTx ^ 2) + if (homoscedastic)
          err
      else
        exp(BetaTx) * err
    }

    return(cbind(x, y))
  }

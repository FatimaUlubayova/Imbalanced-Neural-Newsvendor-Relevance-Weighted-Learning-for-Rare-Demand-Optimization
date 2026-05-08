f_simulation_reg_linear <-
  function(p_imp,
           p_nonImp = 0,
           n,
           mean_err = 0,
           sd_err = 1,
           imbalanced = FALSE,
           homoscedastic = TRUE) {
    x_imp <- matrix(data = rnorm(p_imp*n), nrow = n, ncol = p_imp)
    x_nonImp <- matrix(data = rnorm(p_nonImp*n), nrow = n, ncol = p_nonImp)
    x <- cbind(x_imp, x_nonImp)

    Beta <- c(5, rep(2, p_imp), rep(0, p_nonImp))
    err <- rnorm(n, mean = mean_err, sd = sd_err)

    y <- cbind(1,x)%*%Beta + err
    if (imbalanced) {
      y <- cbind(1,x)^2%*%Beta + err

      mean_y <- mean(y)
      sd_y <- sd(y)

      q4 <- quantile(y, 0.75)
      y[y > q4] <- y[y > q4] + sort(rnorm(sum(y > q4), mean = mean_y/sqrt(10), sd = sd_y)) + rnorm(sum(y > q4))

      q2 <- quantile(y, 0.25)
      y[y < q2] <- y[y < q2] - sort(rnorm(sum(y > q4), mean = mean_y/sqrt(10), sd = sd_y)) + rnorm(sum(y > q4))
    }

    return(cbind(x, y))
  }

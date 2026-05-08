f_NNET_newswendor <- function(
    x_train,
    y_train,
    x_test,
    w = NULL,
    c_u = 1,
    c_o = 1,
    n_draw = 100,
    n_node = 20L,
    n_hidden = 2L,
    dropout = 0.25,
    BayesLinear = FALSE,
    n_epochInitialTraining = 100L,
    n_patience = 20L,
    n_ma = 5,
    n_epochMax = 1000L,
    S_points = NULL,
    S_phi = NULL,
    S_der = NULL,
    verbose = FALSE,
    ...
) {

  n <- length(y_train)
  n_test <- nrow(x_test)
  p <- ncol(x_train)
  y_train <- as.double(y_train)

  ### tensor variables for torch
  x_train_tensor <- torch$from_numpy(as.matrix(x_train))$float()
  y_train_tensor <- torch$from_numpy(array(y_train))

  x_test_tensor <- torch$from_numpy(as.matrix(x_test))$float()

  ### pre training
  counter_epoch <- 1
  m_nnet <-
    f_createNNModel(
      n_node = n_node,
      n_hidden = n_hidden,
      dropout = dropout,
      p = p,
      classification = FALSE
    )

  ### loss function
  # regressor_loss <- torch$nn$MSELoss()
  c_u_tensor <- torch$as_tensor(c_u)
  c_o_tensor <- torch$as_tensor(c_o)

  if (is.null(w)) {
    w <- rep(1, n)
  } else {
    if (length(w) != n) {
      stop("length of w must be equal to y's")
    }
  }
  w_tensor <- torch$as_tensor(w + 1e-6)

  regressor_loss <- function(output, target) {
    torch$sum(
      (
        c_o_tensor * torch$clamp_min(output - target, 0) * torch$abs(output - target) +
          c_u_tensor * torch$clamp_min(target - output, 0) * torch$abs(output - target)
      ) * w_tensor
    )
  }

  ### optimization function
  # regressor_optimizer <- torch$optim$RMSprop(m_nnet$parameters())
  # regressor_optimizer <- torch$optim$SGD(m_nnet$parameters(), lr = 0.01)
  regressor_optimizer <- torch$optim$Adam(m_nnet$parameters())

  averageLosses <- c()
  repeat {
    predStep <- m_nnet(x_train_tensor$double())

    lossStep <- regressor_loss(predStep$reshape(n), y_train_tensor)
    averageLosses[counter_epoch] <-
      lossStep$detach()$numpy()

    regressor_optimizer$zero_grad()
    lossStep$backward()
    regressor_optimizer$step()

    if (counter_epoch > n_epochInitialTraining + n_ma) {
      break
    }
    counter_epoch <- counter_epoch + 1
  }

  ### train Bayes deep learning
  repeat {
    predStep <- m_nnet(x_train_tensor$double())
    lossStep <- regressor_loss(predStep$reshape(n), y_train_tensor)
    averageLosses[counter_epoch] <-
      lossStep$detach()$numpy()

    # plot(averageLosses)

    LossMA_now <-
      mean(averageLosses[seq(counter_epoch, counter_epoch - n_ma + 1)])
    LossMA_early <-
      mean(averageLosses[seq(counter_epoch - n_patience,
                             counter_epoch - n_patience - n_ma + 1)])

    if (verbose & (counter_epoch %% 10) == 0) {
      cat("epcoh = ", counter_epoch, " | LossMA_now = ", round(LossMA_now, 5), " | LossMA_early = ", round(LossMA_early, 5), "\n", sep = "")
    }
    if (LossMA_now > LossMA_early) {
      # cat("finito\n")
      break
    }

    if (n_epochMax == counter_epoch) {
      # cat("finito\n")
      break
    }

    counter_epoch <- counter_epoch + 1

    regressor_optimizer$zero_grad()
    lossStep$backward()
    regressor_optimizer$step()
  }

  if (dropout > 0) {
    ### probability draw matrix
    prob_M <- matrix(data = NA,
                     nrow = n_draw,
                     ncol = n_test)

    ### draw probabilities from posterior
    for (i in 1:n_draw) {
      prob_M[i,] <- c(m_nnet(x_test_tensor$double())$detach()$numpy())
    }

    ### model averaging
    pred <- apply(prob_M, 2, mean)
    se <- apply(prob_M, 2, sd)
    pred_lower <- apply(prob_M, 2, function(m) quantile(m, 0.025))
    pred_upper <- apply(prob_M, 2, function(m) quantile(m, 0.975))

    results <- list(
      pred = pred,
      pred_lower = pred_lower,
      pred_upper = pred_upper,
      se = se
    )
  } else {
    pred <- m_nnet(x_test_tensor$double())$detach()$numpy()

    results <- list(
      pred = pred
    )
  }
}

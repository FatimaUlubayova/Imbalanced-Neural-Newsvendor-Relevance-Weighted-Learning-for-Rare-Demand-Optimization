#' @title  Calculates performance for regression
#'
#' @description Calculates performance for regression
#'
#' @param x feature matrix.
#' @param y a factor class variable.
#' @param k_number k for k-fold cross validation.
#' @param k_repeats number of repeats for k-fold cross validation.
#' @param f_reg regressor
#' @param f_rsmp resampling function
#' @param i_train_list indexes of training samples for each fold of repeated k-fold cross validation.
#' @param parallel should parallel processing be used? TRUE or FALSE. Default TRUE
#' @param cl parallel package cluster from makeCluster().
#' @param ... asd
#'
#' @details
#' Calculates performance scores for regression using k-fold cross validation with given regressor and resampling method.
#'
#' @return asd.
#'
#' @author Fatih Saglam, saglamf89@gmail.com
#'
#' @examples
#' rnorm(1)
#'
#' @importFrom caret createMultiFolds
#'
#' @rdname f_calculatePerformances_regression
#' @export

f_calculatePerformances_regression <-
  function(x = x,
           y = y,
           k_number = 10,
           k_repeats = 1,
           resample = TRUE,
           newsvendor = FALSE,
           sampleWeighted = FALSE,
           w = NULL,
           thresh_rel = 0.5,
           f_reg,
           f_rsmp,
           i_train_list = NULL,
           parallel = TRUE,
           cl = NULL,
           c_u = 1,
           c_o = 1,
           verbose = FALSE,
           S_points = NULL,
           S_phi = NULL,
           S_der = NULL,
           ...) {
    set.seed(1)
    if (is.null(i_train_list)) {
      i_train_list <-
        createMultiFolds(y = y, k = k_number, times = k_repeats)
    }

    wNULL <- is.null(w)

    perf_table <-
      matrix(
        data = NA,
        nrow = length(i_train_list),
        ncol = length(metricReg_all)
      )
    colnames(perf_table) <- names(metricReg_all)

    t_start <- Sys.time()

    if (parallel) {
      parallel::clusterExport(cl = cl, "x")
      parallel::clusterExport(cl = cl, "y")

      perf_table_t <-
        parSapply(
          cl = cl,
          X = 1:length(i_train_list),
          FUN = function(i) {
            x_train <- as.matrix(x[i_train_list[[i]], ])
            y_train <- y[i_train_list[[i]]]

            x_test <- as.matrix(x[-i_train_list[[i]], ])
            y_test <- y[-i_train_list[[i]]]

            if (sampleWeighted) {
              if (wNULL) {
                w <- rep(1 / length(y), length(y))
              }

              w_train <- w[i_train_list[[i]]]
              w_test <- w[-i_train_list[[i]]]
            }

            means <- apply(x_train, 2, mean)
            sds <- apply(x_train, 2, sd) + 1e-6

            x_train <- scale(x = x_train,
                             center = means,
                             scale = sds)
            x_test <- scale(x = x_test,
                            center = means,
                            scale = sds)

            if (resample) {
              if (wNULL) {
                w_train <- ImbRegSamp::relevance_PCHIP(
                  y = y,
                  y_new = y_train,
                  S_points = S_points,
                  S_phi = S_phi,
                  S_der = S_der
                )$rel
              }

              m_rsm <-
                f_rsmp(
                  x = x_train,
                  y = y_train,
                  thresh_rel = thresh_rel,
                  phi = w_train + 1e-5,
                  ...
                )

              x_train <- m_rsm$x
              y_train <- m_rsm$y
            }

            ### training ####
            if (newsvendor) {
              if (sampleWeighted) {
                if (resample) {
                  w_train <- rep(1 / length(y_train), length(y_train))
                }

                # cat("n_w: ", length(w_train), "| n: ", length(y_train), "\n")

                m_results <-
                  f_reg(
                    x_train = x_train,
                    y_train = y_train,
                    x_test = x_test,
                    w = w_train,
                    c_u = c_u,
                    c_o = c_o
                  )
              } else {
                m_results <-
                  f_reg(
                    x_train = x_train,
                    y_train = y_train,
                    x_test = x_test,
                    c_u = c_u,
                    c_o = c_o
                  )
              }
            } else {
              if (sampleWeighted) {
                if (resample) {
                  w_train <- rep(1 / length(y_train), length(y_train))
                }

                m_results <-
                  f_reg(
                    x_train = x_train,
                    y_train = y_train,
                    x_test = x_test,
                    w = w_train
                  )
              } else {
                m_results <-
                  f_reg(x_train = x_train,
                        y_train = y_train,
                        x_test = x_test)
              }
            }

            pred <- m_results$pred
            ##################

            ### evaluation ####
            if (newsvendor) {
              if (sampleWeighted) {
                if (wNULL) {
                  w_test <- ImbRegSamp::relevance_PCHIP(
                    y = y,
                    y_new = y_test,
                    S_points = S_points,
                    S_phi = S_phi,
                    S_der = S_der
                  )$rel
                }

                if (resample) {
                  sapply(metricReg_all, function(m) {
                    m(
                      truth = y_test,
                      pred = pred,
                      w = w_test,
                      thresh = thresh_rel,
                      y_train = y,
                      c_u = c_u,
                      c_o = c_o
                    )
                  })
                } else {
                  sapply(metricReg_all, function(m) {
                    m(
                      truth = y_test,
                      pred = pred,
                      w = w_test,
                      y_train = y,
                      c_u = c_u,
                      c_o = c_o
                    )
                  })
                }
              } else {
                if (resample) {
                  if (wNULL) {
                    w_test <- ImbRegSamp::relevance_PCHIP(
                      y = y,
                      y_new = y_test,
                      S_points = S_points,
                      S_phi = S_phi,
                      S_der = S_der
                    )$rel
                  }
                  sapply(metricReg_all, function(m) {
                    m(
                      truth = y_test,
                      pred = pred,
                      w = w_test,
                      thresh = thresh_rel,
                      y_train = y,
                      c_u = c_u,
                      c_o = c_o
                    )
                  })
                } else {
                  sapply(metricReg_all, function(m) {
                    m(
                      truth = y_test,
                      pred = pred,
                      y_train = y,
                      c_u = c_u,
                      c_o = c_o
                    )
                  })
                }
              }
            } else {
              if (sampleWeighted) {
                if (resample) {
                  if (wNULL) {
                    w_test <- ImbRegSamp::relevance_PCHIP(
                      y = y,
                      y_new = y_test,
                      S_points = S_points,
                      S_phi = S_phi,
                      S_der = S_der
                    )$rel
                  }
                  sapply(metricReg_all, function(m) {
                    m(
                      truth = y_test,
                      pred = pred,
                      w = w_test,
                      thresh = thresh_rel,
                      y_train = y
                    )
                  })
                } else {
                  sapply(metricReg_all, function(m) {
                    m(
                      truth = y_test,
                      pred = pred,
                      w = w_test,
                      y_train = y
                    )
                  })
                }
              } else {
                if (resample) {
                  if (wNULL) {
                    w_test <- ImbRegSamp::relevance_PCHIP(
                      y = y,
                      y_new = y_test,
                      S_points = S_points,
                      S_phi = S_phi,
                      S_der = S_der
                    )$rel
                  }
                  sapply(metricReg_all, function(m) {
                    m(
                      truth = y_test,
                      pred = pred,
                      w = w_test,
                      thresh = thresh_rel,
                      y_train = y
                    )
                  })
                } else {
                  sapply(metricReg_all, function(m) {
                    m(truth = y_test,
                      pred = pred,
                      y_train = y)
                  })
                }
              }
            }
            ###################
          }
        )

      perf_table <- t(perf_table_t)
    } else {
      for (i in 1:length(i_train_list)) {
        x_train <- as.matrix(x[i_train_list[[i]], ])
        y_train <- y[i_train_list[[i]]]

        x_test <- as.matrix(x[-i_train_list[[i]], ])
        y_test <- y[-i_train_list[[i]]]

        if (sampleWeighted) {
          if (wNULL) {
            w <- rep(1 / length(y), length(y))
          }

          w_train <- w[i_train_list[[i]]]
          w_test <- w[-i_train_list[[i]]]
        }

        means <- apply(x_train, 2, mean)
        sds <- apply(x_train, 2, sd) + 1e-6

        x_train <- scale(x = x_train,
                         center = means,
                         scale = sds)
        x_test <- scale(x = x_test,
                        center = means,
                        scale = sds)

        if (resample) {
          if (wNULL) {
            w_train <- ImbRegSamp::relevance_PCHIP(
              y = y,
              y_new = y_train,
              S_points = S_points,
              S_phi = S_phi,
              S_der = S_der
            )$rel
          }

          m_rsm <-
            f_rsmp(
              x = x_train,
              y = y_train,
              thresh_rel = thresh_rel,
              phi = w_train + 1e-5,
              ...
            )

          x_train <- m_rsm$x
          y_train <- m_rsm$y
        }

        ### training ####
        if (newsvendor) {
          if (sampleWeighted) {
            if (resample) {
              w_train <- rep(1 / length(y_train), length(y_train))
            }

            # cat("n_w: ", length(w_train), "| n: ", length(y_train), "\n")

            m_results <-
              f_reg(
                x_train = x_train,
                y_train = y_train,
                x_test = x_test,
                w = w_train,
                c_u = c_u,
                c_o = c_o
              )
          } else {
            m_results <-
              f_reg(
                x_train = x_train,
                y_train = y_train,
                x_test = x_test,
                c_u = c_u,
                c_o = c_o
              )
          }
        } else {
          if (sampleWeighted) {
            if (resample) {
              w_train <- rep(1 / length(y_train), length(y_train))
            }

            m_results <-
              f_reg(
                x_train = x_train,
                y_train = y_train,
                x_test = x_test,
                w = w_train
              )
          } else {
            m_results <-
              f_reg(x_train = x_train,
                    y_train = y_train,
                    x_test = x_test)
          }
        }

        pred <- m_results$pred
        ##################

        ### evaluation ####
        if (newsvendor) {
          if (sampleWeighted) {
            if (wNULL) {
              w_test <- ImbRegSamp::relevance_PCHIP(
                y = y,
                y_new = y_test,
                S_points = S_points,
                S_phi = S_phi,
                S_der = S_der
              )$rel
            }

            if (resample) {
              perf_table[i, ] <- sapply(metricReg_all, function(m) {
                m(
                  truth = y_test,
                  pred = pred,
                  w = w_test,
                  thresh = thresh_rel,
                  y_train = y,
                  c_u = c_u,
                  c_o = c_o
                )
              })
            } else {
              perf_table[i, ] <- sapply(metricReg_all, function(m) {
                m(
                  truth = y_test,
                  pred = pred,
                  w = w_test,
                  y_train = y,
                  c_u = c_u,
                  c_o = c_o
                )
              })
            }
          } else {
            if (resample) {
              if (wNULL) {
                w_test <- ImbRegSamp::relevance_PCHIP(
                  y = y,
                  y_new = y_test,
                  S_points = S_points,
                  S_phi = S_phi,
                  S_der = S_der
                )$rel
              }
              perf_table[i, ] <- sapply(metricReg_all, function(m) {
                m(
                  truth = y_test,
                  pred = pred,
                  w = w_test,
                  thresh = thresh_rel,
                  y_train = y,
                  c_u = c_u,
                  c_o = c_o
                )
              })
            } else {
              perf_table[i, ] <- sapply(metricReg_all, function(m) {
                m(
                  truth = y_test,
                  pred = pred,
                  y_train = y,
                  c_u = c_u,
                  c_o = c_o
                )
              })
            }
          }
        } else {
          if (sampleWeighted) {
            if (resample) {
              if (wNULL) {
                w_test <- ImbRegSamp::relevance_PCHIP(
                  y = y,
                  y_new = y_test,
                  S_points = S_points,
                  S_phi = S_phi,
                  S_der = S_der
                )$rel
              }
              perf_table[i, ] <- sapply(metricReg_all, function(m) {
                m(
                  truth = y_test,
                  pred = pred,
                  w = w_test,
                  thresh = thresh_rel,
                  y_train = y
                )
              })
            } else {
              perf_table[i, ] <- sapply(metricReg_all, function(m) {
                m(
                  truth = y_test,
                  pred = pred,
                  w = w_test,
                  y_train = y
                )
              })
            }
          } else {
            if (resample) {
              if (wNULL) {
                w_test <- ImbRegSamp::relevance_PCHIP(
                  y = y,
                  y_new = y_test,
                  S_points = S_points,
                  S_phi = S_phi,
                  S_der = S_der
                )$rel
              }
              perf_table[i, ] <- sapply(metricReg_all, function(m) {
                m(
                  truth = y_test,
                  pred = pred,
                  w = w_test,
                  thresh = thresh_rel,
                  y_train = y
                )
              })
            } else {
              perf_table[i, ] <- sapply(metricReg_all, function(m) {
                m(truth = y_test,
                  pred = pred,
                  y_train = y)
              })
            }
          }
        }
        ###################
      }
    }

    t_end <- Sys.time()
    if (verbose) {
      cat("Time difference of ", t_end - t_start, " secs", sep = "")
    }

    return(perf_table)
  }

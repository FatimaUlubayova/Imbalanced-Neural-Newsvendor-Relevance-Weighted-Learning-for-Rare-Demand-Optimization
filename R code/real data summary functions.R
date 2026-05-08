# ------------------------------------------------------------
# ASSUMED PRESENT (DO NOT CHANGE)
# ------------------------------------------------------------
metricReg_NECRA <- function(truth, pred, w, c_u = 1, c_o = 1, coef = 1, ...) {
  sum((c_o * pmax(pred - truth, 0) * abs(pred - truth)^coef +
         c_u * pmax(truth - pred, 0) * abs(pred - truth)^coef) * w)
}

metricReg_NECR <- function(truth, pred, w, alpha, c_u = 1, c_o = 1, coef = 1, ...) {
  ttruth <- truth[w >= alpha]
  ppred <- pred[w >= alpha]
  sum((c_o * pmax(ppred - ttruth, 0) * abs(ppred - ttruth)^coef +
         c_u * pmax(ttruth - ppred, 0) * abs(ppred - ttruth)^coef))
}

# ------------------------------------------------------------
# Helper: fixed (c_u, c_o) economic decomposition in the same
#          UNITS as your NECRA (weighted sums; no normalization)
# ------------------------------------------------------------
.econ_decompose_fixed <- function(truth, pred, w, c_u, c_o, coef) {
  over  <- pmax(pred - truth, 0)
  under <- pmax(truth - pred, 0)
  mag   <- abs(pred - truth)^coef
  W     <- sum(w)

  OverCostSum   <- sum((c_o * over  * mag) * w)
  UnderCostSum  <- sum((c_u * under * mag) * w)
  TotalCostSum  <- OverCostSum + UnderCostSum

  UnderRate     <- sum((under > 0) * w) / W
  OverRate      <- sum((over  > 0) * w) / W
  ServiceLevel  <- 1 - UnderRate
  AvgUnderQty   <- if (sum((under > 0) * w) > 0) sum(under * w) / sum((under > 0) * w) else 0
  AvgOverQty    <- if (sum((over  > 0) * w) > 0) sum(over  * w) / sum((over  > 0) * w) else 0

  data.frame(
    TotalCostSum, OverCostSum, UnderCostSum,
    UnderRate, OverRate, ServiceLevel,
    AvgUnderQty, AvgOverQty
  )
}

# ------------------------------------------------------------
# ONE MODEL: NECR(α) curve via YOUR metricReg_NECR,
#            NECRA scalar via YOUR metricReg_NECRA,
#            plus interpretable economic decomposition.
# ------------------------------------------------------------
newsvendor_econ_one <- function(
    truth, pred, w,
    c_u, c_o, coef = 1,
    alpha_grid = seq(0, 0.99, length.out = 100),
    alpha_eval = 0,
    include_baseline = TRUE,
    include_curve = TRUE
) {
  stopifnot(length(truth) == length(pred), length(pred) == length(w))


  if (include_curve) {
    # NECR(α) curve and α-specific point (both via YOUR NECR)
    necr_curve <- data.frame(
      alpha = alpha_grid,
      NECR  = sapply(alpha_grid, function(a)
        metricReg_NECR(truth, pred, w, alpha = a, c_u = c_u, c_o = c_o, coef = coef))
    )
  } else {
    necr_curve <- data.frame()
  }


  NECR_alpha <- metricReg_NECR(truth, pred, w, alpha = alpha_eval, c_u = c_u, c_o = c_o, coef = coef)

  # NECRA scalar via YOUR NECRA (no numerical integration needed)
  NECRA <- metricReg_NECRA(truth, pred, w, c_u = c_u, c_o = c_o, coef = coef)

  # Economic decomposition at fixed (c_u, c_o) (weighted sums = same units as NECRA)
  dec <- .econ_decompose_fixed(truth, pred, w, c_u, c_o, coef)

  # Optional baseline (mean predictor) for reductions in same units
  if (include_baseline) {
    base_pred <- rep(mean(truth), length(truth))
    NECRA_base      <- metricReg_NECRA(truth, base_pred, w, c_u = c_u, c_o = c_o, coef = coef)
    NECR_alpha_base <- metricReg_NECR(truth, base_pred, w, alpha = alpha_eval, c_u = c_u, c_o = c_o, coef = coef)

    NECRA_Reduction     <- NECRA_base - NECRA
    NECRA_ReductionPct  <- if (NECRA_base != 0) 100 * NECRA_Reduction / NECRA_base else NA_real_

    NECRa_Reduction     <- NECR_alpha_base - NECR_alpha
    NECRa_ReductionPct  <- if (NECR_alpha_base != 0) 100 * NECRa_Reduction / NECR_alpha_base else NA_real_
  } else {
    NECRA_base <- NECR_alpha_base <- NECRA_Reduction <- NECRA_ReductionPct <- NA_real_
    NECRa_Reduction <- NECRa_ReductionPct <- NA_real_
  }

  summary <- cbind(
    data.frame(c_u = c_u, c_o = c_o, coef = coef, alpha_eval = alpha_eval,
               NECR_alpha = NECR_alpha, NECRA = NECRA,
               NECRA_baseline = NECRA_base, NECR_alpha_baseline = NECR_alpha_base,
               NECRA_Reduction = NECRA_Reduction, NECRA_ReductionPct = NECRA_ReductionPct,
               NECR_alpha_Reduction = NECRa_Reduction, NECR_alpha_ReductionPct = NECRa_ReductionPct),
    dec
  )

  list(curve = necr_curve, summary = summary)
}

# ------------------------------------------------------------
# MULTI-MODEL COMPARE: pass a *named* list of predictions.
# Returns stacked curve table and one-row-per-model summary.
# ------------------------------------------------------------
newsvendor_econ_compare <- function(
    models_list, truth, w,
    c_u, c_o, coef = 1,
    alpha_grid = seq(0, 0.99, length.out = 100),
    alpha_eval = 0,
    include_baseline = TRUE,
    include_curve = TRUE
) {
  stopifnot(is.list(models_list), length(models_list) > 0)

  curves <- list(); sums <- list()
  for (nm in names(models_list)) {
    res <- newsvendor_econ_one(
      truth = truth, pred = models_list[[nm]], w = w,
      c_u = c_u, c_o = c_o, coef = coef,
      alpha_grid = alpha_grid, alpha_eval = alpha_eval,
      include_baseline = include_baseline, include_curve = include_curve
    )
    res$curve$Model   <- if (include_curve) nm else c()
    res$summary$Model <- nm
    curves[[nm]] <- res$curve
    sums[[nm]]   <- res$summary
  }

  list(
    curves  = do.call(rbind, curves),   # NECR(α) points per model (for plotting)
    summary = do.call(rbind, sums)      # NECRA (yours) + NECR@α + decompositions + reductions
  )
}

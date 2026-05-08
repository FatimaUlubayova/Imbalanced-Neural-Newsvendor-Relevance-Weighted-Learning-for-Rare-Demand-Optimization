# --- Train minimal models (one cost pair) ---
c_u <- 0.5; c_o <- 0.5

m_CL <- train_newsvendor(
  x_train_scaled, y_train_scaled, w_train = rep(1, nrow(x_train_scaled)),
  c_u = c_u, c_o = c_o, epochs = 200L, patience = 20L, val_frac = 0.10, val_seed = 42L, verbose = TRUE
)

m_RW <- train_newsvendor(
  x_train_scaled, y_train_scaled, w_train = rel_train$rel,
  c_u = c_u, c_o = c_o, epochs = 200L, patience = 20L, val_frac = 0.10, val_seed = 42L, verbose = TRUE
)

pred_fun_CL <- function(X) predict_newsvendor(m_CL, X) * (max_y - min_y) + min_y
pred_fun_RW <- function(X) predict_newsvendor(m_RW, X) * (max_y - min_y) + min_y

pred_CL <- pred_fun_CL(x_test_scaled)
pred_RW <- pred_fun_RW(x_test_scaled)

w_rel <- rel_test$rel
alphas <- c(0.5, 0.75)

# --- (D1) Tail permutation importance: All vs Tail (no retraining) ---
perm_import_one <- function(pred_fun, X, y, w, alpha, nrep=5) {
  base <- metricReg_NECR(truth=y, pred=pred_fun(X), w=w, alpha=alpha)
  tail_idx <- which(w >= alpha)
  out_all  <- numeric(ncol(X))
  out_tail <- numeric(ncol(X))
  for (j in seq_len(ncol(X))) {
    inc_all <- replicate(nrep, {
      Xp <- X; Xp[, j] <- sample(Xp[, j])
      metricReg_NECR(truth=y, pred=pred_fun(Xp), w=w, alpha=alpha) - base
    })
    inc_tail <- replicate(nrep, {
      Xp <- X; Xp[tail_idx, j] <- sample(Xp[tail_idx, j])
      metricReg_NECR(truth=y, pred=pred_fun(Xp), w=w, alpha=alpha) - base
    })
    out_all[j]  <- mean(inc_all)
    out_tail[j] <- mean(inc_tail)
  }
  list(all=out_all, tail=out_tail)
}

perm_RW_a05 <- perm_import_one(pred_fun_RW, x_test_scaled, y_test, w_rel, alpha=0.5)
perm_RW_a075 <- perm_import_one(pred_fun_RW, x_test_scaled, y_test, w_rel, alpha=0.75)

rho_05  <- cor(rank(perm_RW_a05$all),  rank(perm_RW_a05$tail),  method="spearman")
rho_075 <- cor(rank(perm_RW_a075$all), rank(perm_RW_a075$tail), method="spearman")

# --- (D2) Local sensitivity via autograd (average grad-norm) ---
grad_norm_mean <- function(fit, X, idx) {
  mdl <- fit$model; device <- fit$device; mdl$eval()
  torch <- reticulate::import("torch")
  batch <- torch$tensor(as.matrix(X[idx, , drop=FALSE]), dtype=torch$float32, device=device)$requires_grad_(TRUE)
  with_no_grad <- torch$enable_grad()  # allow grads
  on.exit(with_no_grad$`__exit__`(NULL, NULL, NULL))
  with_no_grad$`__enter__`()
  out <- mdl(batch)  # (B,1)
  loss <- out$sum()
  loss$backward()
  g <- batch$grad
  reticulate::py_to_r(reticulate::r_to_py(g)$norm(p=2)$item() / nrow(X[idx, , drop=FALSE]))
}

for_alpha <- function(alpha) {
  tail_idx <- which(w_rel >= alpha)
  non_idx  <- which(w_rel < alpha)
  c(
    tail  = grad_norm_mean(fit = m_RW, X = x_test_scaled, idx = tail_idx),
    nont  = if (length(non_idx)>0) grad_norm_mean(m_RW, x_test_scaled, sample(non_idx, min(length(non_idx), length(tail_idx)))) else NA_real_
  )
}

sens_05  <- for_alpha(0.5)
sens_075 <- for_alpha(0.75)

# --- (D3) Counterfactual stress tests on top-3 tail features ---
top3_tail <- function(perm_obj) order(perm_obj$tail, decreasing=TRUE)[1:3]
t3_05  <- top3_tail(perm_RW_a05)
t3_075 <- top3_tail(perm_RW_a075)

stress_delta <- function(pred_fun, X, y, w, alpha, feat_idx) {
  base <- metricReg_NECR(truth=y, pred=pred_fun(X), w=w, alpha=alpha)
  sds  <- apply(X, 2, sd)
  deltas <- sapply(feat_idx, function(j) {
    Xp <- X; Xp[, j] <- Xp[, j] + sds[j]
    d1 <- metricReg_NECR(truth=y, pred=pred_fun(Xp), w=w, alpha=alpha) - base
    Xm <- X; Xm[, j] <- Xm[, j] - sds[j]
    d2 <- metricReg_NECR(truth=y, pred=pred_fun(Xm), w=w, alpha=alpha) - base
    c(plus=d1, minus=d2)
  })
  t(deltas)
}

stress_05  <- stress_delta(pred_fun_RW, x_test_scaled, y_test, w_rel, 0.5,  t3_05)
stress_075 <- stress_delta(pred_fun_RW, x_test_scaled, y_test, w_rel, 0.75, t3_075)

# --- (D4) Tail leave-one-out (sample 20 points) ---
loo_tail <- function(pred_fun, X, y, w, alpha, B=20) {
  tail_idx <- which(w >= alpha)
  base <- metricReg_NECR(truth=y, pred=pred_fun(X), w=w, alpha=alpha)
  if (length(tail_idx) == 0) return(numeric())
  picks <- sample(tail_idx, min(B, length(tail_idx)))
  sapply(picks, function(i) {
    wi <- w; wi[i] <- 0  # drop by zeroing weight
    metricReg_NECR(truth=y, pred=pred_fun(X), w=wi, alpha=alpha) - base
  })
}

loo_05  <- loo_tail(pred_fun_RW, x_test_scaled, y_test, w_rel, 0.5,  B=20)
loo_075 <- loo_tail(pred_fun_RW, x_test_scaled, y_test, w_rel, 0.75, B=20)

list(
  perm_spearman = c(alpha0.5=rho_05, alpha0.75=rho_075),
  sensitivity_mean_grad = rbind(alpha0.5=sens_05, alpha0.75=sens_075),
  stress_05 = stress_05, stress_075 = stress_075,
  loo_quantiles = rbind(
    alpha0.5 = c(median=median(loo_05), q95=quantile(loo_05, 0.95)),
    alpha0.75 = c(median=median(loo_075), q95=quantile(loo_075, 0.95))
  )
)

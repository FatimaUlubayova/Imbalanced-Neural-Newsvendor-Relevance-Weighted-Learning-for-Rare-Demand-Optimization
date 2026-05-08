# --- deps (as you listed) ---
library(openxlsx)
library(FatihResearch)
library(ImbRegSamp)

source(file = "real data summary functions.R")
set.seed(1)

torch <- reticulate::import("torch")
nn    <- reticulate::import("torch.nn")

# ---------------------------
# Model: 2 hidden layers × 50 (ReLU) + linear output
# ---------------------------
build_newsvendor_mlp <- function(input_dim, hidden1 = 128L, hidden2 = 64L) {
  nn$Sequential(
    nn$Linear(as.integer(input_dim), hidden1),
    nn$ReLU(),
    nn$Dropout(0.25),
    nn$Linear(hidden1, hidden2),
    nn$ReLU(),
    nn$Dropout(0.25),
    nn$Linear(hidden2, 1L)
  )
}

# ---------------------------
# Your loss (batch-wise), vectorized
# output: (B,1), target: (B,1) or (B,)
# c_u_tensor, c_o_tensor: scalar tensors on the same device
# w_tensor: (B,) or (B,1) relevance/weight per sample
# ---------------------------

# regressor_loss <- function(output, target, c_u_tensor, c_o_tensor, w_tensor) {
#   torch$sum(
#     (
#       c_o_tensor * torch$clamp_min(output - target, 0) * torch$abs(output - target) +
#         c_u_tensor * torch$clamp_min(target - output, 0) * torch$abs(output - target)
#     ) * w_tensor
#   )
# }


regressor_loss <- function(output, target, c_u_tensor, c_o_tensor, w_tensor) {
  # Ensure shapes: (B,1)
  if (length(target$shape) == 1L) target <- target$unsqueeze(1L)
  if (length(w_tensor$shape) == 1L) w_tensor <- w_tensor$unsqueeze(1L)

  over  <- torch$clamp_min(output - target, 0)
  under <- torch$clamp_min(target - output, 0)
  err   <- torch$abs(output - target)

  loss_terms <- (c_o_tensor * over * err + c_u_tensor * under * err) * w_tensor
  torch$sum(loss_terms)
}

# ---------------------------
# Dataloader helper
# x: matrix/data.frame; y: numeric vector
# w: numeric vector of per-sample weights (e.g., relevance φ(d))
# ---------------------------
make_loader <- function(x, y, w, batch_size=128L, shuffle=TRUE, device=NULL) {
  if (is.null(device)) {
    device <- if (torch$cuda$is_available()) torch$device("cuda") else torch$device("cpu")
  }
  x_t <- torch$tensor(as.matrix(x), dtype=torch$float32, device=device)
  y_t <- torch$tensor(as.numeric(y), dtype=torch$float32, device=device)
  w_t <- torch$tensor(as.numeric(w), dtype=torch$float32, device=device)

  ds  <- reticulate::import("torch.utils.data")$TensorDataset(x_t, y_t, w_t)
  dl  <- reticulate::import("torch.utils.data")$DataLoader(
    ds, batch_size=as.integer(batch_size), shuffle=shuffle
  )
  list(loader=dl, device=device)
}
# train_newsvendor with auto validation split (default val_frac = 0.10)
# Assumes:
# - build_newsvendor_mlp(input_dim, hidden1, hidden2) exists
# - make_loader(...) and regressor_loss(...) are the ones you already use
# - torch/nn imported via reticulate

train_newsvendor <- function(
    x_train, y_train, w_train,
    x_val = NULL, y_val = NULL, w_val = NULL,
    c_u = 1.0, c_o = 1.0,
    epochs = 200L, batch_size = 128L, lr = 1e-3,
    weight_decay = 0.0, patience = 10L,
    hidden1 = 128L, hidden2 = 64L, verbose = TRUE,
    # NEW:
    val_frac = 0.10,         # fraction of *provided* train data to use as validation if val not provided
    val_seed = NULL          # optional seed for reproducible split
) {
  stopifnot(nrow(x_train) == length(y_train), length(y_train) == length(w_train))

  # ----- auto-split if no explicit validation is supplied -----
  has_explicit_val <- !is.null(x_val) && !is.null(y_val) && !is.null(w_val)
  if (!has_explicit_val && val_frac > 0) {
    N <- nrow(x_train)
    vN <- max(1L, as.integer(round(N * val_frac)))
    if (!is.null(val_seed)) {
      old_seed <- .Random.seed
      set.seed(val_seed)
      on.exit({ if (exists("old_seed")) .Random.seed <<- old_seed }, add = TRUE)
    }
    idx <- sample.int(N)
    val_idx   <- idx[seq_len(vN)]
    train_idx <- idx[-seq_len(vN)]

    x_val <- x_train[val_idx, , drop = FALSE]
    y_val <- y_train[val_idx]
    w_val <- w_train[val_idx]

    x_train <- x_train[train_idx, , drop = FALSE]
    y_train <- y_train[train_idx]
    w_train <- w_train[train_idx]

    if (verbose) message(sprintf("Auto validation split: %d train / %d val (val_frac=%.2f)",
                                 nrow(x_train), nrow(x_val), val_frac))
  }

  input_dim <- ncol(as.matrix(x_train))
  mdl <- build_newsvendor_mlp(input_dim, hidden1, hidden2)

  device <- if (torch$cuda$is_available()) torch$device("cuda") else torch$device("cpu")
  mdl$to(device)

  opt <- reticulate::import("torch.optim")$RMSprop(
    mdl$parameters()
  )

  # Prepare loaders
  tr <- make_loader(x_train, y_train, w_train, batch_size = batch_size, shuffle = TRUE, device = device)

  has_val <- !is.null(x_val) && !is.null(y_val) && !is.null(w_val)
  if (has_val) {
    va <- make_loader(x_val, y_val, w_val, batch_size = batch_size, shuffle = FALSE, device = device)
  }

  # Scalars on device
  c_u_tensor <- torch$tensor(as.numeric(c_u), dtype = torch$float32, device = device)
  c_o_tensor <- torch$tensor(as.numeric(c_o), dtype = torch$float32, device = device)

  best_val <- Inf
  best_state <- NULL
  wait <- 0L

  for (ep in seq_len(epochs)) {
    torch$set_grad_enabled(TRUE)  # <—
    mdl$train(); tr_loss_sum <- 0.0

    iter <- reticulate::iterate(tr$loader)
    for (batch in iter) {
      x_b <- batch[[1L]]; y_b <- batch[[2L]]; w_b <- batch[[3L]]

      opt$zero_grad()
      out <- mdl(x_b)

      loss <- regressor_loss(out, y_b, c_u_tensor, c_o_tensor, w_b)
      loss$backward()
      opt$step()

      tr_loss_sum <- tr_loss_sum + as.numeric(loss$item())
    }

    # Validation
    val_loss_num <- NA_real_
    if (has_val) {
      mdl$eval(); vl_sum <- 0.0
      with_no_grad <- torch$no_grad()
      on.exit(with_no_grad$`__exit__`(NULL, NULL, NULL), add = TRUE)
      with_no_grad$`__enter__`()
      iter_v <- reticulate::iterate(va$loader)
      for (batch in iter_v) {
        x_b <- batch[[1L]]; y_b <- batch[[2L]]; w_b <- batch[[3L]]
        out <- mdl(x_b)
        vl  <- regressor_loss(out, y_b, c_u_tensor, c_o_tensor, w_b)
        vl_sum <- vl_sum + as.numeric(vl$item())
      }
      val_loss_num <- vl_sum
      # Early stopping on validation loss (kept as in your version)
      if (val_loss_num + 1e-9 < best_val) {
        best_val <- val_loss_num
        best_state <- reticulate::r_to_py(list(
          state_dict = mdl$state_dict()
        ))
        wait <- 0L
      } else {
        wait <- wait + 1L
      }
      if (wait >= patience) {
        if (verbose) message(sprintf("Early stopping at epoch %d (best val: %.4f)", ep, best_val))
        break
      }
    }

    if (verbose & (ep %% 10) == 0) {
      if (has_val) {
        message(sprintf("Epoch %3d | train_loss(sum)=%.4f | val_loss(sum)=%.4f", ep, tr_loss_sum, val_loss_num))
      } else {
        message(sprintf("Epoch %3d | train_loss(sum)=%.4f", ep, tr_loss_sum))
      }
    }
  }

  # Load best weights if we tracked validation
  if (has_val && !is.null(best_state)) {
    mdl$load_state_dict(best_state$state_dict)
  }

  list(
    model = mdl,
    device = device,
    c_u = c_u, c_o = c_o,
    loss_fn = regressor_loss
  )
}


# ---------------------------
# Prediction helper
# ---------------------------
predict_newsvendor <- function(fit, x) {
  mdl <- fit$model
  device <- fit$device
  mdl$eval()
  x_t <- torch$tensor(as.matrix(x), dtype=torch$float32, device=device)
  with_no_grad <- torch$no_grad()
  on.exit(with_no_grad$`__exit__`(NULL, NULL, NULL))
  with_no_grad$`__enter__`()
  as.numeric(mdl(x_t)$squeeze()$cpu()$numpy())
}

data_train <- read.csv(file = "Real Data/train.csv")
data_test <- read.csv(file = "Real Data/test.csv")

data_train <- data_train[complete.cases(data_train),]
data_test <- data_test[complete.cases(data_test),]

rel_train <- relevance_PCHIP(y = data_train[,12])
rel_test <- relevance_PCHIP(y = data_train[,12], y_new = data_test[,12])

data_train$city <- as.factor(data_train$city)
data_train$brand <- as.factor(data_train$brand)
data_train$container <- as.factor(data_train$container)
data_train$capacity <- as.factor(data_train$capacity)

data_test$city <- as.factor(data_test$city)
data_test$brand <- as.factor(data_test$brand)
data_test$container <- as.factor(data_test$container)
data_test$capacity <- as.factor(data_test$capacity)

form <- paste0("quantity~", paste0(colnames(data_train)[3:11], collapse = "+"))

x_train <- model.matrix(as.formula(form), data = data_train)[,-1]
x_test <- model.matrix(as.formula(form), data = data_test)[,-1]

colnames(x_train) <- colnames(x_test) <- make.names(colnames(x_train))

means <- apply(x_train, 2, mean)
sds <- apply(x_train, 2, sd)

x_train_scaled <- scale(x_train, means, sds)
x_test_scaled <- scale(x_test, means, sds)

y_train <- data_train[,12]
y_test <- data_test[,12]

min_y <- min(y_train)
max_y <- max(y_train)
y_train_scaled <- (y_train - min_y)/(max_y - min_y)
y_test_scaled <- (y_test - min_y)/(max_y - min_y)

c_ratios = c(0.1, 0.25, 0.5, 0.75, 0.9)
rrs = c(0, 0.25, 0.5, 0.75, 1)

m_summary_all <- list()

for (c_ratio in c_ratios) {
  c_u <- c_ratio
  c_o <- 1 - c_u


  m_CL <- train_newsvendor(x_train = x_train_scaled,
                           y_train = y_train_scaled,
                           w_train = rep(1, nrow(x_train_scaled)), c_u = c_u, c_o = c_o, epochs = 1000L, patience = 100L)


  m_RW <- train_newsvendor(x_train = x_train_scaled, y_train =
                           y_train_scaled,
                           w_train = rel_train$rel, c_u = c_u, c_o = c_o, epochs = 1000L, patience = 100L)


  pred_CL <- predict_newsvendor(fit = m_CL, x = x_test_scaled)
  pred_RW <- predict_newsvendor(fit = m_RW, x = x_test_scaled)

  pred_CL_descaled <- pred_CL*(max_y - min_y) + min_y
  pred_RW_descaled <- pred_RW*(max_y - min_y) + min_y

  for (rr in rrs) {

    m_SMOTER <- SMOTER(x = x_train_scaled, y = y_train_scaled, phi = rel_train$rel, thresh_rel = rr)
    x_train_scaled_SMOTER <- m_SMOTER$x_new
    y_train_scaled_SMOTER <- m_SMOTER$y_new

    m_SMOTER <- train_newsvendor(x_train = x_train_scaled_SMOTER,
                                 y_train = y_train_scaled_SMOTER,
                                 w_train = rep(1, nrow(x_train_scaled_SMOTER)), c_u = c_u, c_o = c_o, epochs = 1000L, patience = 100L)


    pred_SMOTER <- predict_newsvendor(fit = m_SMOTER, x = x_test_scaled)
    pred_SMOTER_descaled <- pred_SMOTER*(max_y - min_y) + min_y

    preds <- list(
      CL     = pred_CL_descaled,
      SMOTER = pred_SMOTER_descaled,
      RW     = pred_RW_descaled
    )

    m_summary <- newsvendor_econ_compare(
      models_list = preds,
      truth = y_test,
      w = rel_test$rel,
      c_u = c_u,
      c_o = c_o,
      coef = 1,
      alpha_eval = rr,
      include_baseline = FALSE,
      include_curve = FALSE
    )

    m_summary_all <- append(m_summary_all, list(m_summary$summary))
  }
}

write.xlsx(x = do.call(rbind, m_summary_all), file = "real data results.xlsx", asTable = TRUE)

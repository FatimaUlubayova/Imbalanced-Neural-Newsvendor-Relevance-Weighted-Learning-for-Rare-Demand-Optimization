f_createNNModel <- function(n_node = 20,
                            n_hidden = 2,
                            dropout = 0.25,
                            p,
                            classification = TRUE,
                            k_class = NULL,
                            BayesLinear = FALSE,
                            BL_prior_mu = 0,
                            BL_prior_sigma = 0.05) {
  m_torch <- torch$nn$Sequential()

  if (BayesLinear) {
    m_torch$append(
      torchbnn$BayesLinear(
        prior_mu = torch$tensor(BL_prior_mu),
        prior_sigma = torch$tensor(BL_prior_sigma),
        in_features = p,
        out_features = n_node
      )
    )
  } else {
    m_torch$append(torch$nn$Linear(in_features = p, out_features = n_node))
    if (dropout > 0) {
      m_torch$append(torch$nn$Dropout(p = dropout))
    }
  }
  m_torch$append(torch$nn$ReLU())

  if (n_hidden > 1) {
    for (i in 2:n_hidden) {
      if (BayesLinear) {
        m_torch$append(
          torchbnn$BayesLinear(
            prior_mu = torch$tensor(BL_prior_mu0),
            prior_sigma = torch$tensor(BL_prior_sigma),
            in_features = n_node,
            out_features = n_node
          )
        )
      } else {
        m_torch$append(torch$nn$Linear(in_features = n_node, out_features = n_node))
        if (dropout > 0) {
          m_torch$append(torch$nn$Dropout(p = dropout))
        }
      }
      m_torch$append(torch$nn$ReLU())
    }
  }

  if (BayesLinear) {
    m_torch$append(
      torchbnn$BayesLinear(
        prior_mu = torch$tensor(BL_prior_mu),
        prior_sigma = torch$tensor(BL_prior_sigma),
        in_features = n_node,
        out_features = if (classification) k_class else 1L
      )
    )
  } else {
    m_torch$append(torch$nn$Linear(
      in_features = n_node,
      out_features = if (classification) k_class else 1L))
  }
  if (classification) {
    m_torch$append(torch$nn$Softmax(dim = 1L))
  } else {
    m_torch$double()
  }
  return(m_torch)
}

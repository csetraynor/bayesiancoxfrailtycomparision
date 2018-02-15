weibull_sim_data <- function(alpha, mu, n, beta, X) {
  
  beta <- as.vector(as.numeric(beta))
  X <- array(matrix(as.numeric(X)), dim = c(n, length(beta)))
  
  #prognostic index
  hazard_ratio = exp (X %*% beta )
  
  
  
  t = lapply(hazard_ratio, function(x) 
    rweibull(n = 1, shape = alpha, scale = exp(-(mu + hazard_ratio)/alpha)))
  t = do.call(rbind, t)
  print(t)
  
  
  data <- data.frame(surv_months = t,
                     censor_months = rexp(n = n, rate = 1/100),
                     stringsAsFactors = F
  ) %>%
    dplyr::mutate(os_status = ifelse(surv_months < censor_months,
                                     'DECEASED', 'LIVING'
    ),
    os_months = ifelse(surv_months < censor_months,
                       surv_months, censor_months
    )
    )
  
  return(data)
}

test_alpha <- 0.8
test_mu <- -4
test_n <- 100
test_X = matrix(c(rnorm(100), sample(c(0,1), 100, replace = TRUE), ncol=2))
test_beta = c(0.5, 1)
weibull_sim_data(alpha = test_alpha, mu = test_mu, n = test_n, beta = test_beta, X = test_X)

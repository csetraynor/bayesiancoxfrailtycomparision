#Simulate time to event data
#weibull_sim_data function takes two parameters (alpha and mu) as inputs and a desired sample size (n). 


weibull_sim_data <- function(alpha, mu, n) {
  
  data <- data.frame(surv_months = rweibull(n = n, alpha, exp(-(mu)/alpha)),
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
#Censoring is "arbitrarily" rexp() , censoring is assumed to be noninformative.

#Simulate data for arbitrary imput values
test_alpha <- 0.8
test_mu <- -3

## sample size from TCGA blca data
test_n <- 500

## test these inputs for arbitrary values of alpha & mu
simulated_data <- 
  weibull_sim_data(alpha = test_alpha,
                   mu = test_mu,
                   n = test_n
  ) 
head(simulated_data)


## plot KM curve from simulated data
require(survival)

simulated_data <- 
  simulated_data %>%
  dplyr::mutate(os_deceased = os_status == 'DECEASED')

ggfortify::autoplot(survfit(Surv(os_months, os_deceased) ~ 1,
                            data = simulated_data
), conf.int = F) + 
  ggtitle('Simulated KM curve')

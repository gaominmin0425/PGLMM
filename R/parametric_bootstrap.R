library(broom.mixed)
library(tidyverse)
para_bootstrap <- function(B, Dataset, para){
  ## we first re-sample the random effect Z_i
  n = nrow(Dataset)
  i = 10
  j = 3
  N = i*B
  Dataset1 = Dataset %>% mutate(Year2005 = ifelse(Dataset$year == "2005",1,0), Year2006 = ifelse(Dataset$year=="2006", 1,0))
  tortoise_fit = run_model(Dataset1, "tortoise")
  Sigmasq = tortoise_fit$sigmasq
  Zi = rnorm(N, mean =0, sd = sqrt(Sigmasq))
  Zi_1 = rep(Zi,each = 3)
  ##we fitted the model with random effects to compute mu_i_hat
  mu_i_hat = exp(log(Dataset1$Area)+tortoise_fit$beta[1]+(para*Dataset1$prev
                                                          +tortoise_fit$beta[3]*Dataset1$Year2005
                                                          +tortoise_fit$beta[4]*Dataset1$Year2006
                                                          +Zi_1))
  ## Generate B times of Y_ij from poisson model, and then refit the model to obtain parametric bootstrapped estimates.
  Dataset_new = Dataset1 %>% slice(rep(1:30,B))%>% mutate(B = rep(1:B,each = 30)) %>% mutate(mu_i_hat = mu_i_hat) %>% mutate(Y_hat = rpois(n(), mu_i_hat))
  Bootstrap1 = Dataset_new %>% group_by(B) %>% summarise(tidy(glmer(Y_hat~prev + offset(log(Area))+factor(year)+(1|Site),
                                                                    family = poihsson)), .groups = "drop")

  return(Bootstrap1)
}



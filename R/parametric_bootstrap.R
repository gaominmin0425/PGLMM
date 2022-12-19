library(broom.mixed)
library(tidyverse)
#' Title function of Parametric bootstrap
#'
#' @param B iteration of bootstrap
#' @param Dataset the dataset of my own example - "gopher_tortoise" dataset
#' @param para the value of the coefficient of the predictor -- "prev". For example, tortoise_fit$beta[2], or 0
#'
#' @return return the bootstrap which contains B, effect, group. term, estimates, standard error, statistic, and p-value.
#' @export
#'
#' @examples
#' ## choosing different B into the para_bootstrap function
#'para_bootstrap(100. tortoise, tortoise_fit$beta[2])
#'tortoise_fit$beta[2] indicates the coefficient of "prev" in fitted GLMM model of "tortoise" data set.
#' @example
#'para_bootstrap(1000, tortoise, tortoise_fit$beta[2])
#'generating B = 1000
#' @example
#' para_bootstrap(10000, tortoise, tortoise_fit$beta[2])
#' generating B = 10000

para_bootstrap <- function(B, Dataset, para){
  n = nrow(Dataset) ## length of the date set, which is the sample size of the data set.
  i = 10 ## 10 site
  j = 3 ## every site record three years
  N = i*B ## the total number of sites after iterating B times
  ## change categorical variables as factor, and then add them to the original data.
  Dataset1 = Dataset %>% mutate(Year2005 = ifelse(Dataset$year == "2005",1,0), Year2006 = ifelse(Dataset$year=="2006", 1,0))
  ## call the function in the estimation function to get the statistic
  tortoise_fit = run_model(Dataset1, "tortoise")
  Sigmasq = tortoise_fit$sigmasq
  ## we first re-sample the random effect Z_i
  Zi = rnorm(N, mean =0, sd = sqrt(Sigmasq))
  Zi_1 = rep(Zi,each = 3)
  ##we fitted the model with random effects to compute mu_i_hat
  mu_i_hat = exp(log(Dataset1$Area)+tortoise_fit$beta[1]+(para*Dataset1$prev
                                                          +tortoise_fit$beta[3]*Dataset1$Year2005
                                                          +tortoise_fit$beta[4]*Dataset1$Year2006
                                                          +Zi_1))
  ## Generate B times of Y_ij from poisson model, and then refit the model to obtain parametric bootstrapped estimates.
  Dataset_new = Dataset1 %>% slice(rep(1:30,B))%>% mutate(B = rep(1:B,each = 30)) %>% mutate(mu_i_hat = mu_i_hat) %>% mutate(Y_hat = rpois(n(), mu_i_hat))
  ## Create bootstrap and return it later
  Bootstrap1 = Dataset_new %>% group_by(B) %>% summarise(tidy(glmer(Y_hat~prev + offset(log(Area))+factor(year)+(1|Site),
                                                                    family = poisson)), .groups = "drop")

  return(Bootstrap1)
}



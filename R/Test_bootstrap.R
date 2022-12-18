source("parametric_bootstrap.R")
source("gopher_tortoise.R")

tortoise <- read_csv("gopher_tortoise.csv")
tortoise = tortoise %>% mutate(Year2005 = ifelse(tortoise$year == "2005",1,0), Year2006 = ifelse(tortoise$year=="2006", 1,0))
tortoise_fit <- run_model(tortoise, "tortoise")
Sigmasq = tortoise_fit$sigmasq
gopher = para_bootstrap(100,tortoise)
## compute bootstrap standard error and confidence interval in terms of prev
gopher %>% filter(term == "prev") %>% summarize(SD = sd(estimate),Lower95 = quantile(estimate, .025),Upper95 = quantile(estimate,.975))


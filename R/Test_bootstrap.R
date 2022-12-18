source("parametric_bootstrap.R")
source("gopher_tortoise.R")
gopher = para_bootstrap(100,tortoise)
## compute bootstrap standard error and confidence interval in terms of prev
gopher %>% filter(term == "prev") %>% summarize(SD = sd(estimate),Lower95 = quantile(estimate, .025),Upper95 = quantile(estimate,.975))


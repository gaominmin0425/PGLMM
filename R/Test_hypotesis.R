## Test hypothesis test when beta1 is zero
gopher = para_bootstrap(100,tortoise, 0)
## compute the t-value from test statistics from gopher_tortoise.R
t_value = tortoise_fit$test_stat["prev"]
p_value = gopher %>% filter(term == "prev") %>%summarize(p = mean(abs(statistic) > abs(t_value)))

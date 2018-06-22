sim_regression = function(n, beta0 = 0, beta1 = 1) {
	
	sim_data = tibble(
		x = rnorm(n, mean = 1, sd = 1),
		y = beta0 + beta1 * x + rnorm(n, 0, 1)
	)
	
	ls_fit = lm(y ~ x, data = sim_data)
	
	tibble(
		beta0_hat = coef(ls_fit)[1],
		beta1_hat = coef(ls_fit)[2]
	)
}

n = rep(30, 10)

library(tidyverse)
library(purrr)

map_df(n, ~sim_regression(n = .x), .id = "n")

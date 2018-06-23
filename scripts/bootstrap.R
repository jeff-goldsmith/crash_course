df = tibble(
  x = rnorm(250, 1, 1),
  error = x * rnorm(250, 0, 1),
  y = 2 + 3 * x + error
)

df %>% 
  ggplot(aes(x = x, y = y)) + geom_point()

df %>% 
  modelr::bootstrap(100) %>% 
  mutate(models = map(strap, ~lm(y ~ x, data = .x)),
         coef = map(models, ~broom::tidy(.x))) %>% 
  select(.id, coef) %>% 
  unnest() %>% 
  group_by(term) %>% 
  summarize(m = mean(estimate),
            sd = sd(estimate))

lm(y ~ x, data = df) %>% broom::tidy()

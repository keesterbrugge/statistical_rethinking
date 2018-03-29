library(tidyverse)
library(brms)
library(tidybayes)
library(tidybayes.rethinking)


overlay_fitted_distribution <- function( data2, predictor, predicted, model){  # also y and model
  x_var <- rlang::enquo(predictor)
  x_name <- rlang::quo_name(x_var)
  y_var <- rlang::enquo(predicted)
  
  data2 %>%
    data_grid(!!x_name := seq_range(!!x_var, n = 101)) %>% 
    add_fitted_samples(model = model) %>% ungroup() %>%
    ggplot() +
    geom_point(data = data2, aes_string(x = rlang::quo_text(x_var), y = rlang::quo_text(y_var) )) + 
    stat_lineribbon(aes_string(x = rlang::quo_text(x_var), y = "estimate"), alpha = 0.5, .prob = c(.95)) +
    theme(legend.position="none")
}



library(rethinking)
data(milk)
d <- milk

b5.5 <- 
  brm(data = d, family = gaussian,
      kcal.per.g ~ 1 + neocortex.perc,
      prior = c(set_prior("normal(0, 100)", class = "Intercept"),
                set_prior("normal(0, 1)", class = "b"),
                set_prior("uniform(0, 1)", class = "sigma")),
      chains = 4, iter = 2000, warmup = 500, cores = 4,
      control = list(adapt_delta = 0.95))

## add both fitted and predicted not just either
list(fit = add_fitted_samples, pred = add_predicted_samples ) %>% 
  map(function(f) f(
    newdata = d,  model = b5.5)) %$% 
  full_join(x = fit, y = pred) %>% ungroup()


d %>% filter(!is.na(neocortex.perc)) %>% group_by(kcal.per.g) %>% 
  summarise(
    fit = list(add_fitted_samples(newdata = ., model = b5.5, n = 2) %>% ungroup() %>% select(estimate)),
    pred = list(add_predicted_samples(newdata = ., model = b5.5, n = 2) %>% ungroup() %>% select(pred))
  ) %>% unnest()




list(fit = add_fitted_samples, pred = add_predicted_samples ) %>% 
  map_dfr(function(f) f(
    newdata = d %>% filter(!is.na(neocortex.perc), !is.na(kcal.per.g)) ,  model = b5.5, n = 2)) 


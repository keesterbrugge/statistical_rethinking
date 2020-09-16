library(tidyverse)
library(brms)
library(tidybayes)
library(tidybayes.rethinking)
library(modelr)

# plot line fitted line and data points ####
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


# plot parameters estimates ####
b5.8 %>% gather_samples( `b_.*|sigma`, regex = TRUE) %>% ggplot() + stat_pointintervalh(aes(x = estimate, y = term))
# tidybayes::as_sample_tibble(b)

# add fitted and prediction to brms ####
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

## add both fitted and predicted not just either. 2 alternatives
add_fitted_and_predicted_samples <- function(newdata, model, ...){
  list(estimate = add_fitted_samples, pred = add_predicted_samples ) %>% 
    invoke_map(.f = ., .x = list(list(newdata = newdata,  model = model, ...))) %$% 
    full_join(x = estimate, y = pred) %>% ungroup()
}

# list(fit = add_fitted_samples, pred = add_predicted_samples ) %>% 
#   map_dfr(function(f) f(
#     newdata = d %>% filter(!is.na(neocortex.perc), !is.na(kcal.per.g)) ,  model = b5.5, n = 2)) 

#  too slow
# d %>% filter(!is.na(neocortex.perc)) %>% group_by(kcal.per.g) %>% 
#   summarise(
#     fit = list(add_fitted_samples(newdata = ., model = b5.5, n = 2) %>% ungroup() %>% select(estimate)),
#     pred = list(add_predicted_samples(newdata = ., model = b5.5, n = 2) %>% ungroup() %>% select(pred))
#   ) %>% unnest()







## model comparison #####

data(cars)

df <- tibble(name = c("model_frac", "model"), data = list(sample_frac(cars, size = 0.4), cars))


df3 <- df %>% mutate(model = data %>% purrr::map( ~ brm(data = .x, family = gaussian,
                                                        dist ~ 1 + speed,
                                                        prior = c(set_prior("normal(0, 100)", class = "Intercept"),
                                                                  set_prior("normal(0, 10)", class = "b"),
                                                                  set_prior("uniform(0, 30)", class = "sigma")),
                                                        chains = 2, iter = 2000, warmup = 1000, cores = 2))
)

# update(b, formula. = dist ~ 1 + speed,
#                       newdata = sample_frac(cars, size = 0.4))
       
       
df3 <- df3 %>% mutate(
  lppd3 = model %>% map_dbl(
    . %>% 
      log_lik %>% 
      exp %>% 
      as_tibble %>% 
      summarise_all(mean) %>% 
      log %>% 
      sum)
)

calc_lppd <-function(brmsfit){brmsfit %>% 
  log_lik %>% 
  exp %>% 
  as_tibble %>% 
  summarise_all(mean) %>% 
  log %>% 
  sum}

df3 <- df3 %>% mutate(
  pwaic = model %>% map_dbl(
    . %>% 
      log_lik %>% 
      as_tibble %>% 
      summarise_all(var) %>% 
      sum)
)


# coeftab ####
library(tidybayes)
models <- list(b6.11 = b6.11, b6.12 = b6.12, b6.13 = b6.13, b6.14 = b6.14)
(df <- tibble(name = names(models), model = models) %>% 
    mutate(coef_broom = model %>% map(.f = tidy)) %>% unnest(coef_broom) %>% filter(term != "lp__")
)

df %>% ggplot() +
  ggstance::geom_pointrangeh(aes(x = estimate, xmin = lower, xmax = upper, y = name)) + 
  geom_vline(xintercept =  0, linetype = "dashed") +
  facet_wrap(~ term, ncol = 1)


# multi formula brm fit ####
multi_formula_brm <- function(...) {
  model <- NULL
  function(form){
    if (is.null(model)){
      model <<-  brm(formula = form, ...)
      model
    } else {
      update(model, formula. = form)
    }
  }
}

# column-list formula comparison ####
library(rethinking)
library(tidyverse)
library(brms)


data(Howell1)
d <- Howell1
d$age <- (d$age - mean(d$age))/sd(d$age)
i <- sample(1:nrow(d),size=nrow(d)/2)
d1 <- d[ i , ]


formulas <- list(
  m1 = height ~ 1 + age,
  m2 = height ~ 1 + age + I(age^2))

df <- tibble( 
  name = names(formulas),
  formulas = formulas
) %>% 
  mutate(model = formulas %>% purrr::map( 
    .f = brm, 
    data = d1, 
    family = gaussian,
    prior = c(set_prior("normal(0, 100)", class = "Intercept"),
              set_prior("normal(0, 10)", class = "b"),
              set_prior("cauchy(0, 1)", class = "sigma")),
    chains = 2, iter = 2000, warmup = 1000, cores = 2)
  )

waic_wrapper <- function(...) {
  dots <- list(...)
  if (!"x" %in% names(dots)) {
    names(dots)[1] <- "x"
  }
  do.call(brms::waic, dots)
}

invoke(waic_wrapper, .x = df$model, model_names = df$name)


(invoke(brms_fn_wrapper, .x = df$model, fn = brms::pp_average, model_names = df$name, weights = "loo2", 
        summary = TRUE, newdata = d1 %>% data_grid(age = seq_range(age, n = 50))) -> model_averaged_pred)

model_averaged_pred %>% as_tibble() %>% 
  mutate(age_val = d1 %>% data_grid(age = seq_range(age, n = 50)) %>% pull(age)) %>% 
  ggplot() + 
  geom_lineribbon(aes(x = age_val, y = Estimate, ymin = Q2.5, ymax = Q97.5, fill = c(0.95)))




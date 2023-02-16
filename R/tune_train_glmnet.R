tune_train_glmnet <- function(data, recipe, resamples) {
  outcome <- recipe$var_info$variable[which(recipe$var_info$role == "outcome")]
  predictors <- recipe$var_info$variable[which(recipe$var_info$role == "predictor")]
  
  set.seed(1994)
  grid <- grid_regular(penalty(), mixture(), levels = c(50, 5))
  
  tune_spec <-
    logistic_reg(
      penalty = tune(),
      mixture = tune()
    ) %>%
    set_engine("glmnet")
  
  wf <- 
    workflow() %>%
    add_model(tune_spec) %>%
    add_recipe(recipe)
  
  tuning <- 
    tune_grid(
      wf,
      resamples = resamples,
      grid = grid,
      metrics = metric_set(roc_auc, accuracy, sensitivity, specificity),
      control = control_grid(save_pred = T)
    )
  
  best_params <-
    tuning %>%
    select_best(metric = "roc_auc")
  
  final_wf <- 
    wf %>% 
    finalize_workflow(best_params)
  
  fit <- fit(final_wf, data = data)
  
  res <- 
    list(
      outcome = outcome,
      predictors = predictors,
      tuning = tuning,
      best_params = best_params,
      fit = fit
    )
  
  return(res)
} 

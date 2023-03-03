ClassifElasNet <- R6Class(
  classname = "ClassifElasNet",
  inherit = ClassifMetrics,
  
  public = list(
    train_df = NULL,
    test_df = NULL,
    response = NULL,
    fitted = NULL,
    recipe = NULL,
    test_targets = NULL,
    train_targets = NULL,
    
    tune_res = NULL,
    best_params = NULL,
    final_wf = NULL,
    
    test_preds = NULL,
    
    initialize = function(train_df, test_df, response) {
      self$train_df <- train_df
      self$test_df <- test_df
      self$response <- response
      self$recipe <-
        recipe(as.formula(glue("{self$response} ~ .")), data = self$train_df) %>%
        step_other(all_nominal_predictors(), threshold = 0.05) %>%
        step_lencode_glm(all_nominal_predictors(), outcome = self$response) %>%
        step_impute_bag(all_predictors()) %>%
        step_YeoJohnson(all_numeric_predictors()) %>%
        step_normalize(all_numeric_predictors())
      self$test_targets <- as.numeric(test_df[[response]]) - 1
      self$train_targets <- as.numeric(train_df[[response]]) - 1
    },
    
    tune = function(levels_lambda, levels_alpha) {
      tune_spec <- logistic_reg(engine = "glmnet", penalty = tune(), mixture = tune())
      resamples <- vfold_cv(self$train_df, v = 4, strata = all_of(self$response))
      formula <- as.formula(glue("{self$response} ~ ."))
      grid <- grid_regular(penalty(), mixture(), levels = c(levels_lambda, levels_alpha))
      recipe <-
        recipe(formula, data = self$train_df) %>%
        step_other(all_nominal_predictors(), threshold = 0.05) %>%
        step_lencode_glm(all_nominal_predictors(), outcome = self$response) %>%
        step_impute_bag(all_predictors()) %>%
        step_YeoJohnson(all_numeric_predictors()) %>%
        step_normalize(all_numeric_predictors())
      wf <-
        workflow() %>%
        add_model(tune_spec) %>%
        add_recipe(recipe)
      tuning <-
        tune_grid(
          wf,
          resamples = resamples,
          grid = grid,
          metrics = metric_set(roc_auc),
          control = control_grid(save_pred = F)
        )
      self$tune_res <- tuning
      self$best_params <- tuning %>% select_best(metric = "roc_auc") %>% select(penalty, mixture)
      self$final_wf <- wf %>% finalize_workflow(self$best_params)
      invisible(self)
    },
    
    train = function() {
      self$fitted <- parsnip::fit(self$final_wf, data = self$train_df)
      invisible(self)
    },
    
    predict = function() self$test_preds <- predict(self$fitted, new_data = self$test_df, type = "prob")$.pred_1,
    
    plot_coefs = function() {
      df <-
        self$fitted %>%
        extract_fit_parsnip() %>%
        tidy() %>%
        filter(term != "(Intercept)")
      
      df_non_zero <-
        df %>%
        filter(estimate != 0)
      
      nb_zero_coef <- nrow(df) - nrow(df_non_zero)
      
      df_non_zero %>%
        mutate(Sign = if_else(estimate > 0, "+", "-")) %>%
        mutate(abs_estimate = abs(estimate)) %>%
        mutate(term = fct_reorder(term, abs_estimate)) %>%
        ggplot(aes(x = term, y = abs_estimate, fill = Sign)) +
        geom_col(alpha = 0.7) +
        xlab(NULL) +
        ylab("Absolute value of coefficient") +
        scale_fill_manual(values = c("#a61d21", "#00743F")) +
        coord_flip() +
        labs(caption = glue("Note: {nb_zero_coef} other coefficients are zero"))
    },
    
    print = function() {
      cat("Régression logistique avec régularisation elastic-net \n\n")
      cat("\tTraining set: ", nrow(self$train_df), " observations\n", sep = "")
      cat("\tTesting set: ", nrow(self$test_df), " observations\n\n", sep = "")
      cat("\tVariable réponse: ", self$response, "\n\n", sep = "")
      cat("\tPrédicteurs:\n", glue("{names(select(self$train_df, -self$response))}\n"), sep = "\n\t")
      invisible(self)
    }
  )
)

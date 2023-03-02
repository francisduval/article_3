NeuralNet <- R6Class(
  classname = "NeuralNet",
  inherit = ClassifMetrics,
  
  public =
    list(
      model_spec = NULL,
      
      train_ds = NULL,
      valid_ds = NULL,
      test_ds = NULL,
      
      fitted = NULL,
      
      test_preds = NULL,
      
      train_targets = NULL,
      test_targets = NULL,
      
      
      initialize = function(model_spec, train_ds, valid_ds, test_ds) {
        self$model_spec <- model_spec
        self$train_ds <- train_ds
        self$valid_ds <- valid_ds
        self$test_ds <- test_ds
        
        self$test_targets <- as.numeric(test_ds$y)
        self$train_targets <- as.numeric(train_ds$y)
      },
      
      train = function(input_size_tele, input_size_class, emb_size = 2, nb_epochs = 1) {
        train_dl <- dataloader(train_ds, batch_size = 128, shuffle = F)
        valid_dl <- dataloader(valid_ds, batch_size = 128, shuffle = F)
        
        self$fitted <- 
          self$model_spec %>%
          setup(
            loss = nnf_binary_cross_entropy,
            optimizer = optim_adam,
            metrics = list(luz_metric_binary_auroc(thresholds = seq(0, 0.5, by = 0.0005)), luz_metric_mse())
          ) %>%
          set_hparams(
            input_size_tele = input_size_tele, 
            input_size_class = input_size_class, 
            emb_size = emb_size
          ) %>% 
          luz::fit(
            train_dl, 
            epochs = nb_epochs, 
            valid_data = valid_dl
          )
        invisible(self)
      },
      
      predict = function() {
        self$test_preds <- as.double(self$fitted$model$forward(self$test_ds[1:length(self$test_ds)]$x))
      },
      
      plot_training = function() {
        tibble(
          train_loss = self$fitted$records$metrics$train %>% map("loss") %>% flatten_dbl(),
          train_auc = self$fitted$records$metrics$train %>% map("auc") %>% flatten_dbl(),
          valid_loss = self$fitted$records$metrics$valid %>% map("loss") %>% flatten_dbl(),
          valid_auc = self$fitted$records$metrics$valid %>% map("auc") %>% flatten_dbl()
        ) %>%
          mutate(epochs = seq_along(train_loss)) %>% 
          pivot_longer(cols = -"epochs") %>%
          separate(col = "name", into = c("dataset", "metric"), sep = "_") %>% 
          ggplot(aes(x = epochs, y = value, col = dataset)) +
          geom_point(shape = 21) +
          geom_line() +
          scale_color_discrete(name = NULL) +
          ylab(NULL) +
          facet_grid(vars(metric), scales = "free_y")
      }
    )
)

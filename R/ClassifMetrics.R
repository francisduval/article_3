ClassifMetrics <- R6Class(
  classname = "ClassifMetrics",
  
  public = 
    list(
      mse = function() mean((self$test_targets - self$test_preds) ^ 2),
      mse_naif = function() mean((self$test_targets - rep(mean(self$train_targets), length(self$test_targets))) ^ 2),
      brier_skill = function() 1 - self$mse() / self$mse_naif(),
      auroc = function() roc_auc_vec(truth = factor(self$test_targets, levels = c("1", "0")), estimate = self$test_preds),
      
      print_metrics = function() {
        cat("AUROC = ", round(self$auroc(), 4), "\n")
        cat("Brier score = ", round(self$mse(), 4), "\n")
        cat("Brier score skill = ", round(self$brier_skill(), 4), "\n")
        cat("Brier score modèle naïf = ", round(self$mse_naif(), 4), "\n")
      }
    )
)

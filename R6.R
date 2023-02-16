library(targets)
library(R6)
library(tidyverse)
library(hms)
library(glue)

luz_metric_mon_mse <- luz_metric(
  abbrev = "MSE",
  initialize = function() {
    self$sum_error <- torch::torch_tensor(0, dtype = torch::torch_float64())
    self$n <- torch::torch_tensor(0, dtype = torch::torch_int64())
  },
  update = function(preds, targets) {
    self$sum_error <- self$sum_error + torch::torch_sum(torch::torch_pow(exponent = 2, preds - targets))
    self$n <- self$n + targets$numel()
  },
  compute = function() {
    (self$sum_error / self$n)$item()
  }
)

aa <- luz_metric_mon_mse()

metric <- aa$new()

preds <- torch_tensor(c(0.1, 0.2, 0.3, 0.4))
targets <- torch_tensor(c(0, 0, 1, 1))

metric$update(preds, targets)

preds <- torch_tensor(runif(100))
targets <- torch_tensor(rbinom(100, 1, 0.2))

metric$update(preds, targets)
metric$compute()

preds <- torch_tensor(runif(100))
targets <- torch_tensor(rbinom(100, 1, 0.2))

metric$update(preds, targets)
metric$compute()

preds <- torch_tensor(runif(100))
targets <- torch_tensor(rbinom(100, 1, 0.2))

metric$update(preds, targets)
metric$compute()

preds <- torch_tensor(runif(100))
targets <- torch_tensor(rbinom(100, 1, 0.2))

metric$update(preds, targets)
metric$compute()


preds <- torch_tensor(runif(1000))
targets <- torch_tensor(rbinom(1000, 1, 0.2))

metric$update(preds, targets)
metric$compute()


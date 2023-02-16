library(targets)
library(torch)
library(luz)
library(torchvision)
library(torchdatasets)
library(tidyverse)

# ===============================================================================================================================

nn_data_time_of_day <- tar_read(nn_data_time_of_day)

time_of_day_dataset <- 
  dataset(
    name = "time_of_day_dataset",
    
    initialize = function(df) {
      x_cont <- select(df, x_1:x_96) %>% as.matrix()
      self$x_cont <- torch_tensor(x_cont)
      
      claim_ind_cov_1_2_3_4_5_6 <- as.numeric(df$claim_ind_cov_1_2_3_4_5_6, levels = c("1", "0")) - 1
      self$y <- torch_tensor(claim_ind_cov_1_2_3_4_5_6)
    },
    
    .getitem = function(i) {
      list(x_cont = self$x_cont[i, ], y = self$y[i])
      
    },
    
    .length = function() {
      self$y$size()[[1]]
    }
)



train_indices <- 1:30000
valid_indices <- 30001:40000
test_indices <- 40001:49671

train_ds <- time_of_day_dataset(nn_data_time_of_day[train_indices, ])
valid_ds <- time_of_day_dataset(nn_data_time_of_day[valid_indices, ])
test_ds <- time_of_day_dataset(nn_data_time_of_day[test_indices, ])

train_dl <- train_ds %>% dataloader(batch_size = 256, shuffle = TRUE)
valid_dl <- valid_ds %>% dataloader(batch_size = 256, shuffle = FALSE)
test_dl <- test_ds %>% dataloader(batch_size = 256, shuffle = FALSE)



net <- 
  nn_module(
    "class_net",
    
    initialize = function() {
      
      self$linear1 = nn_linear(96, 256)
      self$linear2 = nn_linear(256, 64)
      self$linear3 = nn_linear(64, 16)
      self$linear4 = nn_linear(16, 1)
      
    },
    
    forward = function(x) {
      
      x %>%
        self$linear1() %>%
        nnf_relu() %>%
        self$linear2() %>%
        nnf_relu() %>%
        self$linear3() %>%
        nnf_relu() %>%
        self$linear4() %>%
        nnf_sigmoid()
      
    }
)

model <- net()


model$forward(train_ds[10]$x_cont)
model$forward(torch_rand(96))


fitted <- 
  net %>%
  setup(
    loss = nnf_binary_cross_entropy,
    optimizer = optim_adam,
    metrics = list(luz_metric_binary_auroc(num_thresholds = 2000))
  ) %>%
  fit(train_dl, epochs = 5, valid_data = valid_dl)

preds <- predict(fitted, test_dl)







# ===============================================================================================================================

train_indices <- 1:10000
val_indices <- 10001:15000
test_indices <- 15001:20000



add_channel_dim <- function(img) img$unsqueeze(1)
crop_axes <- function(img) transform_crop(img, top = 0, left = 21, height = 131, width = 130)

root <- file.path(tempdir(), "correlation")

train_ds <- guess_the_correlation_dataset(
  # where to unpack
  root = root,
  # additional preprocessing 
  transform = function(img) crop_axes(img) %>% add_channel_dim(),
  # don't take all data, but just the indices we pass in
  indexes = train_indices,
  download = TRUE
)


valid_ds <- guess_the_correlation_dataset(
  root = root,
  transform = function(img) crop_axes(img) %>% add_channel_dim(),
  indexes = val_indices,
  download = FALSE
)

test_ds <- guess_the_correlation_dataset(
  root = root,
  transform = function(img) crop_axes(img) %>% add_channel_dim(),
  indexes = test_indices,
  download = FALSE
)

length(train_ds)
length(valid_ds)
length(test_ds)


train_ds[1]

# ===============================================================================================================================

train_dl <- dataloader(train_ds, batch_size = 64, shuffle = TRUE)

length(train_dl)

batch <- dataloader_make_iter(train_dl) %>% dataloader_next()

dim(batch$x)
dim(batch$y)


par(mfrow = c(8,8), mar = rep(0, 4))

images <- as.array(batch$x$squeeze(2))

images %>%
  purrr::array_tree(1) %>%
  purrr::map(as.raster) %>%
  purrr::iwalk(~{plot(.x)})


valid_dl <- dataloader(valid_ds, batch_size = 64)
length(valid_dl)

test_dl <- dataloader(test_ds, batch_size = 64)
length(test_dl)


# ===============================================================================================================================

torch_manual_seed(777)

net <- nn_module(
  
  "corr-cnn",
  
  initialize = function() {
    
    self$conv1 <- nn_conv2d(in_channels = 1, out_channels = 32, kernel_size = 3)
    self$conv2 <- nn_conv2d(in_channels = 32, out_channels = 64, kernel_size = 3)
    self$conv3 <- nn_conv2d(in_channels = 64, out_channels = 128, kernel_size = 3)
    
    self$fc1 <- nn_linear(in_features = 14 * 14 * 128, out_features = 128)
    self$fc2 <- nn_linear(in_features = 128, out_features = 1)
    
  },
  
  forward = function(x) {
    
    x %>% 
      self$conv1() %>%
      nnf_relu() %>%
      nnf_avg_pool2d(2) %>%
      
      self$conv2() %>%
      nnf_relu() %>%
      nnf_avg_pool2d(2) %>%
      
      self$conv3() %>%
      nnf_relu() %>%
      nnf_avg_pool2d(2) %>%
      
      torch_flatten(start_dim = 2) %>%
      self$fc1() %>%
      nnf_relu() %>%
      
      self$fc2()
  }
)


model <- net()
model(batch$x)

# ===============================================================================================================================

fitted <- 
  net %>%
  setup(
    loss = function(y_hat, y_true) nnf_mse_loss(y_hat, y_true$unsqueeze(2)),
    optimizer = optim_adam
  ) %>%
  fit(train_dl, epochs = 10, valid_data = test_dl)


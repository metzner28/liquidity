library(tidyverse)
library(readxl)
library(keras)
setwd("~/Dropbox/Documents/yale/senior/ECON_491_492/data/final datasets")

kiley = read_excel("kiley_labeled.xlsx") %>%
  select_at(3:29) %>%
  mutate_all(as.numeric) 

kiley_stationary = data.frame(sapply(kiley, diff)) %>%
  select_at(-1) %>%
  mutate_all(scale)

.lagTED = log(kiley$outcome_TEDspread)[2:91]
x2 = data.matrix(cbind(kiley_stationary[1:90,], .lagTED))
y2 = log(kiley$outcome_TEDspread)[3:92]
x2_test = data.matrix(cbind(kiley_stationary[91:116,], log(kiley$outcome_TEDspread)[92:117]))
y2_test = log(kiley$outcome_TEDspread)[93:118]

fit_nn = function(n1, n2, lr, layers, dr1, dr2, act1, act2) {
  
  model = keras_model_sequential()
  if (layers == 1) {
    model %>%
      layer_dense(units = n1, activation = 'sigmoid', input_shape = c(27)) %>%
      layer_dropout(dr1) %>%
      layer_dense(units = 1)
  } else if (layers == 2) {
    model %>%
      layer_dense(units = n1, activation = act1, input_shape = c(27)) %>%
      layer_dropout(dr1) %>%
      layer_dense(units = n2, activation = act2) %>%
      layer_dropout(dr2) %>%
      layer_dense(units = 1)
  } else stop('choose 1 or 2 layers!')
  
  model %>% compile(loss = "mse", optimizer = optimizer_sgd(lr = lr), 
                    metrics = list("mean_absolute_error"))
  
  early_stop = callback_early_stopping(monitor = "val_loss", patience = 15)
  history = model %>% fit(x2, y2, epochs = 100, verbose = 0, 
                          validation_data = list(x2_test, y2_test),
                          metrics = list("mean_absolute_error"),
                          callbacks = early_stop)
  mae = tail(history$metrics$val_mean_absolute_error, n = 1)
  return(mae)
}

fit_nn(n1 = 12, n2 = 6, act1 = 'sigmoid', act2 = 'sigmoid',
       lr = 0.025, layers = 2, dr1 = 0.4, dr2 = 0.4)




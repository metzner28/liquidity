library(tidyverse)
library(tseries)
library(forecast)
library(readxl)
library(glmnet)
setwd("~/Dropbox/Documents/yale/senior/ECON_491_492/data/final datasets")

### fit lasso model to lag TED and PCs

kiley = read_excel("kiley_labeled.xlsx") %>%
  select_at(3:29) %>%
  mutate_all(as.numeric) 

trends = sapply(kiley, ndiffs, alpha = 0.05, test = "adf")

kiley_stationary = data.frame(sapply(kiley, diff)) %>%
  select_at(-1) %>%
  mutate_all(scale)

kileyPCA = prcomp(kiley_stationary)

x_train = cbind(log(kiley$outcome_TEDspread[1:90]), kileyPCA$x[1:90,])
x_test = cbind(log(kiley$outcome_TEDspread[91:117]), kileyPCA$x[91:117,])
y_train = log(kiley$outcome_TEDspread[2:91])
y_test = log(kiley$outcome_TEDspread[92:118])

seq_lambda = seq(0.1, 0, by = -0.001)
mae_train = vector(mode = 'numeric', length = 100)
mae_test = vector(mode = 'numeric', length = 100)
lasso1 = glmnet(x_train, y_train, type.measure = "mse", alpha = 1, lambda = seq_lambda)
for (lambda in seq_lambda) {
  pred_train = predict(lasso1, s = lambda, newx = x_train)
  pred_test = predict(lasso1, s = lambda, newx = x_test)
  mae_train[which(seq_lambda == lambda)] = mean(abs(pred_train - y_train))
  mae_test[which(seq_lambda == lambda)] = mean(abs(pred_test - y_test))
}

df = data.frame('lambda' = seq_lambda, mae_train, mae_test) %>%
  pivot_longer(2:3, names_to = "series", values_to = "mae")

ggplot(df) +
  geom_line(aes(x = lambda, y = mae, color = series)) +
  theme_minimal()


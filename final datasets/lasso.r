library(tidyverse)
library(tseries)
library(forecast)
library(readxl)
library(glmnet)
setwd("~/Dropbox/Documents/yale/senior/ECON_491_492/data/final datasets")

### fit lasso model directly on lag of TED and matrix of differenced indicators

kiley = read_excel("kiley_labeled.xlsx") %>%
  select_at(3:29) %>%
  mutate_all(as.numeric) 

trends = sapply(kiley, ndiffs, alpha = 0.05, test = "adf")

kiley_stationary = data.frame(sapply(kiley, diff)) %>%
  select_at(-1) %>%
  mutate_all(scale)

x2 = data.matrix(cbind(kiley_stationary[1:90,], log(kiley$outcome_TEDspread)[1:90]))
x2_test = data.matrix(cbind(kiley_stationary[91:116,], log(kiley$outcome_TEDspread)[91:116]))
y2 = log(kiley$outcome_TEDspread)[3:92]
y2_test = log(kiley$outcome_TEDspread)[93:118]

# try ols as a baseline
model_ols = lm(y2 ~ ., data = data.frame(x2))
colnames(x2_test) = colnames(x2)
pred_ols = predict(model_ols, newdata = data.frame(x2_test))
mae_ols = mean(abs(y2_test - pred_ols))

# see if ar(p) is any better [it isn't]
model_arima = auto.arima(y2)
pred_ar = predict(model_ar, n.ahead = 27)
mae_ar = mean(abs(y2_test - pred_ar))

# try auto.arima to really cover all bases [this just gives an ar1 regardless]


# now fit the lasso with 100 values of lambda
seq_lambda = seq(0.1, 0, by = -0.001)
mae_train = vector(mode = 'numeric', length = 100)
mae_test = vector(mode = 'numeric', length = 100)
lasso1 = glmnet(x2, y2, type.measure = "mse", alpha = 1, lambda = seq_lambda)
for (lambda in seq_lambda) {
  pred_train = predict(lasso1, s = lambda, newx = x2)
  pred_test = predict(lasso1, s = lambda, newx = x2_test)
  mae_train[which(seq_lambda == lambda)] = mean(abs(pred_train - y2))
  mae_test[which(seq_lambda == lambda)] = mean(abs(pred_test - y2_test))
}

df = data.frame('lambda' = seq_lambda, mae_train, mae_test) %>%
  pivot_longer(2:3, names_to = "series", values_to = "mae")

ggplot(df) +
  geom_line(aes(x = lambda, y = mae, color = series)) +
  theme_minimal()

coef = coef(lasso1, s = seq_lambda[which.min(mae_test)])
sum(coef != 0)
min(mae_test)
r2 = lasso1$dev.ratio[which.min(mae_test)]
pct_improvement_log = (mae_ar - min(mae_test)) / mae_ar
pct_improvement = (exp(mae_ar) - exp(min(mae_test))) / exp(mae_ar)



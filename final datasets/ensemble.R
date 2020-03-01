library(tidyverse)
library(readxl)
library(xgboost)
library(ranger)
setwd("~/Dropbox/Documents/yale/senior/ECON_491_492/data/final datasets")

# try random forest
kiley = read_excel("kiley_labeled.xlsx") %>%
  select_at(3:29) %>%
  mutate_all(as.numeric) %>%
  mutate(outcome_TEDspread = log(outcome_TEDspread))

idx = sample(nrow(kiley), size = 0.8 * nrow(kiley))
kiley_train = kiley[idx,]
kiley_test = kiley[-idx,]

# try rf
mse_rf = vector(mode = 'numeric', length = 5)
for (i in seq(100, 500, by = 100)) {
  rf_TED = ranger(outcome_TEDspread ~ ., data = kiley_train, 
                  num.trees = i, max.depth = 2, min.node.size = 5, 
                  importance = 'impurity')
  pred_rf_TED = predict(rf_TED, data = kiley_test)
  mse_rf[i/100] = mean((pred_rf_TED$predictions - kiley_test$outcome_TEDspread)**2)
}
imp = sort(importance(rf_TED), decreasing = TRUE)

# try xgboost
kiley_train_xgb = model.matrix(outcome_TEDspread ~ . - 1, data = kiley_train)
kiley_test_xgb = model.matrix(outcome_TEDspread ~ . - 1, data = kiley_test)
kiley_xgb = model.matrix(outcome_TEDspread ~ . - 1, data = kiley)
CV_TED = xgb.cv(data = kiley_train_xgb, label = kiley_train$outcome_TEDspread,
                 params = list(objective = 'reg:squarederror', eta = 0.1, max.depth = 6), 
                 nrounds = 100, nfold = 5)
dfCV = data.frame(CV_TED$evaluation_log)
opt = dfCV$iter[which.min(dfCV$test_rmse_mean)]

xgb_ted = xgboost(data = kiley_train_xgb, label = kiley_train$outcome_TEDspread,
                  params = list(objective = 'reg:squarederror', max_depth = 6),
                  eta = 0.1, nrounds = 50, verbose = FALSE)
pred_xgb_TED = predict(xgb_ted, newdata = kiley_test_xgb)
mse_xgb = mean((pred_xgb_TED - kiley_test$outcome_TEDspread)**2)

pred_series_rf = predict(rf_TED, data = kiley)
pred_series_xgb = predict(xgb_ted, newdata = kiley_xgb)
df_TED = data.frame(kiley$outcome_TEDspread, pred_series_rf$predictions, pred_series_xgb) %>%
  mutate(period = seq(1990.25, 2019.5, by = 0.25)) %>%
  pivot_longer(1:3, names_to = "pred", values_to = "value")

ggplot(df_TED) +
  geom_line(aes(x = period, y = value, color = pred)) +
  theme_minimal()


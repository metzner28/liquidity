library(tidyverse)
library(tseries)
library(forecast)
library(readxl)
library(ggfortify)
library(pls)
library(glmnet)
setwd("~/Dropbox/Documents/yale/senior/ECON_491_492/data/final datasets")

kiley = read_excel("kiley_labeled.xlsx") %>%
  select_at(3:29) %>%
  mutate_all(as.numeric) 

trends = sapply(kiley, ndiffs, alpha = 0.05, test = "adf")

kiley_stationary = data.frame(sapply(kiley, diff)) %>%
  select_at(-1) %>%
  mutate_all(scale)

# are we seeing clusters that correspond to elevated values of the ted spread? let's color the plot by the outcome to check
kileyPCA = prcomp(kiley_stationary)
summary(kileyPCA)
dfPCA = as_tibble(kileyPCA$rotation) %>%
  mutate(feature = rownames(kileyPCA$rotation)) %>%
  arrange(-PC1)

# check log ted spread
outcome_TED = log(kiley$outcome_TEDspread[2:118])
df_outcome = cbind(kiley_stationary, outcome_TED)
autoplot(kileyPCA, data = df_outcome, colour = 'outcome_TED') +
  theme_minimal()

# check fss
FSS = read_csv('FSS_scores_all.csv')
outcome_FSS = (FSS$score[2:118] - mean(FSS$score[2:118]))/sd(FSS$score[2:118])
df_FSS = cbind(kiley_stationary, outcome_FSS)

autoplot(kileyPCA, data = df_FSS, colour = 'outcome_FSS') +
  theme_minimal()

### fit pc and pls regression models
kiley_TED = cbind(kiley_stationary, outcome_TED)
kiley_FSS = cbind(kiley_stationary, outcome_FSS)

# train-test split
idx = sample(nrow(kiley_TED), size = 0.8 * nrow(kiley_TED))
kiley_TED_train = kiley_TED[idx,]
kiley_TED_test = kiley_TED[-idx,]

# fit models
pc_TED = pcr(outcome_TED ~., data = kiley_TED_train, validation = "LOO")
validationplot(pc_TED)
mse_train = as.numeric(t(data.frame(MSEP(pc_TED)$val)[2,]))[2:27]

# minimize test MSE (still really frickin high)
mse = vector(mode = 'numeric', length = length(1:26))
for (i in 1:26) {
  pred_TED = predict(pc_TED, ncomp = i, newdata = kiley_TED_test)
  mse[i] = mean((pred_TED - kiley_TED_test$outcome_TED)**2)
}

# plot train and test loss
error = data.frame(mse_train, mse, 1:26) 
colnames(error) = c("train", "test", "components")
error_plot = error %>%
  pivot_longer(cols = c(train, test), names_to = "error", values_to = "value")

ggplot(error_plot) +
  geom_line(aes(x = components, y = value, color = error)) +
  theme_minimal()

opt = which.min(mse)
pred_pc = predict(pc_TED, ncomp = opt, newdata = kiley_stationary)
df = data.frame(period = seq(1990.5, 2019.5, by = 0.25), outcome_TED, pred_pc) %>%
  pivot_longer(2:3, names_to = "pred", values_to = "value")
                
ggplot(df) +
  geom_line(aes(x = period, y = value, color = pred)) +
  theme_minimal()


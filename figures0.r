library(tidyverse)
library(readxl)
library(lubridate)
library(ggplot2)
library(readxl)
library(glmnet)
library(stargazer)
library(keras)
library(sandwich)
setwd("~/Dropbox/Documents/yale/senior/ECON_491_492/drafts")

# fig 1 - graphs of indicators
kiley = read_excel("kiley_fig1.xlsx")

kiley0 =  kiley %>%
  select_at(3:30) %>%
  mutate_all(as.numeric)

kiley1 = kiley0 %>%
  mutate_all(scale) %>%
  mutate(date = kiley$date) %>%
  pivot_longer(1:28, names_to = "indicator") %>%
  separate(indicator, into = c("group", "series"), sep = "_", convert = TRUE) %>%
  mutate(date = case_when(
    str_detect(date, "1q") ~ str_c("3/30/", str_sub(date, 3, -1)), 
    str_detect(date, "2q") ~ str_c("6/30/", str_sub(date, 3, -1)),
    str_detect(date, "3q") ~ str_c("9/30/",str_sub(date, 3, -1)),
    str_detect(date, "4q") ~ str_c("12/30/", str_sub(date, 3, -1)))) %>%
  mutate(date = mdy(date))

ggplot(kiley1) +
  geom_line(aes(x = date, y = value, color = series)) +
  facet_wrap(vars(group), nrow = 4, scales = "free_y") +
  guides(color = guide_legend(ncol = 1)) +
  theme_minimal()
ggsave('fig1.pdf', device = "pdf", scale = 2, dpi = "retina")

# fig 2 - aikman figure
aikman = read_excel("aikman_labeled_NEW.xlsx")

aikman0 =  aikman %>%
  select(-c(1:2)) %>%
  mutate_all(as.numeric)

aikman1 = aikman0 %>%
  mutate_all(scale) %>%
  mutate(date = aikman$Date) %>%
  pivot_longer(1:18, names_to = "indicator") %>%
  separate(indicator, into = c("group", "series"), sep = "_", convert = TRUE) %>%
  mutate(date = case_when(
    str_detect(date, "Q1") ~ str_c("3/30/", str_sub(date, 1, 4)), 
    str_detect(date, "Q2") ~ str_c("6/30/", str_sub(date, 1, 4)),
    str_detect(date, "Q3") ~ str_c("9/30/",str_sub(date, 1, -4)),
    str_detect(date, "Q4") ~ str_c("12/30/", str_sub(date, 1, 4)))) %>%
  mutate(date = mdy(date))

ggplot(aikman1) +
  geom_line(aes(x = date, y = value, color = series)) +
  facet_wrap(vars(group), nrow = 4, scales = "free_y") +
  guides(color = guide_legend(ncol = 1)) +
  theme_minimal()
ggsave('fig2.pdf', device = "pdf", scale = 2, dpi = "retina")

# fig 3 - pca loadings
rm(list=ls())
kiley = read_excel("kiley_labeled.xlsx") %>%
  select_at(3:29) %>%
  mutate_all(as.numeric) 

# test and assure stationarity, scale to zero mean and unit variance
kiley_stationary = data.frame(sapply(kiley, diff)) %>%
  select_at(-1) %>%
  mutate_all(scale)

### PCA
kileyPCA = prcomp(kiley_stationary)
dfPCA = as_tibble(kileyPCA$rotation) %>%
  mutate(feature = rownames(kileyPCA$rotation)) %>%
  arrange(-PC1) %>%
  select_at(c(1:3, 27)) %>%
  pivot_longer(1:3, names_to = 'component', values_to = 'loading')

# heatmap of loadings
ggplot(dfPCA) +
  geom_tile(aes(x = feature, y = component, fill = loading)) +
  scale_fill_distiller(palette = "RdBu") +
  coord_equal() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 9))
ggsave('fig3.pdf', device = "pdf", scale = 2, dpi = "retina")

# fig 4 scatterplot - ted
outcome_TED = log(kiley$outcome_TEDspread[2:118])
df_outcome = cbind(kileyPCA$x, outcome_TED)
ggplot(data.frame(df_outcome)) +
  geom_point(aes(x = PC1, y = PC2, color = outcome_TED), size = 3) +
  scale_color_viridis_c() +
  labs(x = "PC1 (18.79%)", y = "PC2 (14.79%)") +
  theme_minimal()
ggsave('fig4.pdf', device = "pdf", scale = 2, dpi = "retina")

# fig 5 scatterplot - fss
kileyFSS = read_excel("kiley_labeled_FSS.xlsx") %>%
  select_at(3:29) %>%
  mutate_all(as.numeric) 
kileyFSS_stationary = data.frame(sapply(kileyFSS, diff)) %>%
  select_at(-1) %>%
  mutate_all(scale)
outcome_FSS = scale(kileyFSS$outcome_FSS[2:118])
df_outcome = cbind(kileyPCA$x, outcome_FSS)
ggplot(data.frame(df_outcome)) +
  geom_point(aes(x = PC1, y = PC2, color = outcome_FSS), size = 3) +
  scale_color_viridis_c() +
  labs(x = "PC1 (18.79%)", y = "PC2 (14.79%)") +
  theme_minimal()
ggsave('fig5.pdf', device = "pdf", scale = 2, dpi = "retina")

## fig 6-7 model fitting and results
rm(list=ls())
kiley = read_excel("kiley_labeled.xlsx") %>%
  select_at(3:29) %>%
  mutate_all(as.numeric) 
kiley_stationary = data.frame(sapply(kiley, diff)) %>%
  select_at(-1) %>%
  mutate_all(scale)

kileyFSS = read_excel("kiley_labeled_FSS.xlsx") %>%
  select_at(3:29) %>%
  mutate_all(as.numeric) 
kileyFSS_stationary = data.frame(sapply(kileyFSS, diff)) %>%
  select_at(-1) %>%
  mutate_all(scale)

# TED model
.lagTED = log(kiley$outcome_TEDspread)[2:91]
x2_TED = data.matrix(cbind(kiley_stationary[1:90,], .lagTED))
outcome_TED = log(kiley$outcome_TEDspread)[3:92]
x2_test_TED = data.matrix(cbind(kiley_stationary[91:116,], log(kiley$outcome_TEDspread)[92:117]))
y2_test_TED = log(kiley$outcome_TEDspread)[93:118]

model_ar_TED = lm(outcome_TED ~ .lagTED)
df_ar_TED = data.frame(.lagTED = log(kiley$outcome_TEDspread)[92:117])
pred_ar_TED = predict(model_ar_TED, newdata = df_ar_TED)
mse_ar_TED = mean((y2_test_TED - pred_ar_TED)**2)

seq_lambda = seq(0.1, 0, by = -0.001)
mse_train_TED = vector(mode = 'numeric', length = 100)
mse_test_TED = vector(mode = 'numeric', length = 100)
lasso_TED = glmnet(x2_TED, outcome_TED, type.measure = "mse", alpha = 1, lambda = seq_lambda)
for (lambda in seq_lambda) {
  pred_train = predict(lasso_TED, s = lambda, newx = x2_TED)
  pred_test = predict(lasso_TED, s = lambda, newx = x2_test_TED)
  mse_train_TED[which(seq_lambda == lambda)] = mean((pred_train - outcome_TED)**2)
  mse_test_TED[which(seq_lambda == lambda)] = mean((pred_test - y2_test_TED)**2)
}

df = data.frame('lambda' = seq_lambda, mse_train_TED, mse_test_TED) %>%
  pivot_longer(2:3, names_to = "series", values_to = "mse")
ggplot(df) +
  geom_line(aes(x = lambda, y = mse, color = series)) +
  geom_hline(aes(yintercept = mse_ar_TED, linetype = 'mse_ar')) +
  scale_linetype_manual(name = "baseline", values = "dashed") +
  theme_minimal()
ggsave("fig6.pdf", device = "pdf", width = 6, height = 2.75, dpi = "retina")
pct_improvement_TED = (mse_ar_TED - min(mse_test_TED)) / mse_ar_TED

# FSS model
.lagFSS = scale(kileyFSS$outcome_FSS)[2:91]
x2_FSS = data.matrix(cbind(kileyFSS_stationary[1:90,], .lagFSS))
outcome_FSS = scale(kileyFSS$outcome_FSS)[3:92]
x2_test_FSS = data.matrix(cbind(kileyFSS_stationary[91:116,], scale(kileyFSS$outcome_FSS)[92:117]))
y2_test_FSS = scale(kileyFSS$outcome_FSS)[93:118]

model_ar_FSS = lm(outcome_FSS ~ .lagFSS)
df_ar_FSS = data.frame(.lagFSS = scale(kileyFSS$outcome_FSS)[92:117])
pred_ar_FSS = predict(model_ar_FSS, newdata = df_ar_FSS)
mse_ar_FSS = mean((y2_test_FSS - pred_ar_FSS)**2)

seq_lambda = seq(0.2, 0, by = -0.001)
mse_train_FSS = vector(mode = 'numeric', length = 100)
mse_test_FSS = vector(mode = 'numeric', length = 100)
lasso_FSS = glmnet(x2_FSS, outcome_FSS, type.measure = "mse", alpha = 1, lambda = seq_lambda)
for (lambda in seq_lambda) {
  pred_train = predict(lasso_FSS, s = lambda, newx = x2_FSS)
  pred_test = predict(lasso_FSS, s = lambda, newx = x2_test_FSS)
  mse_train_FSS[which(seq_lambda == lambda)] = mean((pred_train - outcome_FSS)**2)
  mse_test_FSS[which(seq_lambda == lambda)] = mean((pred_test - y2_test_FSS)**2)
}

df = data.frame('lambda' = seq_lambda, mse_train_FSS, mse_test_FSS) %>%
  pivot_longer(2:3, names_to = "series", values_to = "mse")
ggplot(df) +
  geom_line(aes(x = lambda, y = mse, color = series)) +
  geom_hline(aes(yintercept = mse_ar_FSS, linetype = 'mse_ar')) +
  scale_linetype_manual(name = "baseline", values = "dashed") +
  theme_minimal()
#ggsave("fig7.pdf", device = "pdf", width = 6, height = 2.75, dpi = "retina")
pct_improvement_FSS = (mse_ar_FSS - min(mse_test_FSS)) / mse_ar_FSS

# coefficients table
df_coef_TED = enframe(data.matrix(coef(lasso_TED))[,which.min(mse_test_TED)])
df_coef_FSS = enframe(data.matrix(coef(lasso_FSS))[,which.min(mse_test_FSS)])
coef_ar_TED = c(coef(model_ar_TED)[1], rep(NA, 26), coef(model_ar_TED)[2])
coef_ar_FSS = c(coef(model_ar_FSS)[1], rep(NA, 26), coef(model_ar_FSS)[2])
  
df_coef = cbind(df_coef_TED, coef_ar_TED, df_coef_FSS$value, coef_ar_FSS) %>%
  separate(name, sep = "_", into = c("Class", "Indicator"), fill = 'left') %>%
  rename('log(TED)' = value, 'FSS'= 'df_coef_FSS$value', 'Baseline (TED)' = coef_ar_TED, 'Baseline (FSS)' = coef_ar_FSS) %>%
  mutate(order = c(1, 3:28, 2)) %>%
  arrange(order) %>%
  select(-order) %>%
  mutate(Indicator = replace(Indicator, Indicator == ".lagTED", ".lag")) %>%
  na_if(0)

# results table
names = c("Observations", "R2", "Test MSE", "Improvement (%)")
results_TED = c(90L, lasso_TED$dev.ratio[which.min(mse_test_TED)], min(mse_test_TED), pct_improvement_TED *100)
results_ar_TED = c(90L, summary(model_ar_TED)$r.squared, mse_ar_TED, NA)
results_FSS = c(90L, lasso_FSS$dev.ratio[which.min(mse_test_FSS)], min(mse_test_FSS), pct_improvement_FSS *100)
results_ar_FSS = c(90L, summary(model_ar_FSS)$r.squared, mse_ar_FSS, NA)
df_results = cbind(names, rep(NA,4), results_TED, results_ar_TED, results_FSS, results_ar_FSS)
colnames(df_results) = colnames(df_coef)

### table 1 - final model results
df_final = rbind(df_coef, df_results) %>%
  mutate_at(vars(3:6), ~as.numeric(.)) %>%
  mutate_if(is.numeric, ~round(.,4))
#stargazer(df_final, summary = FALSE, rownames = FALSE, font.size = 'small')

# fig 8-9 predictions
df_pred_TED = data.frame(predict(lasso_TED, s = seq_lambda[which.min(mse_test_TED)], newx = x2_test_TED), pred_ar_TED, y2_test_TED) %>%
                mutate(time = seq(2013.25, 2019.5, by = 0.25))
colnames(df_pred_TED) = c('lasso', 'AR_1', 'trueTED', 'time')
df_pred_TED = df_pred_TED %>%
  pivot_longer(1:3, names_to = 'series', values_to = 'value')
ggplot(df_pred_TED) +
  geom_line(aes(x = time, y = value, color = series)) +
  theme_minimal()
ggsave('fig8.pdf', width = 6, height = 2.75, dpi = 'retina')

df_pred_FSS = data.frame(predict(lasso_FSS, s = seq_lambda[which.min(mse_test_FSS)], newx = x2_test_FSS), pred_ar_FSS, y2_test_FSS) %>%
  mutate(time = seq(2013.25, 2019.5, by = 0.25))
colnames(df_pred_FSS) = c('lasso', 'AR_1', 'trueFSS', 'time')
df_pred_FSS = df_pred_FSS %>%
  pivot_longer(1:3, names_to = 'series', values_to = 'value')
ggplot(df_pred_FSS) +
  geom_line(aes(x = time, y = value, color = series)) +
  theme_minimal()
ggsave('fig9.pdf', width = 6, height = 2.75, dpi = 'retina')

# hypothesis testing
coefs_TED = data.matrix(coef(lasso_TED))[,which.min(mse_test_TED)]
coefs_TED = names(coefs_TED[coefs_TED != 0])[-1]
formula_TED = paste("outcome_TED ~", paste(coefs_TED, collapse = "+"))
model_ht_TED = lm(as.formula(formula_TED), data = data.frame(x2_TED))
se_TED = sqrt(diag(NeweyWest(model_ht_TED)))

coefs_FSS = data.matrix(coef(lasso_FSS))[,which.min(mse_test_FSS)]
coefs_FSS = names(coefs_FSS[coefs_FSS != 0])[-1]
formula_FSS = paste("outcome_FSS ~", paste(coefs_FSS, collapse = "+"))
model_ht_FSS = lm(as.formula(formula_FSS), data = data.frame(x2_FSS))
se_FSS= sqrt(diag(NeweyWest(model_ht_FSS)))

colnames(x2_test_TED)[27] = '.lagTED'
pred_ht_TED = predict(model_ht_TED, newdata = data.frame(x2_test_TED))
mse_ht_test_TED = mean((y2_test_TED - pred_ht_TED)**2)

colnames(x2_test_FSS)[27] = '.lagFSS'
pred_ht_FSS = predict(model_ht_FSS, newdata = data.frame(x2_test_FSS))
mse_ht_test_FSS = mean((y2_test_FSS - pred_ht_FSS)**2)

stargazer(model_ht_TED, model_ht_FSS, report = "vc*s", se = list(se_TED, se_FSS),
          font.size = 'scriptsize', dep.var.labels = c("log(TED)", "FSS"),
          add.lines = list(c('Test MSE', round(mse_ht_test_TED, 3), round(mse_ht_test_FSS,3))))

### neural network
fit_nn = function(x_train, y_train, x_test, y_test, n1, n2, layers, dr1, dr2, act1, act2) {
  
  model = keras_model_sequential()
  if (layers == 1) {
    model %>%
      layer_dense(units = n1, activation = act1, input_shape = c(27)) %>%
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
  
  model %>% compile(loss = "mse", optimizer = optimizer_sgd(lr = 0.025), 
                    metrics = list("mean_squared_error"))
  
  early_stop = callback_early_stopping(monitor = "val_loss", patience = 15)
  history = model %>% fit(x_train, y_train, epochs = 100, verbose = 0, 
                          validation_data = list(x_test, y_test),
                          metrics = list("mean_squared_error"),
                          callbacks = early_stop)
  mse = tail(history$metrics$val_mean_squared_error, n = 1)
  return(mse)
}

# nn results tables 3 and 4
nn_args = read_excel('nn_args.xlsx')
set.seed(6440) # 6440
nn_args = nn_args %>%
  mutate(mse_TED = mapply(fit_nn, n1, n2, layers, dr1, dr2, act1, act2,
                          MoreArgs = list(x_train = x2_TED, x_test = x2_test_TED, 
                                          y_train = outcome_TED, y_test = y2_test_TED)),
         mse_FSS = mapply(fit_nn, n1, n2, layers, dr1, dr2, act1, act2,
                          MoreArgs = list(x_train = x2_FSS, x_test = x2_test_FSS, 
                                          y_train = outcome_FSS, y_test = y2_test_FSS)),
         pct_TED = (mse_ar_TED - mse_TED) / mse_ar_TED,
         pct_FSS = (mse_ar_FSS - mse_FSS) / mse_ar_FSS
  )

nn_table = nn_args %>%
  select(-layers) %>%
  mutate(pct_TED = ifelse(pct_TED < 0, NA, pct_TED),
         pct_FSS = ifelse(pct_FSS < 0, NA, pct_FSS)) %>%
  na_if(0) %>%
  na_if('na') %>%
  mutate_at(vars(7:10), ~round(.,3)) %>%
  mutate_at(vars(9:10), ~.*100)

colnames(nn_table) = c('Units (1)', 'Units (2)', 'Dropout (1)', 'Dropout (2)',
                       'Activation (1)', 'Activation (2)', 'Test MSE log(TED)', 'Test MSE FSS',
                       'Pct Improvement log(TED)', 'Pct Improvement FSS')

nn_arch = nn_table %>%
  select(1:6) %>%
  mutate(Architecture = row_number()) %>%
  select(Architecture, 1:6) %>%
  mutate_all(as.character)

nn_results = nn_table %>%
  select(7:10) %>%
  mutate(Architecture = row_number()) %>%
  select(Architecture, 1, 3, 2, 4) %>%
  mutate_all(as.character)

stargazer(nn_arch, summary = FALSE, rownames = FALSE, font.size = 'scriptsize')
stargazer(nn_results, summary = FALSE, rownames = FALSE, font.size = 'scriptsize')

# predictions, figures 10 and 11
early_stop = callback_early_stopping(monitor = "val_loss", patience = 15)
model_TED = keras_model_sequential() %>%
  layer_dense(units =  16, input_shape = c(27)) %>%
  layer_dropout(0.25) %>%
  layer_dense(units = 1)
model_TED %>% compile(loss = "mse", optimizer = optimizer_sgd(lr = 0.025), 
                      metrics = list("mean_squared_error"))
history_TED  = model_TED %>% fit(x2_TED, outcome_TED, epochs = 100, verbose = 0, 
                                 validation_data = list(x2_test_TED, y2_test_TED),
                                 metrics = list("mean_squared_error"),
                                 callbacks = early_stop)

pred_nn_TED = predict(model_TED, x2_test_TED)
df_pred_nn_TED = data.frame(seq(2013.25, 2019.5, by = 0.25), pred_nn_TED, y2_test_TED, pred_ar_TED)
colnames(df_pred_nn_TED) = c('time', 'nn', 'trueTED', 'baseline')
df_pred_nn_TED = df_pred_nn_TED %>%
  pivot_longer(2:4, names_to = 'series', values_to = 'value')
ggplot(df_pred_nn_TED) +
  geom_line(aes(x = time, y = value, color = series)) +
  theme_minimal()
ggsave('fig10.pdf', width = 6, height = 2.75, dpi = 'retina')

model_FSS = keras_model_sequential() %>%
  layer_dense(units =  8, input_shape = c(27)) %>%
  layer_dropout(0.25) %>%
  layer_dense(units = 8) %>%
  layer_dropout(0.25) %>%
  layer_dense(units = 1)
model_FSS %>% compile(loss = "mse", optimizer = optimizer_sgd(lr = 0.025), 
                      metrics = list("mean_squared_error"))
history_FSS  = model_FSS %>% fit(x2_FSS, outcome_FSS, epochs = 100, verbose = 0, 
                                 validation_data = list(x2_test_FSS, y2_test_FSS),
                                 metrics = list("mean_squared_error"),
                                 callbacks = early_stop)

pred_nn_FSS = predict(model_FSS, x2_test_FSS)
df_pred_nn_FSS = data.frame(seq(2013.25, 2019.5, by = 0.25), pred_nn_FSS, y2_test_FSS, pred_ar_FSS)
colnames(df_pred_nn_FSS) = c('time', 'nn', 'trueFSS', 'baseline')
df_pred_nn_FSS = df_pred_nn_FSS %>%
  pivot_longer(2:4, names_to = 'series', values_to = 'value')
ggplot(df_pred_nn_FSS) +
  geom_line(aes(x = time, y = value, color = series)) +
  theme_minimal()
ggsave('fig11.pdf', width = 6, height = 2.75, dpi = 'retina')

### UK data
rm(list=ls())

aikman = read_excel("aikman_labeled_NEW.xlsx") %>%
  select_at(-c(1:2)) %>%
  mutate_all(as.numeric)

aikman_stationary = data.frame(sapply(aikman, diff)) %>%
  select_at(-1) %>%
  mutate_all(scale)

aikmanPCA = prcomp(aikman_stationary)
summary(aikmanPCA)
dfPCA = as_tibble(aikmanPCA$rotation) %>%
  mutate(feature = rownames(aikmanPCA$rotation),
         PC1 = -PC1) %>% # see below for why we flip these loadings
  arrange(-PC1) %>%
  select_at(c(1:3, 18)) %>%
  pivot_longer(1:3, names_to = 'component', values_to = 'loading')

# heatmap of loadings
ggplot(dfPCA) +
  geom_tile(aes(x = feature, y = component, fill = loading)) +
  scale_fill_distiller(palette = "RdBu") +
  coord_equal() +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 9))
ggsave('fig12.pdf', device = "pdf", scale = 2, dpi = "retina")

# first 2 PCs by log libor-repo
outcome_LR = log(aikman$outcome_LiborRepo[2:86])
df_outcome = data.frame(cbind(aikmanPCA$x, outcome_LR)) %>%
  mutate(PC1 = -PC1) # PCA is unique up to a sign, this is allowed bc we flipped the PC1 loadings as well
ggplot(df_outcome) +
  geom_point(aes(x = PC1, y = PC2, color = outcome_LR), size = 3) +
  scale_color_viridis_c() +
  labs(x = "PC1 (25.63%)", y = "PC2 (15.77%)") +
  theme_minimal()
ggsave('fig13.pdf', device = "pdf", scale = 2, dpi = "retina")

### model fitting
LR = log(aikman$outcome_LiborRepo)
.lagLR = LR[2:77]
x2_LR = data.matrix(cbind(aikman_stationary[1:76,], .lagLR))
outcome_LR = LR[3:78]
x2_test_LR = data.matrix(cbind(aikman_stationary[77:84,], LR[78:85]))
y2_test_LR = LR[79:86]

model_ar_LR = lm(outcome_LR ~ .lagLR)
df_ar_LR = data.frame(.lagLR = LR[78:85])
pred_ar_LR = predict(model_ar_LR, newdata = df_ar_LR)
mse_ar_LR = mean((y2_test_LR - pred_ar_LR)**2)

seq_lambda = seq(0.2, 0, by = -0.001)
mse_train_LR = vector(mode = 'numeric', length = 200)
mse_test_LR = vector(mode = 'numeric', length = 200)
lasso_LR = glmnet(x2_LR, outcome_LR, type.measure = "mse", alpha = 1, lambda = seq_lambda)
for (lambda in seq_lambda) {
  pred_train = predict(lasso_LR, s = lambda, newx = x2_LR)
  pred_test = predict(lasso_LR, s = lambda, newx = x2_test_LR)
  mse_train_LR[which(seq_lambda == lambda)] = mean((pred_train - outcome_LR)**2)
  mse_test_LR[which(seq_lambda == lambda)] = mean((pred_test - y2_test_LR)**2)
}

df = data.frame('lambda' = seq_lambda, mse_train_LR, mse_test_LR) %>%
  pivot_longer(2:3, names_to = "series", values_to = "mse")
ggplot(df) +
  geom_line(aes(x = lambda, y = mse, color = series)) +
  geom_hline(aes(yintercept = mse_ar_LR, linetype = 'mse_ar')) +
  scale_linetype_manual(name = "baseline", values = "dashed") +
  theme_minimal()
ggsave("fig14.pdf", device = "pdf", width = 6, height = 2.75, dpi = "retina")

df_pred_LR = data.frame(predict(lasso_LR, s = seq_lambda[which.min(mse_test_LR)], newx = x2_test_LR), pred_ar_LR, y2_test_LR) %>%
  mutate(time = seq(2016.75, 2018.5, by = 0.25))
colnames(df_pred_LR) = c('lasso', 'AR_1', 'trueLR', 'time')
df_pred_LR = df_pred_LR %>%
  pivot_longer(1:3, names_to = 'series', values_to = 'value')
ggplot(df_pred_LR) +
  geom_line(aes(x = time, y = value, color = series)) +
  theme_minimal()
ggsave("fig15.pdf", device = "pdf", width = 6, height = 2.75, dpi = "retina")
pct_improvement_LR = (mse_ar_LR - min(mse_test_LR)) / mse_ar_LR

ggplot(aikman) +
  geom_line(aes(x = seq(1997.25, 2018.5, by = 0.25), y = log(outcome_LiborRepo)), color = "blue") +
  labs(x = 'time', y = 'log(LR)') +
  geom_vline(xintercept = seq(1997.25, 2018.5, by = 0.25)[79], linetype = 'dashed') +
  theme_minimal()
ggsave("fig16.pdf", device = 'pdf', scale = 2, dpi = 'retina')

# results
df_coef_LR = enframe(data.matrix(coef(lasso_LR))[,which.min(mse_test_LR)])
coef_ar_LR = c(coef(model_ar_LR)[1], rep(NA, 17), coef(model_ar_LR)[2])
df_coef = cbind(df_coef_LR, coef_ar_LR) %>%
  separate(name, sep = "_", into = c("Class", "Indicator"), fill = 'left') %>%
  rename('log(LR)' = value, 'Baseline (LR)' = coef_ar_LR) %>%
  mutate(order = c(1, 3:19, 2)) %>%
  arrange(order) %>%
  select(-order) %>%
  mutate(Indicator = replace(Indicator, Indicator == ".lagTED", ".lag")) %>%
  na_if(0)

# results table
names = c("Observations", "R2", "Test MSE", "Improvement (%)")
results_LR = c(76L, lasso_LR$dev.ratio[which.min(mse_test_LR)], min(mse_test_LR), pct_improvement_LR *100)
results_ar_LR = c(76L, summary(model_ar_LR)$r.squared, mse_ar_LR, NA)

df_results_LR = cbind(names, rep(NA,4), results_LR, results_ar_LR)
colnames(df_results_LR) = colnames(df_coef)

### table 1 - final model results
df_final = rbind(df_coef, df_results_LR) %>%
  mutate_at(vars(3:4), ~as.numeric(.)) %>%
  mutate_if(is.numeric, ~round(.,4))
stargazer(df_final, summary = FALSE, rownames = FALSE, font.size = 'small')

### hypothesis testing
coefs_LR = data.matrix(coef(lasso_LR))[,which.min(mse_test_LR)]
coefs_LR = names(coefs_LR[coefs_LR != 0])[-1]
formula_LR = paste("outcome_LR ~", paste(coefs_LR, collapse = "+"))
model_ht_LR = lm(as.formula(formula_LR), data = data.frame(x2_LR))
se_LR = sqrt(diag(NeweyWest(model_ht_LR)))

x2_test_LR = data.frame(x2_test_LR) %>%
  rename('.lagLR' = 'LR.78.85.')
pred_ht_LR = predict(model_ht_LR, newdata = data.frame(x2_test_LR))
mse_ht_test_LR = mean((y2_test_LR - pred_ht_LR)**2)
stargazer(model_ht_LR, se = list(se_LR), font.size = "scriptsize")

### neural networks
fit_nn_UK = function(n1, n2, layers, dr1, dr2, act1, act2) {
  
  LR = log(aikman$outcome_LiborRepo)
  .lagLR = LR[2:77]
  x2_LR = data.matrix(cbind(aikman_stationary[1:76,], .lagLR))
  outcome_LR = LR[3:78]
  x2_test_LR = data.matrix(cbind(aikman_stationary[77:84,], LR[78:85]))
  y2_test_LR = LR[79:86]
  
  
  model = keras_model_sequential()
  if (layers == 1) {
    model %>%
      layer_dense(units = n1, activation = act1, input_shape = c(18)) %>%
      layer_dropout(dr1) %>%
      layer_dense(units = 1)
  } else if (layers == 2) {
    model %>%
      layer_dense(units = n1, activation = act1, input_shape = c(18)) %>%
      layer_dropout(dr1) %>%
      layer_dense(units = n2, activation = act2) %>%
      layer_dropout(dr2) %>%
      layer_dense(units = 1)
  } else stop('choose 1 or 2 layers!')
  
  model %>% compile(loss = "mse", optimizer = optimizer_sgd(lr = 0.025), 
                    metrics = list("mean_squared_error"))
  
  early_stop = callback_early_stopping(monitor = "val_loss", patience = 15)
  history = model %>% fit(x2_LR, outcome_LR, epochs = 100, verbose = 0, 
                          validation_data = list(x2_test_LR, y2_test_LR),
                          metrics = list("mean_squared_error"),
                          callbacks = early_stop)
  mse = tail(history$metrics$val_mean_squared_error, n = 1)
  return(mse)
}

nn_args = read_excel('nn_args.xlsx')
set.seed(6440) # 6440
nn_args = nn_args %>%
  mutate(mse_LR = mapply(fit_nn_UK, n1, n2, layers, dr1, dr2, act1, act2),
         pct_LR = (mse_ar_LR - mse_LR) / mse_ar_LR
  )

nn_table = nn_args %>%
  select(-layers, -lr) %>%
  mutate(pct_LR = ifelse(pct_LR < 0, NA, pct_LR)) %>%
  na_if(0) %>%
  na_if('na') %>%
  mutate_at(vars(7:8), ~round(.,3)) %>%
  mutate_at(vars(8), ~.*100) %>%
  mutate_all(as.character)

colnames(nn_table) = c('Units (1)', 'Units (2)', 'Dropout (1)', 'Dropout (2)',
                       'Activation (1)', 'Activation (2)', 'Test MSE log(LR)',
                       'Pct Improvement log(LR)')

stargazer(nn_table, summary = FALSE, rownames = FALSE, font.size = 'scriptsize')


model_LR= keras_model_sequential()
model_LR %>%
  layer_dense(units = 8, activation = 'relu', input_shape = c(18)) %>%
  layer_dense(units = 1)

model_LR %>% compile(loss = "mse", optimizer = optimizer_sgd(lr = 0.025), 
                     metrics = list("mean_squared_error"))

early_stop = callback_early_stopping(monitor = "val_loss", patience = 15)
history = model_LR %>% fit(x2_LR, outcome_LR, epochs = 100, verbose = 0, 
                        validation_data = list(x2_test_LR, y2_test_LR),
                        metrics = list("mean_squared_error"),
                        callbacks = early_stop)
mse = tail(history$metrics$val_mean_squared_error, n = 1)

pred_nn_LR = predict(model_LR, x2_test_LR)
df_pred_nn_LR = data.frame(seq(2016.75, 2018.5, by = 0.25), pred_nn_LR, y2_test_LR, pred_ar_LR)
colnames(df_pred_nn_LR) = c('time', 'nn', 'trueLR', 'baseline')
df_pred_nn_LR = df_pred_nn_LR %>%
  pivot_longer(2:4, names_to = 'series', values_to = 'value')
ggplot(df_pred_nn_LR) +
  geom_line(aes(x = time, y = value, color = series)) +
  theme_minimal()
ggsave('fig16.pdf', width = 6, height = 2.75, dpi = 'retina')


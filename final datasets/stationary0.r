library(tidyverse)
library(tseries)

kiley = read_excel("kiley_labeled.xlsx") %>%
  na_if('next fsr') %>%
  select_at(3:29) %>%
  mutate_all(as.numeric) %>%
  replace_na(as.list(colMeans(kiley0, na.rm = TRUE)))

test_stationarity = function(series) {
  ADF = adf.test(series)
  return(ADF$p.value)
}

trends = sapply(kiley, test_stationarity)

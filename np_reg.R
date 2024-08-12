##nonparametric regression
library(tidyverse)
library(parallel)
library(np)

source("functions.R")
load("data.RData")

diff_data <- apply(data[,-(1)], 2, FUN= function(x) diff(x,1))
diff_data <- tibble(data.frame(data$Date[-1], diff_data))
colnames(diff_data) <- colnames(data)

data <- data %>% rename(swe_CPIF = swe_KPIF)
swe_formula <- swe_CPIF ~ swe_interest + swe_unemployment
us_formula <- us_PCE ~ us_interest + us_unemployment
swe_formula_unconditional <- swe_CPIF ~ swe_interest

set.seed(1337)
swe_diff_np <- np_parallel(formula=swe_formula, lag_var1 = "swe_interest", lag_var2 = "swe_unemployment",
                           lags1= c(0:3), lags2=c(0:3), regtype="ll", data=diff_data)

swe_diff_np_lc <- np_parallel(formula=swe_formula, lag_var1 = "swe_interest", lag_var2 = "swe_unemployment",
                              lags1= c(0:3), lags2=c(0:3,regtype="lc"), data=diff_data)

us_diff_np <- np_parallel(formula=us_formula, lag_var1 = "us_interest", lag_var2 = "us_unemployment",
                          lags1= c(0:3), lags2=c(0:3), regtype="ll", data=diff_data)
                           
swe_diff_np_lc <- np_parallel(formula=swe_formula, lag_var1 = "swe_interest", lag_var2 = "swe_unemployment",
                              lags1= c(1:3), lags2=c(1:3,regtype="lc"), data=diff_data)




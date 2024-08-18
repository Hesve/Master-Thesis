##nonparametric regression
library(tidyverse)
library(parallel)
library(np)

source("functions.R")
load("data.RData")

swe_formula <- swe_KPIF ~ swe_interest + swe_unemployment + lag(swe_KPIF,1)
us_formula <- us_PCE ~ us_interest + us_unemployment + lag(us_PCE,1)

set.seed(1337)
#this corresponds to using the npreg function for each lag combination for interest rates and unemployment
swe_results<- np_parallel(formula=swe_formula, lag_var1 = "swe_interest", lag_var2 = "swe_unemployment",
                           lags1= c(0:6), lags2=c(0:6), regtype="ll", data=data)


us_results<- np_parallel(formula=us_formula, lag_var1 = "us_interest", lag_var2 = "us_unemployment",
                          lags1= c(0:6), lags2=c(0:6), regtype="ll", data=data)
par(mfrow=c(3,3))
#
#this correspondS  plotting the model parts of each sublists in the results.
#for example plot(swe_diff_np$`swe_unemployment lag = 0`$`swe_interest lag = 0`$model)
lapply(swe_diff_np, np.plot)
plot(swe_diff_np$`swe_unemployment lag = 1`$`swe_interest lag = 1`$model$xnames)
#keeping only the results for the interest rate and unemployment rate with the same lags
swe_lag_1 <- swe_results$`swe_unemployment lag = 1`$`swe_interest lag = 1`
swe_lag_2 <- swe_results$`swe_unemployment lag = 2`$`swe_interest lag = 2`
swe_lag_3 <- swe_results$`swe_unemployment lag = 3`$`swe_interest lag = 3`
swe_lag_4 <- swe_results$`swe_unemployment lag = 4`$`swe_interest lag = 4`
swe_lag_5 <- swe_results$`swe_unemployment lag = 5`$`swe_interest lag = 5`
swe_lag_6 <- swe_results$`swe_unemployment lag = 6`$`swe_interest lag = 6`

swe_lag_1$model$xnames <- c("swe_KPIF lag = 1", "swe_unemployment lag = 2","swe_interest lag = 1")
swe_lag_2$model$xnames <- c("swe_KPIF lag = 2", "swe_unemployment lag = 2","swe_interest lag = 2")
plot(swe_lag_1$model)
plot(swe_lag_2$model) 
plot(swe_lag_3$model)
plot(swe_lag_4$model)
plot(swe_lag_5$model)
plot(swe_lag_6$model)

us_lag_1 <- us_results$`us_unemployment lag = 1`$`us_interest lag = 1`
us_lag_2 <- us_results$`us_unemployment lag = 2`$`us_interest lag = 2`
us_lag_3 <- us_results$`us_unemployment lag = 3`$`us_interest lag = 3`
us_lag_4 <- us_results$`us_unemployment lag = 4`$`us_interest lag = 4`
us_lag_5 <- us_results$`us_unemployment lag = 5`$`us_interest lag = 5`
us_lag_6 <- us_results$`us_unemployment lag = 6`$`us_interest lag = 6`
plot(us_lag_1$model)
plot(us_lag_2$model)
plot(us_lag_3$model)
plot(us_lag_4$model)
plot(us_lag_5$model)
plot(us_lag_6$model, ylim=c(c(0,4),c(0,10),c(0,3)))
set.seed(1337)

par(mfrow=c(3,3))
swe_diff_np_lc <- np_parallel(formula=swe_formula, lag_var1 = "swe_interest", lag_var2 = "swe_unemployment",
                              lags1= c(1:6), lags2=c(1:6,regtype="lc"), data=data)

us_diff_np_lc <- np_parallel(formula=us_formula, lag_var1 = "us_interest", lag_var2 = "us_unemployment",
                             lags1= c(1:6),ags2=c(1:6),regtype="lc", data=data)


#combined results for each lag combination for interest rates and unemployment when its the same
swe_combined <- list(swe_lag_1, swe_lag_2, swe_lag_3, swe_lag_4, swe_lag_5, swe_lag_6)
us_combined <- list(us_lag_1, us_lag_2, us_lag_3, us_lag_4, us_lag_5, us_lag_6)

#for both lists, take the bandiwdhts resulst from each sublist$model$bw
swe_bw <- do.call(rbind, lapply(swe_combined, function(x) x$model$bw))
colnames(swe_bw) <- c("lagged_swe_KPIF)", "swe_unemployment", "swe_interest")
rownames(swe_bw) <- paste("lag = ", 1:6)
swe_bw %>% xtable(caption="Bandwidths for Swedish data", label="tab:swe_bw") %>% 
  print(caption.plave="top", table.placement="H")

us_bw <- do.call(rbind, lapply(us_combined, function(x) x$model$bw))
colnames(us_bw) <- c("lagged_us_PCE", "us_unemployment", "us_interest")
rownames(us_bw) <- paste("lag = ", 1:6)
us_bw %>% xtable(caption="Bandwidths for US data", label="tab:us_bw") %>% 
  print(caption.plave="top", table.placement="H")

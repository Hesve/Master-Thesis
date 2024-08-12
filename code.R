#read data


#Reading data


library(tidyverse)
library(forecast)
library(stats)
library(psych)
library(xtable)
#library(parallel)
library(tseries)
library(ctsCausal)
library(parallel)
source("functions.R")
load("data.RData")

data <- data %>% rename(swe_CPIF = swe_KPIF)

psych::describe(data)[-c(1:3),-c(1,6,7,10,13)] %>% #exclude date as first row as well as some redundant columns 
  xtable(caption="Summary statistics for the different variables", 
         label="summary_table", digits=c(0,0, rep(2, ncol(.)-1))) %>%  
  print(caption.placement="top", table.placement="H")

par(mfrow=c(3,2))
ts_plot(data, vars=colnames(data)[-c(1:3)], start=c(1994,6)) 


#ljung box tests

##note not sure which lag length to choose, i guess 12 makes sense since its monthly data. 
#But if trying for less then both CPIF and PCE are stationary according to results



#adf and pp test
do.call(rbind, apply(data[,-1], 2, adf.test, k=12))[-c(1,2),c(1,4)] %>%
  xtable(caption="ADF tests)", 
         label="adf_table_non_diff") %>%
  print(caption.placement="top", table.placement="H")
do.call(rbind, apply(data[,-1], 2, pp.test))

do.call(rbind, apply(data[,-1], 2, Box.test, lag = 12, type = "Ljung-Box")) %>% 
  .[,1:3] %>% 
  xtable(caption="Ljung-Box test for white noise", 
         label="ljung_box_table", digits=c(0,0,0,2)) %>% 
  print(caption.placement="top", table.placement="H")

#package for testing the null of flat density
 #devtools::install_github("tedwestling/ctsCausal")
# also requries additional packages
# install.packages("sets")
 #install.packages("earth")
 #install.packages("SuperLearner")

library(ctsCausal)
library(SuperLearner)
library(earth)
library(sets)





#causal null tests



#####

diff_data <- apply(data[,-(1)], 2, FUN= function(x) diff(x,1))
diff_data <- tibble(data.frame(data$Date[-1], diff_data))

colnames(diff_data) <- colnames(data)
#adf_tests for diff_data 
do.call(rbind, apply(diff_data[,-1], 2, adf.test, k=12))[-c(1,2),c(1,4)] %>%
  xtable(caption="ADF tests for the differenced data", 
         label="adf_table") %>%
  print(caption.placement="top", table.placement="H")

#ljung box tests
do.call(rbind, apply(diff_data[,-1], 2, Box.test, lag = 12, type = "Ljung-Box")) %>% 
  .[,1:3] %>% 
  xtable(caption="Ljung-Box test for white noise", 
         label="ljung_box_table", digits=c(0,0,0,2)) %>% 
  print(caption.placement="top", table.placement="H")

#causal null tests
set.seed(1337)
#this is equivalent to performing the function ctsCausal::causalNullTest() 4 times with different lags of the interest rate
swe_tests_diff <- causal_null_parallel(Y = diff_data$swe_CPIF, A=diff_data$swe_interest, W=diff_data$swe_unemployment, 
                                     lags = c(0:3), control = list(cross.fit = TRUE, verbose=TRUE), p=Inf)
swe_tests_diff

us_tests_diff <- causal_null_parallel(Y = diff_data$us_PCE, A=diff_data$us_interest, W=diff_data$us_unemployment, 
                                    lags = c(0:3), control = list(cross.fit = TRUE, verbose=TRUE), p=Inf)

us_tests_diff

do.call(rbind, lapply(swe_tests_diff, function(x) x$test)) %>% 
  round(2) %>% 
  xtable(caption="Causal null tests for the swedish differenced data", 
         label="causal_null_table") %>%
  print(caption.placement="top", table.placement="H")

do.call(rbind, lapply(us_tests_diff, function(x) x$test)) %>% 
  round(2) %>% 
  xtable(caption="Causal null tests for the US differenced data", 
         label="causal_null_table") %>%
  print(caption.placement="top", table.placement="H")





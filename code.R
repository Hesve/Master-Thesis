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

psych::describe(data)[-c(1:3),-c(1,6,7,10,13)] %>% #exclude date as first row as well as some redundant columns 
  xtable(caption="Summary statistics for the different variables", 
         label="summary_table", digits=c(0,0, rep(2, ncol(.)-1))) %>%  
  print(caption.placement="top", table.placement="H")

par(mfrow=c(3,2))
ts_plot(data, vars=colnames(data)[-c(1:3)], start=c(1994,6)) 


select(data, "us_interest") %>% 
  ts(start=c(1994,6), frequency=12) %>% 
  plot(xlab="Year",cex=10)

#ljung box tests
do.call(rbind, apply(data[,-c(1:3)], 2, Box.test, lag = 12, type = "Ljung-Box")) %>% 
  .[,1:3] %>% 
  xtable(caption="Ljung-Box test for white noise", 
         label="ljung_box_table", digits=c(0,0,0,2)) %>% 
  print(caption.placement="top", table.placement="H")

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



causal_null_double_lag(Y=data$swe_KPIF, A=data$swe_interest, W=data.frame(data$swe_unemployment), lag = 0,
                       control = list(cross.fit = TRUE, verbose=TRUE, g.n.bins=2:5), p=Inf,autoregress_Y_lags = 2,
                       var_name = "swe_KPIF")

results_4_autoregress <- sapply(0:6,
  function(lag){
    causal_null_double_lag(Y=data$swe_KPIF, A=data$swe_interest, W=data.frame(data$swe_unemployment), lag = lag,
                           control = list(cross.fit = TRUE, verbose=TRUE, g.n.bins=2:5), p=Inf,autoregress_Y_lags = 4,
                           var_name = "swe_KPIF")
  }, simplify=FALSE) #significant 0 and 6
len#causal null tests
results_3_autoregress <- sapply(0:6,
                                function(lag){
                                  causal_null_double_lag(Y=data$swe_KPIF, A=data$swe_interest, W=data.frame(data$swe_unemployment), lag = lag,
                                                         control = list(cross.fit = TRUE, verbose=TRUE, g.n.bins=2:5), p=Inf,autoregress_Y_lags = 3,
                                                         var_name = "swe_KPIF")
                                }, simplify=FALSE) #significant 0 and 4

results_2_autoregress <- sapply(0:6,
                                function(lag){
                                  causal_null_double_lag(Y=data$swe_KPIF, A=data$swe_interest, W=data.frame(data$swe_unemployment), lag = lag,
                                                         control = list(cross.fit = TRUE, verbose=TRUE, g.n.bins=2:5), p=Inf,autoregress_Y_lags = 2,
                                                         var_name = "swe_KPIF")
                                }, simplify=FALSE) #significant 0 and 6


set.seed(1337)
results_1_autoregress <- sapply(0:6,
                                function(lag){
                                  causal_null_double_lag(Y=data$swe_KPIF, A=data$swe_interest, W=data.frame(data$swe_unemployment), lag = lag,
                                                         control = list(cross.fit = TRUE, verbose=TRUE, g.n.bins=2:5), p=Inf,autoregress_Y_lags = 1,
                                                         var_name = "swe_KPIF")
                                }, simplify=FALSE) #significant  0 2,3,4

results_1_autoregress
us_results
set.seed(1338)

us_results <- sapply(0:6,
                     function(lag){
                       causal_null_double_lag(Y=data$us_PCE, A=data$us_interest, W=data.frame(data$us_unemployment), lag = lag,
                                              control = list(cross.fit = TRUE, verbose=TRUE, g.n.bins=2:5), p=Inf,autoregress_Y_lags = 1,
                                              var_name = "us_PCE")
                     }, simplify=FALSE)
names(results_1_autoregress) <- paste("lag", 0:6)
names(us_results) <- paste("lag", 0:6)

swe_res_table <- do.call(rbind, lapply(results_1_autoregress, function(x) x$test))
us_res_table <-  do.call(rbind, lapply(us_results, function(x) x$test))



#printing swedish results
  print(
    xtable(swe_res_table[-1,c(1,3)], caption="Causal null tests for Swedish data", 
               label="swe_res_table"), 
        caption.placement="top", table.placement="H")
  
  #printing us results
  print(
    xtable(us_res_table[-1,c(1,3)], caption="Causal null tests for US data", 
               label="us_res_table"), 
        caption.placement="top", table.placement="H")
#####2 3 5

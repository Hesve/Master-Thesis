#read data


#Reading data


library(tidyverse)
library(forecast)
library(stats)
library(psych)
library(xtable)
source("functions.R")
load("data.RData")

psych::describe(data)[-1,-c(1,6,7,10,13)] %>% #exclude date as first row as well as some redundant columns 
  xtable(caption="Summary statistics for the different variables", 
         label="summary_table", digits=c(0,0, rep(2, ncol(.)-1))) %>%  
  print(caption.placement="top", table.placement="H")

  




ccf(data$swe_CPI, data$swe_interest)



lag_cor(data$swe_CPI, data$swe_interest, 1)


par(mfrow=c(3,2))
ts_plot(data, vars=colnames(data)[-1], start=c(1994,6)) #excluding date variable


for(var in colnames(data)[-1]){
  select(data, var) %>% 
    tsdisplay(lag.max = 24, main = var)
}



#ljung box tests

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
set.seed(1337)



#this is equivalent to performing the ctsCausal::causalNullTest() function 8 times
#where we each time try another lag for the interest rate

#causalNullTest() function performs a hypothesis test that the causal dose-response curve theta(a)
#"is flat on the support of the observed exposure A.
#Y is the outcome, A is the exposure, and W is a matrix of covariates.


swe_tests <- causall_null_multi(Y = data$swe_CPI, A=data$swe_interest, W=data$swe_unemployment, 
                                lags = c(0:6, 12), control = list(cross.fit = FALSE, verbose=TRUE, g.n.bins = 2:5))

us_tests <- causall_null_multi(Y = data$us_CPI, A=data$us_interest, W=data$us_unemployment, 
                               lags = c(0:6, 12), control = list(cross.fit = FALSE, verbose=TRUE, g.n.bins = 2:5))


library(np)

library(dplyr)

swe_formula <- swe_CPI ~ swe_interest + swe_unemployment
set.seed(1337)

#this is equivalent to performing the np::npreg() function 8 times where we each time try annother lag for the interest rate
swe_results <- np_multi(formula=swe_formula, lag_var = "swe_interest", lags = c(0:6, 12) ,
                        replace_lag_0 = TRUE, single_lag=TRUE, data=data)


us_formula <- us_CPI ~ us_interest + us_unemployment
us_results <- np_multi(formula=us_formula, lag_var = "us_interest", lags = c(0:6, 12) ,
                        replace_lag_0 = TRUE, single_lag=TRUE, data=data, regtype="ll")

np.summary(swe_results)
np.summary(us_results)

par(mfrow=c(2,2))
np.plot(us_results, view="fixed", theta=seq(0,35,5), phi=seq(0,35,5))
np.plot(swe_results, view="fixed", theta=seq(0,35,5), phi=seq(0,35,5))










####these are just some other results i tried with different estimator and quarterly data instead ####

#testing with local constant estimator
us_results_test <- np_multi(formula=test_formula, lag_var = "us_interest", lags = c(0:6,12) ,
                       replace_lag_0 = TRUE, single_lag=TRUE, data=data, regtype="ll")
swe_results_test <- np_multi(formula=swe_formula, lag_var = "swe_interest", lags = c(0:6,12) ,
                       replace_lag_0 = TRUE, single_lag=TRUE, data=data, regtype="lc")

np.plot(us_results_test, view="fixed", theta=seq(0,35,5), phi=seq(0,35,5))
np.plot(swe_results_test, view="fixed", theta=seq(0,35,5), phi=seq(0,35,5))


#performing the same tests but with the quartely data
load("data_quarterly.RData")
swe_results_quarterly <- np_multi(formula=swe_formula, lag_var = "swe_interest", lags = c(0:3) ,
                                  replace_lag_0 = TRUE, single_lag=TRUE, data=data_quarterly, regtype="ll")
np.plot(swe_results_quarterly, view="fixed", theta=seq(0,35,5), phi=seq(0,35,5))
np.summary(swe_results_quarterly)
swe_test_quarterly <- causall_null_multi(Y = data_quarterly$swe_CPI, A=data_quarterly$swe_interest, W=data_quarterly$swe_unemployment, 
                                         lags = c(0:6, 12), control = list(cross.fit = FALSE, verbose=TRUE, g.n.bins = 2:5))


us_results_quarterly <- np_multi(formula=us_formula, lag_var = "us_interest", lags = c(0:3) ,
                                  replace_lag_0 = TRUE, single_lag=TRUE, data=data_quarterly, regtype="ll")
np.plot(us_results_quarterly, view="fixed", theta=seq(0,35,5), phi=seq(0,35,5))




######## manual coding mostly for testing, can be ignored #######
#swedish data

#nonparametric regression
model_np_swe <- npreg(swe_CPI ~ swe_interest + swe_unemployment, 
                      gradients = TRUE, regtype="ll",  data=data)
summary(model_np_swe)  

model_np_swe2 <- npreg(swe_CPI ~ swe_interest + swe_unemployment + lag(swe_CPI,5), 
                      gradients = TRUE, regtype="ll",  data=data)
plot(model_np_swe2)
#racine et all 2006 significance tests

npsigtest(model_np_swe)
plot(model_np_swe, view="fixed") 

bw.all <- npregbw(swe_CPI ~ swe_interest + swe_unemployment, 
                  gradients = TRUE, regtype="ll",  data=data)

summary(npreg(bws = bw.all))

plot(model_np_swe, plot.errors.method = "bootstrap",
     plot.errors.boot.num = 25)

#testing with quartely data
model_np_swe_quarterly <- npreg(swe_CPI ~ swe_interest + swe_unemployment, 
                                gradients = TRUE, regtype="ll",  data=data_quarterly)


#unconditional PDF AND CDF

f.swe <- npudens( ~ swe_CPI + swe_interest, data=data)
F.swe <- npudist(~ swe_CPI + swe_interest, data=data)

plot(f.swe, xtrim =-0.2, view = "fixed", main = "")
summary(f.swe)
summary(F.swe)
plot(F.swe)


#conditional densities
f.swe.cond <- npcdist(swe_CPI ~ swe_interest + swe_unemployment, 
                gradients = TRUE, regtype="ll",  data=data)
f.swe.cond3 <- npcdist(swe_CPI ~ swe_interest + swe_unemployment,  data=data)
plot(f.swe.cond)
plot(f.swe.cond3, view="fixed")
f.swe.cond2 <- npcdist(swe_CPI ~ swe_interest, 
                 gradients = TRUE, regtype="ll",  data=data)
plot(f.swe.cond2, view="fixed")
plot(Fhat)

bw_cond <- npcdistbw(swe_CPI ~ swe_interest + swe_unemployment, 
                     gradients = TRUE, regtype="ll",  data=data)
 model.q0.25 <- npqreg(bws = bw_cond, tau = 0.25)
 model.q0.50 <- npqreg(bws = bw_cond, tau = 0.50)
 model.q0.75 <- npqreg(bws = bw_cond, tau = 0.75)
 
 plot(bw_cond)
 
 
 
 ####US data
 #nonparametric regression
 model_np_us <- npreg(us_CPI ~ us_interest + us_unemployment, 
                      gradients = TRUE, regtype="ll",  bwmethod = "cv.aic", data=data)
 summary(model_np_us)  
 
 model_np_us2 <- npreg(us_CPI ~ lag(us_interest,2) + us_unemployment, 
                       gradients = TRUE, regtype="ll",  data=data)
 plot(model_np_us, view="fixed")
 plot(model_np_us2, view="fixed")
 #racine et all 2006 significance tests
 
 
 
 npsigtest(model_np_us)
 plot(model_np_us, view="fixed") 
 
 bw.all <- npregbw(us_CPI ~ us_interest + us_unemployment, 
                   gradients = TRUE, regtype="ll",  data=data)
 
 summary(npreg(bws = bw.all))
 
 plot(model_np_us, plot.errors.method = "bootstrap",
      plot.errors.boot.num = 25)
 
 
 #unconditional PDF AND CDF
 
 f.us <- npudens( ~ us_CPI + us_interest, data=data)
 F.us <- npudist(~ us_CPI + us_interest, data=data)
 
 plot(f.us, xtrim =-0.2, view = "fixed", main = "")
 summary(f.us)
 summary(F.us)
 plot(.us)
 test <- np.sign_test_table(swe_results)
 
 
 #conditional densities
 f.us.cond <- npcdist(us_CPI ~ us_interest + us_unemployment, 
                      gradients = TRUE, regtype="ll",  data=data)
 f.us.cond3 <- npcdist(us_CPI ~ us_interest + us_unemployment,  data=data)
 plot(f.us.cond)
 plot(f.us.cond3, view="fixed")
 f.us.cond2 <- npcdist(us_CPI ~ us_interest, 
                       gradients = TRUE, regtype="ll",  data=data)
 plot(f.us.cond2, view="fixed")
 plot(Fhat)
 
 bw_cond <- npcdistbw(us_CPI ~ us_interest + us_unemployment, 
                      gradients = TRUE, regtype="ll",  data=data)
 model.q0.25 <- npqreg(bws = bw_cond, tau = 0.25)
 model.q0.50 <- npqreg(bws = bw_cond, tau = 0.50)
 model.q0.75 <- npqreg(bws = bw_cond, tau = 0.75)
 
 plot(bw_cond) 
 

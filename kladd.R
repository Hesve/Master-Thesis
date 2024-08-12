np_double_lag <- function(formula, data, lag_var1, lag_var2, lags1, lags2, replace_lag_0=TRUE, regtype="ll"){
  results <- list()
  for (lag1 in lags1){
    for(lag2 in lags2){
      cat("current lags are ", lag1, " and ", lag2, fill=TRUE)
      new_formula <- add_lag_to_formula(formula, lag_var1, lag1, replace_lag_0)
      new_formula <- add_lag_to_formula(new_formula, lag_var2, lag2, replace_lag_0)
      paste_string <- paste(lag_var1, "=", lag1, lag_var2, "", lag2, sep=" ")
      results[[paste_string]] <-  np_multi(formula=test_formula, lag_var = "us_interest", lags = c(0:6,12) ,
                                           replace_lag_0 = TRUE, single_lag=TRUE, data=data, regtype="ll")
    }
  }
  return(results)
}
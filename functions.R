####some general functions that will be used in the analysis

lag_cor <- function(x, y, lag){
  stopifnot( "Invalid lag" = (is.numeric(lag) && lag >=0))
  stopifnot( "X and Y length differ" = (length(x) == length(y)))
  if (lag == 0) {
    correlation <- cor(x, y)
  } else {
    correlation <- cor(x[(1 + lag):length(x)], y[1:(length(y) - lag)])
  }
  return(correlation)
}

cc_table <- function(x,y, lags){
  res <- sapply(lags, FUN=function(lag){
    cor <- lag_cor(x,y, lag=lag)
    names(cor) <- paste("Lag", lag)
    return(cor)
  })
  return(res)
}

#Wrapper function to make time series plots
ts_plot <- function(tibble, vars, frequency=12, start){
  for (var in vars){
    select(tibble, var) %>% 
      ts(start=start, frequency=frequency) %>% 
      plot(xlab="Year",cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5, cex=5)
  }
}

####
#the following functions are meant to be used with the npreg functions from npreg package.
#the reason is that the npreg functions requires an formula as a specific argument
#hence these functions will help make the formulas more automatized for various scenarios
#so that given a base formula for which variables to include, we can easily add
#lagged versions of one variable without specifying the whole formula again
#Hence, these are more helpful for comparing different models, rather than use for a single model



#credit to stackoverflow 
#this functions is meant to work for the add-lag_to_formula function belowp
#given a lagged variable, it drops the unlagged variable and keeps only the lagged version.
#ex: formula <-  swe_CPI ~ swe_interest + swe_unemployment + dplyr::lag(swe_interest, 1)
#drop_formula_term(formula, "swe_interest)
#returns: swe_unemployment + dplyr::lag(swe_interest, 1)

drop_formula_term <- function(the_formula, var_name) {
  var_position <- grep(
    var_name, attr(terms(the_formula), "term.labels"))[1]
  update(
    the_formula,
    drop.terms(terms(the_formula), var_position,
               keep.response=TRUE))
}



#this function a formula as argument and adds lagged version of the lag_var to it
#replace_lag_0 means that the non_lagged variable is dropped from the formula while only keeping the lagged verions
#example 
#formula <- swe_CPI ~ swe_interest + swe_unemployment
# add_lag_to_formula(formula, "swe_interest", 1:2) 
#returns
# swe_CPI ~ swe_interest + swe_unemployment + dplyr::lag(swe_interest, 1) 

add_lag_to_formula <- function(formula, lag_var, lag, replace_lag_0=FALSE) {
  # Ensure lag_var is treated as a symbol
  lag_var_sym <- sym(lag_var)
  
  # Parse the original formula
  formula_rhs <- formula[[3]]
  formula_lhs <- formula[[2]]
  
  #saves all lag expressions in a list
  lag_expressions <- lapply(lag, function(l) {
    l <- as.numeric(l)  # Convert to numeric to avoid the L suffix
    bquote(lag(.(lag_var_sym), .(l)))
  })
  
  #creating a proper language expression to use in formula by combining the  
  #lag_expressions with the old formula right hand side
  new_rhs<- Reduce(function(x, y) call("+", x, y), c(list(formula_rhs),
                                                      lag_expressions))
  new_formula <- as.formula(call("~", formula_lhs, new_rhs))
  if(replace_lag_0 == TRUE ){
    new_formula <- drop_formula_term(new_formula, lag_var)
  }
  
  return(new_formula)
}


#wrapper function for npreg and npsigtest functions to perform them given formula
#and save results in a list
np_wrapper <- function(formula, data,regtype="ll"){
  np_model <- npreg(formula, gradients=TRUE, regtype=regtype, data=data)
  sign_test <- npsigtest(np_model)
  list_results <- list("model"=np_model, "sign_test" = sign_test)
  return(list_results)
}

#generalized wrapper for npreg and npsigtest functions that allows for multiple lags
#if single_lag =FALSE it performs the functions using all specified lags in a single model,
#if single_lag=TRUE it performs the functions iteratively over each lag one at a time for multiple models
# if replace_lag_0= TRUE it will drop the non-lagged variable from the formula

np_multi <- function(formula, data,lag_var, lags, replace_lag_0=TRUE, 
                     single_lag = TRUE, regtype="ll"){
  if(missing(lags)){ #this is the case where we do not lag any variable
    list_results <- np_wrapper(formula=formula, data=data, regtype=regtype,...)
  } else if(single_lag){ #this case for when we use each lag separately in new models
    list_results <- sapply(lags, FUN=function(lag){
      cat("current lag is ", lag, fill=TRUE)
      if(lag==0){
        new_formula <- formula
      } else{
        new_formula <-  add_lag_to_formula(formula,lag=lag, lag_var=lag_var, replace_lag_0)
      }
      results <- np_wrapper(formula = new_formula, data=data, regtype=regtype)
    }, simplify = FALSE)
    list_names <- sapply(lags, FUN=function(lag_length){paste0("lag=",lag_length)})
    list_results <- setNames(list_results, list_names)
  }else{ #this case for when all lags in the same model
  new_formula <- add_lag_to_formula(formula,lag_var, lags, replace_lag_0)
 list_results <- np_wrapper(new_formula, data=data, regtype=regtype)
  }
  return(list_results)
}

np.plot<- function(result_list, view="fixed", .., theta=NULL, phi=NULL, ...){
  n_models <- length(result_list)
  
  #giving default values of theta and phi unless specified
  if (is.null(theta)) {
    theta <- rep(30, n_models)
  }
  if(is.null(phi)){
    phi <- rep(10, n_models)
  }
    
  sapply(1:n_models, function(i){
    main_name <- paste(names(result_list)[i], "theta=", 
                       theta[i], "phi=", phi[i], sep=" ")
    plot(result_list[[i]]$model, 
    view=view, theta=theta[i], phi=phi[i], 
    main=main_name)
    })
}

#function to perform the np_multi over two lagged variables
#for each alg of the second variable, it performs the np_multi for each lag of the first variable
np_double_lag <- function(formula, data, lag_var1, lag_var2, lags1, lags2, replace_lag_0=TRUE, regtype="ll"){
  results_list <- sapply(lags2, FUN=function(lag2){
    cat("Current", lag_var2, "lag is", lag2, fill=TRUE)
    new_formula <- add_lag_to_formula(formula, lag_var=lag_var2, lag=lag2, replace_lag_0)
    res <- np_multi(new_formula, data, lag_var=lag_var1, lags=lags1, replace_lag_0, 
                    regtype, single_lag=TRUE)
    names(res) <- paste(lag_var1, "lag", "=", lags1)
  return(res)},
  simplify=FALSE)

  return(results_list)
}



np_parallel <- function(formula, data, lag_var1, lag_var2, lags1, lags2, 
                        replace_lag_0=TRUE, regtype="ll"){
  n_cores <- parallel::detectCores()- 1 #saving 1 core for OS 
  print("cores done")
  
  cl <- makeCluster(n_cores)
  clusterExport(cl, c("data", "formula"))
  print("export done")
  clusterEvalQ(cl, {
    library(tidyverse)
    library(np)
    source("functions.R") # Ensure this file is in the working directory
  })
  print("eval done")
  formula
  data
  results_list <- parSapply(cl, lags2, FUN=function(lag2){
    print("starting parsapply")
    print(formula)
    new_formula <- add_lag_to_formula(formula, lag_var=lag_var2, lag=lag2, replace_lag_0)
    res <- np_multi(new_formula, data, lag_var=lag_var1, lags=lags1, replace_lag_0, 
                    regtype, single_lag=TRUE)
    names(res) <- paste(lag_var1, "lag", "=", lags1)
    return(res)},
    simplify=FALSE)
  names(results_list) <- paste(lag_var2, "lag", "=", lags2)
  stopCluster(cl)
  return(results_list)
}

#function to retrieve the sign test results from a list of npreg models freom np.multi function and 
#then present the bandwidht value, p_value and model R" for all models in a list
#note, only works for when the np.multi function is used with single_lag=TRUE since it
#assumes you have a list of models with different lags

#####
summary_wrapper <- function(np_model){
  bandwidth <- np_model$model$bw
  p_value <- np_model$sign_test$P
  r_squared <- np_model$model$R2
  results <- rbind(bandwidth, p_value)
  colnames(results) <- np_model$model$xnames
  return(list("estimation"=round(results ,4), "r_squared"=r_squared))
}
#####

np.summary <- function(np_model_list){
  results_list <- lapply(np_model_list, function(cur_model){
    bandwidth <- cur_model$model$bw
    p_value <- cur_model$sign_test$P
    r_squared <- cur_model$model$R2
    cur_model_result <- rbind(bandwidth, p_value)
    colnames(cur_model_result) <- cur_model$model$xnames
    return(list("estimation"=round(cur_model_result,4), "r_squared"=r_squared))
  })
  return(results_list)
}



#function based on the causalNullTest() from ctscausal package.
#given arguments Y, A, W, control, it performs the causal null test
# for the argument A, take the lag of the variable depending on specified lag length as annother argument
#it also needs to adjust the length of the Y variable and W variable to match the lagged A variable
#its assumed that the number of observaitons for all variables with no lag is the same
#the function returns the results of the causal null test for the lagged variable
causal_wrapper <- function(Y, A, W, lag, control, p){
  stopifnot("Invalid lag" = (is.numeric(lag) && lag >=0))
  stopifnot("Y and A length differ" = (length(Y) == length(A)))
  stopifnot("Y and W length differ" = (length(Y) == length(W)))
  if (lag == 0) {
    results <- causalNullTest(Y = Y, A=A, W=data.frame(W), control=control, p=p)
  } else {
    results <- causalNullTest(Y = Y[1:(length(Y)-lag)], A=A[(1+lag):length(A)], 
                              W=data.frame(W[1:(length(W) - lag)]), 
                              control=control, p=p)
  }
  return(results)
}

causal_null_multi <- function(Y, A, W, lags, control = list(cross.fit = TRUE,
                                                             verbose=TRUE), p=Inf){
  res <- sapply(lags, FUN=function(lag){
    cat("current lag is", lag, fill = TRUE)
    causal_wrapper(Y, A, W, lag, control, p=p)
  })
  names(res) <- paste("lag", lags)
  return(res)
}

length_checks <- function(Y, A, W){
  stopifnot("Y and A length differ" = (length(Y) == length(A)))
  stopifnot("Y and W length differ" = (length(Y) == nrow(W)))
}

#function to lag a variable  for each given lag and return a new dataframe with autoregressed variables
#lags integer of how many lags to consider
#var_name is name of the variable to be lagged to be used in DF
auto_lag <- function(var, lags, var_name){
  new_data <- data.frame(var)
  seqs <- seq_along(1:lags)
  for (lag in seqs){
    new_data <- cbind(new_data, dplyr::lag(var, lag))
  }
  colnames(new_data) <- c(var_name, paste0(var_name, "_lag",seqs))
  return(new_data)
}
#function to perform the causal_null_multi function for two lagged variables in parallel similiar to the np_parallel function
#lag_var which variable of the covariates to lag
#lags1 is asusmed to be A
#lags 2 is the lags for lag_var

####does not work yet#########
#performs the causalnulltest while lagging both A and W #and autoregessing Y
causal_null_double_lag <- function(Y, A, W,  lag, control, p, autoregress_Y_lags, var_name){
  stopifnot(is.data.frame(W))
  stopifnot("Invalid lag" = (is.numeric(lag) && lag >=0))
  length_checks(Y, A, W)
  n_obs <- length(Y)
  new_Y <- Y[(1+lag):(n_obs)]
  new_A <- A[1:(n_obs-lag)]
  new_W <- data.frame(W[1:(n_obs - lag),])
  autoregress_df <- auto_lag(Y, autoregress_Y_lags, var_name=var_name)
  autoregress_df<- autoregress_df[(1+autoregress_Y_lags):n_obs,][-1,]#remove first column which is non-lagged Y
  nrow_dif <- abs(nrow(autoregress_df) - nrow(new_W))
  #checking which dataframe is longer and adjusting the length of the other to match'
  #by removing from from first rows to last
  #this is done to ensure that the causalNullTest function works
  #
  if (nrow(autoregress_df) > nrow(new_W)){
    autoregress_df <- autoregress_df[(1+nrow_dif):nrow(autoregress_df),]
  } else if (nrow(autoregress_df) < nrow(new_W)){
    new_W <- data.frame(new_W[(1+nrow_dif):nrow(new_W),])
    new_A <- new_A[(1+nrow_dif):length(new_A)]
    new_Y <- new_Y[(1+nrow_dif):length(new_Y)]
  }
  
  combined_W <- data.frame(new_W, autoregress_df)
    results <- causalNullTest(Y = new_Y, A=new_A, W=combined_W, 
                              control=control, p=p)
  return(results)
}

causal_null_double_lag_parallel <- function(Y, A, W,  lags, control, p, autoregress_Y_lags, var_name){
  n_cores <- parallel::detectCores()- 1 #saving 1 core for OS 
  print("cores done")
  cl <- makeCluster(n_cores)
  clusterExport(cl, c("Y", "A", "W", "lags","control", "p", "var_name", 
                      "autoregress_Y_lags"), envir=environment())
  print("export done")
  browser()
  clusterEvalQ(cl, {
    library(tidyverse)
    library(ctsCausal)
    source("functions.R") # Ensure this file is in the working directory
  })
  print("starting parsapply")
  results_list <- parSapply(cl, lags, FUN=function(lag){
    res <- causal_null_double_lag(Y, A, W, control=control, p=p, lag=lag, 
                                  autoregress_Y_lags=autoregress_Y_lags, 
                                  var_name=var_name)
    return(res)
    },
    simplify=FALSE)
 results_list <- paste("lag", lags)
  stopCluster(cl)
  return(results_list)
}


#parallel version of the causall_null_multi function
#testing one lagged variable at a time
causal_null_parallel <- function(Y, A, W, lags, control = list(cross.fit = FALSE,
                                                             verbose=TRUE, g.n.bins = 2:5), p=2){
  n_cores <- parallel::detectCores()- 1 #saving 1 core for OS 
  print("cores done")
  cl <- makeCluster(n_cores)
  clusterExport(cl, c("Y", "A", "W", "lags", "control", "p"), envir=environment())
  print("export done")
  clusterEvalQ(cl, {
    library(tidyverse)
    library(ctsCausal)
    source("functions.R") # Ensure this file is in the working directory
  })
  print("starting parsapply")
   results_list <- parSapply(cl, lags, FUN=function(lag){
    res <- causal_wrapper(Y, A, W, lag, control, p=p)
    return(res)},
    simplify=FALSE)
   names(results_list) <- paste("lag", lags)
   stopCluster(cl)
  return(results_list)
}


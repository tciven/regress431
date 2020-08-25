#' Implements ridge regression with many predictors
#'
#' This function computes coefficients for ridge regression
#' All columns of the provided data frame are used as predictors, except the
#' one specified as a response.
#'
#' No interaction terms are included.
#'
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param lambda A vector of penalty terms to try
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#'
#' @export


ridge_regression <- function(dat, response, lambda) {

  #selecting vars
  x <- dat %>% select(-{{response}})
  y <- dat %>% select({{response}})

  #vars need to be standardized
  x <- scale(as.matrix(x))
  y <- as.matrix(y)

  results <- data.frame()

  #computing coefficients for each penalty term fiven
  for (L in lambda){
    #rudge matrix
    ridge <- diag(L, nrow = ncol(x))

    #using matrix multiplication to compute coeffs
    newbetas <- data.frame(t(solve((t(x) %*% x) + ridge) %*% t(x) %*% y))

    #storing each coeffs in table with corresponding penalty term
    newbetas <- cbind(newbetas, lambda = L)
    results <- rbind(results, newbetas)

  }


  ### This should be a data frame, with columns named
  ### "Intercept" and the same variable names as dat, and also a column
  ### called "lambda".

  return(results)

}



#' Determines the best penalty term from a set of options
#'
#' This function uses a randomly chosen test and training set
#'
#' No interaction terms are included.
#'
#'
#' @param train_dat A data frame to construct the model from
#' @param test_dat A data frame to test the model on
#' @param response The name of a response variable in the data frame (unquoted)
#' @param lambda A vector of penalty terms to try
#'
#' @return A data frame of penalty terms and resulting errors
#'
#' @import dplyr
#'
#' @export
#

find_best_lambda <- function(train_dat, test_dat, response, lambda) {
  #use our train dataset to generate coeffs corresponding to each error term in lambda vector
  coef_mat <- ridge_regression(train_dat, response, lambda)

  coef_mat <- coef_mat %>% select(-lambda)

  test_y <- test_dat %>% select({{response}})

  lambda_errors = data.frame()

  #use each set of coeffs to generate predicted y vector
  for (i in 1:nrow(coef_mat)){
    predvals <- predict_from_coefs(test_dat, response, coef_mat[i,])

    #euclidean error
    error <- sum((predvals[,2] - test_y)^2)

    #adding each lambda and error pair
    newrow <- cbind(lambda = lambda[i], error = error)
    lambda_errors <- rbind(lambda_errors, newrow)
  }

  ### lambda_errors is a data frame with two columns: "lambda" and "error"
  ### For each lambda, the resulting Sum of Squared error is recorded

  return(lambda_errors)

}

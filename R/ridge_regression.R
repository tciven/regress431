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

  x <- dat %>% select(-{{response}})
  y <- dat %>% select({{response}})

  x <- scale(as.matrix(x))
  y <- scale(as.matrix(y))

  x <- cbind(Intercept = 1, x)

  results <- data.frame()

  for (L in lambda){

    ridge <- diag(L, nrow = ncol(x))

    newbetas <- data.frame(t(solve((t(x) %*% x) + ridge) %*% t(x) %*% y))

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
find_best_lambda <- function(train_dat, test_dat, response, lambda) {

  coef_mat <- ridge_regression(train_dat, response, lambda)

  coef_mat <- coef_mat %>% select(-lambda)

  test_y <- test_dat %>% select({{response}})

  lambda_errors = data.frame()

  for (i in 1:nrow(coef_mat)){
    predvals <- predict_from_coefs(test_dat, response, coef_mat[i,])

    error <- sum((predvals[,2] - test_y)^2)

    newrow <- cbind(lambda = lambda[i], error = error)

    lambda_errors <- rbind(lambda_errors, newrow)
  }

  ### lambda_errors should be a data frame with two columns: "lambda" and "error"
  ### For each lambda, you should record the resulting Sum of Squared error
  ### (i.e., the predicted value minus the real value squared) from prediction
  ### on the test dataset.

  return(lambda_errors)
}

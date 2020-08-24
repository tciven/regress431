#' Implements simple linear regression by gradient descent
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param explanatory The name of the explanatory variable in the data frame (unquoted)
#' @param errplot Boolean to plot error and two coeffs
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#'
#' @export
slr_gd <- function(dat, response, explanatory, errplot = F){


  ### Compute coefficients by gradient descent
  ### Return a data frame of the same form as in the `simple_linear_regression`

  x <- dat %>% select({{explanatory}})
  y <- dat %>% select({{response}})

  xnames <- c("Intercept", names(x))

  x <- as.matrix(cbind(Intercept = 1, x))

  #establish the learning rate
  lr <- .000001

  #coeff mat, guess coeffs are b0 = 0, b1 = 0
  gdc = as.matrix(c(0, 0))

  n = length(y)

  #intital error
  error <- sum((x %*% gdc - y))^2/n

  #how long the algorithm should run for
  nrep = 500000

  checker = nrep/100

  #set up to plot errors
  if (errplot){
    slops = rep(0, 100)
    ints = rep(0, 100)
    errs = rep(0, 100)
    }

  #each loop is
  for (i in 1:nrep){

    #predict based on current coeffs
    preds = x %*% gdc

    #vector of errors from actual y
    differ = y - preds

    #compute slope error (delta m) and subtract from current slope
    DM <- -2 * sum(x[,2] * (differ)) / n
    gdc[2] <- gdc[2] - lr * DM

    #compute intercept error (delta b) and subtract from current intercept
    DB <- -2 * sum(differ) / n
    gdc[1] <- gdc[1] - lr * DB

    #progress counter and error store
    if (i %% checker == 0){
      print(paste(i * 100 /nrep,"% Done"))

      if (errplot){
        store = i/checker
        slops[store] = gdc[2]
        ints[store] = gdc[1]
        errs[store] = sum(differ^2)/n
      }
    }
  }

  #renaming coeffs
  results <- data.frame(t(gdc))
  names(results) <- xnames

  #plotting error and path of coeffs
  if (errplot){
    plot(slops, main = "Slopes", xlab = "Iteration in 10,000s", ylab = "Slope")
    plot(ints, main = "Intercepts", xlab = "Iteration in 10,000s", ylab = "Intercept")
    plot(errs, main = "Error", xlab = "Iteration in 10,000s", ylab = "Error")
    }

  return(results)
}





#' Implements linear regression with many predictors by gradient descent
#'
#' This function computes coefficients for multiple regression by gradient descent
#' All columns of the provided data frame are used as predictors, except the
#' one specified as a response.
#'
#' No interaction terms are included.
#'
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#'
#'@export
#'
#'



mlr_gd <- function(dat, response) {
  #learning rate and repition hyperparameters
  lr = .000001
  nrep = 500000

  #grabbing vars
  x <- dat %>% select(-{{response}})
  y <- dat %>% select({{response}})

  n <- length(y)

  xnames <- c("Intercept", names(x))

  #adding intercept column of all ones
  x <- as.matrix(cbind(Intercept = 1,x))

  #defining matrix of predictor variables
  nx <- ncol(x)
  xnames <- names(x)

  #matrix of coeffs
  gdc <- matrix(0, nx)


  for (i in 1:nrep){
    #prediciton and error vector using current coeffs
    preds = x %*% gdc
    diff = y - preds

    #for each x var calculate partial error and subtract from coef
    for (j in 2:nx){
      DM <- -2 * sum(x[,j] * diff) / n
      gdc[j] <- gdc[j] - lr * DM

    }

    #partial error / adjsutment for intercept done seperately
    DB <- -2 * sum(diff) / n
    gdc[1] <- gdc[1] - lr * DB

    error <- sum(diff^2)/n

  }

  #naming coeffs in table
  results <- data.frame(t(gdc))
  names(results) <- xnames

  return(results)

}



#'
#' A small function to implement linear projections
#'
#' @param u vector to be projected
#' @param a vector to be projected upon
#'
#' @return A vector: the projection of u onto a
#'
#'@export

proj <- function(u, a){

  return(as.numeric(((t(a) %*% u) / (t(u) %*% u))) %*% u)

}





#'
#' Implements the Gram-Schmidt process for creating Q and R decomposition matrices
#'
#' @param x the data matrix to be decomposed
#'
#' @return a list of the q and r matrices
#'
#' @export


gramsch <- function(x){

  rank = ncol(x)
  dim = nrow(x)

  #establish empty matrixes to fill in
  q = matrix(0, dim, rank)
  u = matrix(0, dim, rank)
  r = matrix(0, rank, rank)

  #iterate through # of vars in x
  for (i in 1:rank){
    subsum = rep(0, dim)

    #project each of the previous vectors onto the current one
    #leave first column alone. only needs to be scaled
    if (i > 1){
      for (j in 1:(i-1)){
        subsum <- subsum + proj(u[,j], x[,i])
      }
    }

    #sutract all of those projections from current vector
    u[,i] = x[,i] - subsum

    #scale current vector to be of unit length
    q[,i] <- u[,i] / sqrt(sum(u[,i]^2))

    #(k, l)th entry of r matrix is k column of q by l column of x
    for (k in 1:i) {
      for (l in i:rank){
        r[k,l] <- q[,k] %*% x[,l]
      }
    }
  }

  return(list(q = q, r = r))

}



#' Implements linear regression with many predictors by matrix decomposition
#'
#' This function computes coefficients for multiple regression by QR matrix decomposition
#' All columns of the provided data frame are used as predictors, except the
#' one specified as a response.
#'
#' No interaction terms are included.
#'
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#'
#'@export
#'


mlr_qr <- function(dat, response) {
  #pull desired vars
  x <- dat %>% select(-{{response}})
  y <- dat %>% select({{response}})

  x <- cbind(Intercept = 1, x)

  xnames <- names(x)

  x <- as.matrix(x)
  y <- as.matrix(y)

  #use the gram schimdt process to find orthogonal basis
  decomp <- gramsch(x)

  q <- decomp$q
  r <- decomp$r

  #use q and r from GS and y to compute coefficients
  b <- solve(r) %*% t(q) %*% y

  #relable coeffs with var names
  results <- data.frame(t(b))
  names(results) <- xnames


  return(results)

}


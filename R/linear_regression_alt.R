#' Implements simple linear regression by gradient descent
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param explanatory The name of the explanatory variable in the data frame (unquoted)
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#'
#' @export
slr_gd <- function(dat, response, explanatory){


  ### Compute coefficients by gradient descent
  ### Return a data frame of the same form as in the `simple_linear_regression`

  x <- dat %>% select({{explanatory}})
  y <- dat %>% select({{response}})

  xnames <- c("Intercept", names(x))

  x <- as.matrix(cbind(Intercept = 1, x))

  lr <- .000001

  gdc = as.matrix(c(0, 0))

  n = length(y)

  error <- sum((x %*% gdc - y))^2/n

  nrep = 500000

  checker = nrep/100

  # slops = rep(0, 100)
  # ints = rep(0, 100)
  # errs = rep(0, 100)

  for (i in 1:nrep){
    preds = x %*% gdc
    differ = y - preds

    if (i %% checker == 0){
      print(paste(i * 100 /nrep,"% Done"))

      # store = i/checker
      # slops[store] = gdc[2]
      # ints[store] = gdc[1]
      # errs[store] = sum(differ^2)/n
      }

    DM <- -2 * sum(x[,2] * (differ)) / n
    gdc[2] <- gdc[2] - lr * DM


    DB <- -2 * sum(differ) / n
    gdc[1] <- gdc[1] - lr * DB

  }

  results <- data.frame(t(gdc))
  names(results) <- xnames

  # plot(slops, main = "Slopes", xlab = "Iteration in 10,000s", ylab = "Slope")
  # plot(ints, main = "Intercepts", xlab = "Iteration in 10,000s", ylab = "Intercept")
  # plot(errs, main = "Error", xlab = "Iteration in 10,000s", ylab = "Error")

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
  lr = .000001
  nrep = 500000

  checker = nrep/100

  x <- dat %>% select(-{{response}})
  y <- dat %>% select({{response}})

  n <- length(y)

  xnames <- c("Intercept", names(x))

  x <- as.matrix(cbind(Intercept = 1,x))

  nx <- ncol(x)
  xnames <- names(x)

  gdc <- matrix(0, nx)
  print(gdc)
  print(x)


  for (i in 1:nrep){

    if (i %% checker == 0){
      print(paste(i * 100 /nrep,"% Done"))
    }

    preds = x %*% gdc
    diff = y - preds

    for (j in 2:nx){
      DM <- -2 * sum(x[,j] * diff) / n
      gdc[j] <- gdc[j] - lr * DM

    }

    DB <- -2 * sum(diff) / n
    gdc[1] <- gdc[1] - lr * DB

    error <- sum(diff^2)/n

  }


  results <- data.frame(t(gdc))
  names(results) <- xnames

  return(results)

}

#'
#'A small function to implement linear projections
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
#' Implements the Gram-Schmidt process for creating the Q and R matrices
#'
#' @param x the data matrix to be decomposed
#'
#' @return a list of the q and r matrices
#'
#' @export


gramsch <- function(x){

  rank = ncol(x)
  dim = nrow(x)

  q = matrix(0, dim, rank)
  u = matrix(0, dim, rank)
  r = matrix(0, rank, rank)

  for (i in 1:rank){
    subsum = rep(0, dim)

    if (i > 1){
      for (j in 1:(i-1)){
        subsum <- subsum + proj(u[,j], x[,i])
      }
    }

    u[,i] = x[,i] - subsum

    q[,i] <- u[,i] / sqrt(sum(u[,i]^2))

    for (k in 1:i) {
      for (l in i:rank){
        r[k,l] <- q[,k] %*% x[,l]
      }
    }
  }
  print(q)
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
  x <- dat %>% select(-{{response}})
  y <- dat %>% select({{response}})

  x <- cbind(Intercept = 1, x)

  xnames <- names(x)

  x <- as.matrix(x)
  y <- as.matrix(y)

  decomp <- gramsch(x)

  q <- decomp$q
  r <- decomp$r

  b <- solve(r) %*% t(q) %*% y

  results <- data.frame(t(b))
  names(results) <- xnames

  ### Compute coefficients by QR decomposition
  ### Return a data frame of the same form as in the `multiple_linear_regression`

  return(results)

}


#' Maximum Likelihood Estimation
#'
#' This function evaluates a log-likelihood function over a range of parameter values
#' and identifies the maximum likelihood estimate (MLE) of the parameters based on the provided data.
#'
#' @param lfun A function that calculates the log-likelihood for given data and parameters.
#' @param x A numeric vector of data points or observations for which the log-likelihood will be calculated.
#' @param param A numeric vector of parameter values to evaluate against the log-likelihood function.
#' @param ... Additional graphical parameters to customize the plot, such as `col`, `lwd`, etc.
#'
#' @return A list containing:
#'   \item{i}{Index of the parameter value that maximizes the log-likelihood.}
#'   \item{parami}{Parameter value that maximizes the log-likelihood.}
#'   \item{yi}{Maximum log-likelihood value corresponding to the maximum parameter.}
#'   \item{slope}{Slope values calculated around the maximum; "NA" if out of bounds (less than 1 or more than the number of parameter values).}
#' @importFrom graphics axis abline points plot
#' @export
mymaxlik = function (lfun, x, param, ...) {
  #how many param values are there?
  np = length(param)
  i <- NULL

  #outer--notice the order, x then param
  #this produces a matrix - try outer(1:4,5:10,function(x,y) paste(x,y,sep =" " )) to understand
  z = outer(x, param, lfun) #A
  # z is a matrix where each x, param is replaced with the function evaluated at those values
  y = apply(z, 2, sum)

  #y is a vector made up of the column sums
  #Each y is the log lik for a new parameter value plot(param, y, col = "Blue", type = "l", lwd = 2, ...)
  #which gives the index for the value of y >= max.
  #there could be a max between two values of the parameters, therefore 2 indices
  #the first max will take the larger indice
  plot(param, y, col = "Blue", type = "l", lwd = 2, ...)  # Add this line before abline
  abline(v = param[i], lwd = 2, col = "Red")

  #plots a nice point where the max lik is points(param[i], y[i], pch = 19, cex = 1.5, col = "Black")
  axis(3, param[i], round(param[i], 2))
  #check slopes. If it is a max the slope should change from + to
  #We should get three + and two -vs
  slope <- NA
  ifelse(i-3 >= 1 & i + 2 <= np, slope <-(y[(i-2):(i+2)]-y[(i-3):(i+1)])/(param[(i-2):(i+2)]-param[(i-3):(i+1)]), slope <-"NA")
  return(list(i=i, parami = param[i], yi = y[i], slope = slope))
}

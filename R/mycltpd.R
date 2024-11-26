#' Central Limit Theorem for Poisson Distribution
#'
#' This function simulates the Central Limit Theorem by drawing random samples from a Poisson distribution, calculating the sample means, and visualizing their distribution using histograms. It also generates a barplot for the sample values and compares the histogram of sample means with the theoretical normal distribution.
#'
#' @param n Integer. The sample size for each iteration.
#' @param iter Integer. The number of iterations to simulate.
#' @param lambda Numeric. The rate (mean) parameter for the Poisson distribution. Default is 10.
#' @param ... Additional arguments to pass to the histogram function.
#'
#' @return A layout with three plots:
#' @export
#' @importFrom grDevices rainbow
#' @importFrom graphics barplot curve hist
#' @importFrom stats dnorm dpois rpois
#'
#' @examples
#' # Simulate with sample size 10, 1000 iterations, and lambda = 10
#' mycltp(n = 10, iter = 1000, lambda = 10)
#'
mycltp=function(n, iter, lambda=10, ...) {

  ## r-random sample from the Poisson
  y = rpois(n * iter, lambda = lambda)

  ## Place these numbers into a matrix
  ## The columns will correspond to the iteration and the rows will equal the sample size n
  data = matrix(y, nrow = n, ncol = iter, byrow = TRUE)

  ## Apply the function mean to the columns (2) of the matrix
  ## These are placed in a vector w
  w = apply(data, 2, mean)

  ## We will make a histogram of the values in w
  param = hist(w, plot = FALSE)

  ## Find the max density
  ymax = max(param$density)

  ## Add 10% more to ymax
  ymax = 1.1 * ymax

  ## Layout for graphing
  layout(matrix(c(1, 1, 2, 3), nrow = 2, ncol = 2, byrow = TRUE))

  ## Create the histogram
  hist(w, freq = FALSE, ylim = c(0, ymax), col = rainbow(max(w)),
       main = paste("Histogram of sample mean", "\n", "sample size= ", n, " iter=", iter, " lambda=", lambda, sep = ""),
       xlab = "Sample mean", ...)

  ## Add a theoretical normal curve
  curve(dnorm(x, mean = lambda, sd = sqrt(lambda/n)), add = TRUE, col = "Red", lty = 2, lwd = 3)

  ## Barplot of the sampled values
  barplot(table(y) / (n * iter), col = rainbow(max(y)), main = "Barplot of sampled y", ylab = "Rel. Freq", xlab = "y")

  ## Plot the Poisson probability function
  x = 0:max(y)
  plot(x, dpois(x, lambda = lambda), type = "h", lwd = 5, col = rainbow(max(y)),
       main = "Probability function for Poisson", ylab = "Probability", xlab = "y")
}

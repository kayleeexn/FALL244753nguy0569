#' Bootstrap
#'
#' @param iter Number of bootstrap iterations
#' @param x Numeric vector containing sample data
#' @param fun Function to apply to each bootstrap sample (e.g., "mean")
#' @param alpha Significance level for the confidence interval
#' @param cx Numeric. Scaling factor for text size in the plot. Defaults to 1.5.
#' @param ... Additional graphical parameters to be passed to `hist`
#'
#' @return A list with the confidence interval and applied function
#' @export
#' @importFrom graphics segments text
#' @importFrom stats quantile
#'
#' @examples
#' # Example usage of myboot
#' set.seed(123)
#' sample_data <- rnorm(100)
#' myboot(iter = 1000, x = sample_data)
myboot <- function(iter = 10000, x, fun = "mean", alpha = 0.05, cx = 1.5, ...) {
  n = length(x)
  y = sample(x, n * iter, replace = TRUE)
  rs.mat = matrix(y, nrow = n, ncol = iter, byrow = TRUE)
  xstat = apply(rs.mat, 2, match.fun(fun))
  ci = quantile(xstat, c(alpha/2, 1 - alpha/2))
  para = hist(xstat, freq = FALSE, las = 1, main = paste("Histogram of Bootstrap sample statistics", "\n", "alpha = ", alpha, " iter = ", iter, sep = ""), ...)
  mat = matrix(x, nrow = length(x), ncol = 1, byrow = TRUE)
  pte = apply(mat, 2, match.fun(fun))
  abline(v = pte, lwd = 3, col = "grey")
  segments(ci[1], 0, ci[2], 0, lwd = 4)
  text(ci[1], 0, paste("(", round(ci[1], 2), sep = ""), col = "Red", cex = cx)
  text(ci[2], 0, paste(round(ci[2], 2), ")", sep = ""), col = "Red", cex = cx)
  text(pte, max(para$density) / 2, round(pte, 2), cex = cx)
  return(list(ci = ci, fun = fun, x = x))
}

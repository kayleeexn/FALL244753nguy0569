#' Binomial Experiment
#'
#' @param iter Number of iterations (the number of binomial experiments to run)
#' @param n Number of trials per binomial experiment
#' @param p Probability of success for each trial in the binomial experiment
#'
#' @return A vector containing the number of successes in each binomial experiment.
#' @export
#'
#' @examples
#' mybin(iter = 1000, n = 20, p = 0.5)
mybin = function(iter, n, p) {
  # Make a matrix to hold samples, initialize filled with NA's
  sam.mat = matrix(NA, nrow = n, ncol = iter, byrow = TRUE)

  # Make vector to hold the number of successes in each trial
  succ = c()
  for(i in 1:iter) {
    # Fill each column with a new sample
    sam.mat[,i] = sample(c(1,0), n, replace = TRUE, prob = c(p, 1 - p))

    # Calculate the sum from the sample
    succ[i] = sum(sam.mat[,i])
  }

  return(succ)  # Return the number of successes
}

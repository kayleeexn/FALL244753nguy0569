
#' Hypergeometric Experiment Simulation
#'
#' @param iter Integer, how many samplings are repeated, default 100
#' @param N Integer, total size of population or total number of items in a population, default 20
#' @param r Integer, number of considered successes in the population, default 12
#' @param n Integer, sample size, how many are drawn in each iteration, default 5
#'
#' @returnA table of proportions of successes from the hypergeometric distribution simulation.
#'
#' @export
#'
#' @examples
#' myhyper(iter = 1000, N = 20, r = 12, n = 5)
#' this represents the default settings of the simulation
#' should print out a table of information

myhyper = function(iter = 100, N = 20, r = 12, n = 5) {
  #make a matrix to hold samples, initialize filled with NA's
  sam.mat = matrix(NA, nr = n, nc = iter, byrow = TRUE)

  #make vector to hold the number of success in each trial
  succ = c()
  for(i in 1:iter) {
    #fill each column with a new sample
    sam.mat[,i] = sample(rep(c(1,0), c(r, N - r)), n, replace = FALSE)

    #calculate the sum from the sample
    succ[i] = sum(sam.mat[,i])
  }

  #make table for the successes
  suc.tab = table(factor(succ, levels = 0:n))

  #make barplot of the proportions
  barplot(suc.tab/(iter), col = rainbow(n + 1), main = "Hypergeometric Simulation", xlab = "Number of Successes")
  suc.tab/iter
}

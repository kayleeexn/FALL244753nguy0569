#' Optimal Number of Tickets Calculation
#'
#' This function calculates the optimal number of tickets to sell for an event
#' given the event capacity, risk tolerance (`gamma`), and probability of
#' attendance (`p`). It computes both discrete and continuous approximations
#' and plots the results for comparison.
#'
#' @param N The number of available seats or capacity of the event. Must be a positive integer.
#' @param gamma The allowable risk of overbooking. A probability value between 0 and 1.
#' @param p The probability that a ticket holder attends the event. A value between 0 and 1.
#' @return A list with the optimal number of tickets to sell for both discrete (`nd`) and continuous (`nc`) cases.
#' @importFrom stats pbinom pnorm
#' @importFrom graphics plot points lines abline layout
#' @export
#'
#' @examples
#' # Example usage of ntickets function:
#' result <- ntickets(N = 100, gamma = 0.02, p = 0.95)
#' print(result)
ntickets <- function(N, gamma, p) {
  # Discrete calculation
  n <- seq(N, floor(N + N / 10), by = 1)
  obj_discrete <- 1 - gamma - pbinom(N, size = n, prob = p)
  ind_discrete <- which.min(abs(obj_discrete))
  nd <- n[ind_discrete]

  # Continuous calculation
  n_continuous <- seq(N, floor(N + N / 10), by = 0.001)
  obj_continuous <- 1 - gamma - pnorm(N + 0.5, mean = n_continuous * p, sd = sqrt(n_continuous * p * (1 - p)))
  ind_continuous <- which.min(abs(obj_continuous))
  nc <- n_continuous[ind_continuous]

  # Plot the objective function for both discrete and continuous cases
  layout(matrix(1:2, nrow = 2, byrow = TRUE))

  # Plot the discrete calculation
  plot(n, obj_discrete, type = "n", ylab = "Objective", pch = 16,
       col = ifelse(n == nd, "lightpink", "black"),
       main = paste("Objective vs n to find optimal tickets sold (", nd, ")\n",
                    "gamma = ", gamma, " N = ", N, " discrete"))
  points(n, obj_discrete, pch = 16, cex = 0.75, col = ifelse(n == nd, "lightpink", "black"))
  lines(n, obj_discrete, col = ifelse(n == nd, "lightpink", "black"), lwd = 0.75)
  abline(h = obj_discrete[ind_discrete], v = nd, col = "lightpink")

  # Plot the continuous calculation
  plot(n_continuous, obj_continuous, type = "l", xlab = "n", ylab = "Objective",
       main = paste("Objective vs n to find optimal tickets sold (", round(nc, 2), ")\n",
                    "gamma = ", gamma, " N = ", N, " continuous"))
  abline(h = obj_continuous[ind_continuous], v = nc, col = "cyan")

  # Return a named list
  return(list(nd = nd, nc = round(nc, 2), N = N, p = p, gamma = gamma))
}

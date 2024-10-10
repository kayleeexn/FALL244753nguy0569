#' Calculate Optimal Number of Tickets to Sell for an Overbooked Flight
#'
#' This function calculates the optimal number of tickets to sell for an overbooked flight using both discrete and continuous approximations.
#' It helps maximize revenue while minimizing the risk of overbooking.
#'
#' @param N Number of Seats on a flight.
#' @param gamma Confidence level (e.g., 0.05 for 95% confidence).
#' @param p Probability of a passenger showing up.
#'
#' @return A named list containing
#' \describe{
#' \item{nd}{the optimal number of tickets to sell based on discrete calculations.}
#'   \item{nc}{the optimal number of tickets to sell based on continuous calculations.}
#'   \item{N}{the number of seats available (input).}
#'   \item{p}{probability that the passenger shows up (input).}
#'   \item{gamma}{confidence level that the airline wants to achieve regarding the risk of overbooking (input).}
#' }
#'
#' @export
#'
#' @examples
#' ntickets(N = 400, gamma = 0.02, p = 0.95)
ntickets <- function(N, gamma, p) {
  #Discrete Distribution Calculations
  n <- seq(N, floor(N + N/10), by = 1)
  obj_discrete <- 1 - gamma - pbinom(N, size = n, prob = p)
  ind_discrete <- which.min(abs(obj_discrete))
  nd <- n[ind_discrete]

  #Continuous Calculation
  n_cont <- seq(N, floor(N + N/10), by = 0.001)
  obj_cont <- 1 - gamma - pnorm(N + 0.5, mean = n_cont * p, sd = sqrt(n_cont * p * (1 - p)))
  ind_cont <- which.min(abs(obj_cont))
  nc <- n_cont[ind_cont]

  layout(matrix(1:2, nrow = 2, byrow = TRUE))

  #Discrete Plot
  plot(n, obj_discrete, type = "n", main = paste("Objective Vs n to find optimal tickets sold (", nd, ")\n", "gamma=", gamma, " N=", N, " discrete"), ylab = "Objective", pch = 16, col = ifelse(n == nd, "lightpink", "black"))
  points(n, obj_discrete, pch = 16, cex = 0.75, col = ifelse(n == nd, "lightpink", "black"))
  lines(n, obj_discrete, col = ifelse(n == nd, "lightpink", "black"), lwd = 0.5)
  abline(h = obj_discrete[ind_discrete], v = nd, col = "lightpink")

  #Continuous Plot
  plot(n_cont, obj_cont, type = "l", main = paste("Objective Vs n to find optimal tickets sold (",round(nc, 2),")\n", "gamma=", gamma, " N=", N, " continuous"), xlab = "n", ylab = "Objective")
  abline(h = obj_cont[ind_cont], v = nc, col = "cyan")

  #Return a named list
  return(list(nd = nd, nc = round(nc, 2), N = N, p = p, gamma = gamma))
}

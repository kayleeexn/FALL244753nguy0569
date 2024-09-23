#' @title Random Plotting in LAB3
#'
#' @param data data gathered from spruce data frame
#'
#' @return A sophisticated plot
#' @export
#'
#' @examples see LAB3

plot_tree <- function(data) {
  plot(Height~BHDiameter, data = spruce.df,
     main = "Tree Height vs BHDiameter",
     xlab = "Breast Height Diameter (cm)",
     ylab = "Height of Tree (m)",
     pch = 21, bg = "blue", cex = 1.2,
     xlim = c(0, max(BHDiameter) * 1.1),
     ylim = c(0, max(Height) * 1.1)
     )
}

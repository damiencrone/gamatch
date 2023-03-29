#' Compute F-statistic for ANOVA
#'
#' This function computes the F-statistic for a one-way ANOVA.
#'
#' @param x A numeric vector of data values.
#' @param group A factor vector specifying the groups to which the data values belong.
#'
#' @return The F-statistic for the one-way ANOVA.
#'
#' @examples
#' set.seed(123)
#' x <- rnorm(100)
#' group <- rep(letters[1:4], 25)
#' compute_f(x, group)
#'
#' @export
compute_f = function (x, group) {
  gm = tapply(x, group, mean)
  gv = tapply(x, group, var)
  m = mean(x)
  ssb = sum((gm - m)^2) * length(gm)
  ssw = sum((x - gm[group])^2)
  dfb = length(gm) - 1
  dfw = length(x) - length(gm)
  fstat = (ssb / dfb) / (ssw / dfw)
  fstat
}

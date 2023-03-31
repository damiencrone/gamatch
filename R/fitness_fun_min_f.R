#' Calculates the fitness of a genetic algorithm solution for a experimental
#' design problem with a minimum f-statistic objective.
#'
#' @param x A binary vector representing the genetic algorithm solution.
#' @param nsub An integer specifying the number of subjects in the dataset
#' @param ncond An integer specifying the number of experimental conditions in
#'   the experiment.
#' @param npg An integer specifying the number of subjects per condition.
#' @param data A data frame containing the variables on which subjects are to be
#'   matched.
#'
#' @return A numeric value representing the fitness of the genetic algorithm
#'   solution.
#'
#' @export
fitness_fun_min_f = function (x, nsub, ncond, npg, data) {

  xmat = matrix(data = x, nrow = nsub, ncol = ncond)
  multiple_condition_assignments = any(rowSums(xmat) > 1)
  wrong_n_per_group = any(colSums(xmat) != npg)
  wrong_n_assignments = sum(xmat) != npg*ncond
  var_names = colnames(data)

  is_invalid = multiple_condition_assignments |
    wrong_n_per_group |
    wrong_n_assignments
  if (is_invalid) {
    return(-Inf)
  }

  data$condition = NA
  assignment_mat = apply(xmat == 1, 2, which)
  for (i in 1:nrow(assignment_mat)) {
    ind = assignment_mat[i,]
    data$condition[ind] = i
  }

  f_vec = rep(NA, length(var_names))
  names(f_vec) = var_names
  g = as.factor(data$condition)
  ind = !is.na(g)
  for (v in var_names) {
    y = data[, v]
    f_vec[v] = compute_f(x = y[ind], group = g[ind])
  }
  fitness = -mean(f_vec)
  return(fitness)
}



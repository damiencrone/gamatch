#' Extracts the group assignments from a genetic algorithm output object.
#'
#' @param ga_output An object of class \code{ga-class}, representing the output
#'   of a genetic algorithm run.
#' @param nsub An integer specifying the number of subjects.
#' @param ngroups An integer specifying the number of matched groups of N = 1
#'   per condition to which subjects are to be assigned.
#'
#' @return A matrix with \code{nsub} rows and \code{ngroups} columns, where each
#'   row represents a subject and each column represents a set of matched
#'   subjects.
#'
#' @export
extract_group_assignment = function(ga_output, nsub, ngroups) {
  assignment_mat = matrix(data = ga_output@solution,
                          nrow = nsub,
                          ncol = ngroups)
  assignment_mat = apply(assignment_mat == 1, 2, which)
  assignment_mat = assignment_mat[sample(1:nrow(assignment_mat)),] # Shuffle row order
  return(assignment_mat)
}

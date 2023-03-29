#' Generates suggestions for a genetic algorithm solution.
#'
#' @param nsuggestions An integer specifying the number of suggestions to
#'   generate.
#' @param nsub An integer specifying the number of subjects requiring matching.
#' @param ngroups An integer specifying the number of conditions to which
#'   subjects are to be assigned
#'
#' @return A binary matrix with \code{nsuggestions} rows and \code{nsub *
#'   ngroups} columns, where each row represents a suggested genetic algorithm
#'   solution.
#'
#' @examples
#' generate_suggestions(5, 20, 4)
#'
#' @export
generate_suggestions = function (nsuggestions, nsub, ngroups) {
  npg = floor(nsub/ngroups)
  nbits = ngroups*nsub
  suggestion_mat = matrix(0, nrow = nsuggestions, ncol = nbits)
  for (i in 1:nsuggestions) {
    xmat = matrix(data = 0, nrow = nsub, ncol = ngroups)
    assignment = sample(x = rep(1:ngroups, npg))
    pvec = sample(x = 1:nrow(xmat), size = length(assignment))
    for (j in 1:length(assignment)) {
      row_ind = pvec[j]
      col_ind = assignment[j]
      xmat[row_ind, col_ind] = 1
    }
    suggestion_mat[i,] = as.vector(xmat)
  }
  return(suggestion_mat)
}

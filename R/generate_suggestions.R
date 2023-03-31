#' Generates suggestions for a genetic algorithm solution.
#'
#' @param nsuggestions An integer specifying the number of suggestions to
#'   generate.
#' @param nsub An integer specifying the number of subjects requiring matching.
#' @param ncond An integer specifying the number of conditions to which
#'   subjects are to be assigned
#'
#' @return A binary matrix with \code{nsuggestions} rows and \code{nsub *
#'   ncond} columns, where each row represents a suggested genetic algorithm
#'   solution.
#'
#' @examples
#' generate_suggestions(5, 20, 4)
#'
#' @export
generate_suggestions = function (nsuggestions, nsub, ncond) {
  npg = floor(nsub/ncond)
  nbits = ncond*nsub
  suggestion_mat = matrix(0, nrow = nsuggestions, ncol = nbits)
  for (i in 1:nsuggestions) {
    xmat = matrix(data = 0, nrow = nsub, ncol = ncond)
    for (j in 1:ncol(xmat)) {
      k = (j-1)*npg + 1:npg
      xmat[k, j] = 1
    }
    xmat = xmat[sample(1:nrow(xmat)),]
    suggestion_mat[i,] = as.vector(xmat)
  }
  return(suggestion_mat)
}

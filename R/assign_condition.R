#' Assigns conditions to subjects based on a group assignment matrix.
#'
#' This function assigns treatment conditions to subjects in a dataset based on
#' a group assignment matrix. The group assignment matrix specifies which
#' subjects are assigned to each treatment group.
#'
#' @param data A data frame containing the data to which conditions will be
#'   assigned.
#' @param group_labels A vector of treatment group labels. The length of this
#'   vector should be equal to the number of treatment groups in the group
#'   assignment matrix.
#' @param assignment_mat A matrix with \code{nsub} rows and \code{ngroups}
#'   columns, where each row represents a subject and each column represents a
#'   treatment group. The matrix specifies which subjects are assigned to each
#'   treatment group.
#' @param groupvar A string specifying the name of the variable in \code{data}
#'   that will store the treatment condition assignments. The default value is
#'   \code{"condition"}.
#'
#' @return A data frame with the same columns as \code{data}, with a new column
#'   added for the treatment condition assignments.
#'
#' @export
assign_condition = function(data, group_labels, assignment_mat, groupvar = "condition") {
  for (i in 1:length(group_labels)) {
    row_ind = rownames(data)[assignment_mat[i,]]
    data[row_ind, groupvar] = group_labels[i]
  }

  data_remainder = data[-unlist(assignment_mat),]
  if (nrow(data_remainder)) {
    remainder_assignment = sample(x = group_labels,
                                  size = nrow(data_remainder))
    data[rownames(data_remainder), groupvar] = remainder_assignment
  }
  return(data)
}

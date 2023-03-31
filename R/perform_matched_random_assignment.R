#' Perform matched random assignment
#'
#' This function performs matched random assignment of subjects to conditions
#' using a binary genetic algorithm (GA). The GA algorithm seeks to
#' pseudo-randomly assign subjects to \code{ncond} groups while minimizing
#' between group variance on all variables in \code{data}.
#'
#' @param data A data frame containing the variables to be used in the
#'   assignment procedure.
#' @param ncond An integer specifying the number of conditions to which subjects
#'   will be assigned.
#' @param nsuggestions An optional integer specifying the number of random
#'   suggestions to generate for the GA algorithm. Default is 500.
#' @return A data frame with an additional column indicating the assigned
#'   condition for each subject.
#' @export
perform_matched_random_assignment = function (data, ncond, nsuggestions = 500) {
  nsub = nrow(data)
  npg = floor(nsub/ngroups)
  nbits = ncond*nsub
  suggestion_mat = generate_suggestions(nsuggestions, nsub, ncond)
  ga_output = ga(
    type = "binary",
    fitness = fitness_fun_min_f,
    nBits = nbits,
    maxiter = 10000, # Maximum number of generations
    run = 50,       # Stop if the best-so-far fitness hasn't improved for 'run' generations
    popSize = 500,
    suggestions = suggestion_mat,
    # Additional fitness function inputs
    nsub    = nsub,
    ncond   = ncond,
    npg     = npg,
    data    = data
  )
  assignment_mat = extract_group_assignment(ga_output, nsub, ncond)
  data = assign_condition(data = data,
                          group_labels = condition_labels,
                          assignment_mat = assignment_mat)
  return(data)
}

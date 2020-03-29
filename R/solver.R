getSolver <- function(solver) {
  if(is.null(solver)) {
    tolower(prioritizr:::default_solver_name())
  } else if (tolower(solver) %in% c("gurobi", "rsymphony", "lpsymphony")) {
    tolower(solver)
  } else {
    stop("'solver' must be one of 'gurobi', 'rsymphony', 'lpsymphony' or NULL")
  }
}

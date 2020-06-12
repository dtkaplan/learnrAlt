#' Trivial exercise evaluator for demonstration purposes
#'
#' @export
trivial_evaluator <- function(exercise, envir, evaluate_global_setup = FALSE) {
  list(
    feedback = NULL,
    error_message = NULL,
    timeout_exceeded = FALSE,
    html_output = "The trivial evaluator doesn't do anything with your code. Sorry. See learnrAlt::evaluate_exercise2() for a useful evaluator."
  )
}

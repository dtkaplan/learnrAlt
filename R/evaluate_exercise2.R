#' Danny's version of exercise evaluation
#'
#' @export
evaluate_exercise2 <- function(exercise, envir,
                               evaluate_global_setup = FALSE) {

  cat("Using the alternative evaluator\n")
  # return immediately and clear visible results
  # do not consider this an exercise submission
  if (
    nchar(
      learnr:::str_trim(paste0(exercise$code, collapse = "\n"))
    ) == 0
  ) {
    return(learnr:::empty_result())
  }

  if (evaluate_global_setup) {
    eval(parse(text = exercise$global_setup), envir = envir)
  }

  # capture a copy of the envir before any execution is done
  envir_prep <- duplicate_env(envir)

  # "global" err object to look for
  err <- NULL

  checker_feedback <- NULL # Part of kluge for when there is no checker function.
  cat("exercise checker is\n")
  cat(str(exercise$options$exercise.checker), "\n")
  # see if we need to do code checking
  if (!is.null(exercise$options$exercise.checker) && exercise$options$exercise.checker != "NULL") {
    cat("Running the checker\n")
    # get the checker
    tryCatch({
      checker <- eval(parse(text = exercise$options$exercise.checker), envir = envir)
    }, error = function(e) {
      message("Error occured while retrieving 'exercise.checker'. Error:\n", e)
      err <<- e$message
    })
    if (!is.null(err)) {
      return(learnr:::error_result("Error occured while retrieving 'exercise.checker'."))
    }

    # call the checker
    cat("running the checker again\n")
    tryCatch({
      checker_feedback <- checker(
        label = exercise$label,
        user_code = exercise$code,
        solution_code = exercise$solution,
        check_code = exercise$check,
        envir_result = NULL,
        evaluate_result = NULL,
        envir_prep = envir_prep,
        last_value = NULL,
        parsing_phase = TRUE,
        setup = exercise$setup,
        # global setup doesn't seem to be used
        global_setup = exercise$global_setup,
        exercise = exercise
      )
    }, error = function(e) {
      err <<- e$message
      message("Error occured while evaluating initial 'exercise.checker'. Error:\n", e)
    })
    if (!is.null(err)) {
      return(learnr:::error_result(
        paste("This error occured while evaluating the code:",
              err, collapse = "\n"))
      )
    }

    # if it's an 'incorrect' feedback result during parsing then return it
    if (!is.null(checker_feedback) && is.list(checker_feedback)) {
      learnr:::feedback_validated(checker_feedback)
      if ("parsing_phase" %in% names(checker_feedback) &&
          checker_feedback$parsing_phase) {
        if (!checker_feedback$correct) {
          return(list(
            feedback = checker_feedback,
            error_message = NULL,
            timeout_exceeded = FALSE,
            html_output = feedback_as_html(checker_feedback)
          ))
        }
      }

    }
  }

  # if we got here, we passed the parsing_phase checking and evaluation
  html_output <-
    display_exercise_results(exercise, envir,
                             evaluate_global_setup = FALSE, err = err)

  # now handle any non-parsing phase feedback from the checker

  # validate the feedback
  # DTK: This just throws errors if the necessary parts aren't there
  learnr:::feedback_validated(checker_feedback)

  # amend output with feedback as required
  feedback_html <-
    if (!is.null(checker_feedback)) {
      feedback_as_html(checker_feedback)
    } else {
      NULL
    }

  if (!is.null(feedback_html)) {
    # if no feedback, append invisible_feedback
    feedback_location <- checker_feedback$location %||% "append"
    if (feedback_location == "append") {
      html_output <- htmltools::tagList(html_output, feedback_html)
    } else if (feedback_location == "prepend") {
      html_output <- htmltools::tagList(feedback_html, html_output)
    } else if (feedback_location == "replace") {
      html_output <- feedback_html
    }
  }

  # return a list with the various results of the expression
  list(
    feedback = checker_feedback,
    error_message = NULL,
    timeout_exceeded = FALSE,
    html_output = html_output
  )
}

#' Display the results from an evaluated exercise in the chunk
#'
display_exercise_results <- function(exercise, envir,
                                     evaluate_global_setup = FALSE,
                                     err = NULL) {
  # create temp dir for execution (remove on exit)
  exercise_dir <- tempfile(pattern = "learnr-tutorial-exercise")
  dir.create(exercise_dir)
  oldwd <- setwd(exercise_dir)
  on.exit({
    setwd(oldwd)
    unlink(exercise_dir, recursive = TRUE)
  }, add = TRUE)

  # hack the pager function so that we can print help
  # http://stackoverflow.com/questions/24146843/including-r-help-in-knitr-output
  pager <- function(files, header, title, delete.file) {
    all.str <- do.call("c",lapply(files,readLines))
    cat(all.str,sep="\n")
  }
  orig_width <- options(width=70)
  on.exit(options(orig_width), add = TRUE)
  orig_pager <- options(pager=pager)
  on.exit(options(orig_pager), add = TRUE)

  # restore knitr options and hooks after knit
  optk <- knitr::opts_knit$get()
  on.exit(knitr::opts_knit$restore(optk), add = TRUE)
  optc <- knitr::opts_chunk$get()
  on.exit(knitr::opts_chunk$restore(optc), add = TRUE)
  hooks <- knitr::knit_hooks$get()
  on.exit(knitr::knit_hooks$restore(hooks), add = TRUE)
  ohooks <- knitr::opts_hooks$get()
  on.exit(knitr::opts_hooks$restore(ohooks), add = TRUE)
  templates <- knitr::opts_template$get()
  on.exit(knitr::opts_template$restore(templates), add = TRUE)

  # set preserved chunk options
  knitr::opts_chunk$set(as.list(exercise$options))

  # temporarily set knitr options (will be rest by on.exit handlers above)
  knitr::opts_chunk$set(echo = TRUE)
  knitr::opts_chunk$set(comment = NA)
  knitr::opts_chunk$set(error = FALSE)


  # write the R code to a temp file (include setup code if necessary)
  code <- c(exercise$setup, exercise$code)
  exercise_r <- "exercise.R"
  writeLines(code, con = exercise_r, useBytes = TRUE)

  # spin it to an Rmd
  exercise_rmd <- knitr::spin(hair = exercise_r,
                              knit = FALSE,
                              envir = envir,
                              format = "Rmd")


  # create html_fragment output format with forwarded knitr options
  knitr_options <- rmarkdown::knitr_options_html(
    fig_width = exercise$options$fig.width,
    fig_height = exercise$options$fig.height,
    fig_retina = exercise$options$fig.retina,
    keep_md = FALSE
  )

  # capture the last value and use a regular output handler for value
  # https://github.com/r-lib/evaluate/blob/e81ba2ba181827a86525767371e6dfdeb364c8b7/R/output.r#L54-L56
  # @param value Function to handle the values returned from evaluation. If it
  #   only has one argument, only visible values are handled; if it has more
  #   arguments, the second argument indicates whether the value is visible.
  last_value <- NULL
  last_value_is_visible <- TRUE

  evaluate_result <- NULL
  knitr_options$knit_hooks$evaluate = function(
    code, envir, ...,
    output_handler # knitr's output_handler
  ) {
    has_visible_arg <- length(formals(output_handler$value)) > 1

    # wrap `output_handler$value` to be able to capture the `last_value`
    # while maintaining the original functionality of `output_handler$value`
    output_handler_value_fn <- output_handler$value
    output_handler$value <- function(x, visible) {
      last_value <<- x
      last_value_is_visible <<- visible

      if (has_visible_arg) {
        output_handler_value_fn(x, visible)
      } else {
        output_handler_value_fun(withVisible(x)[[1]])
      }
    }

    evaluate_result <<- evaluate::evaluate(
      code, envir, ...,
      output_handler = output_handler
    )

    evaluate_result
  }
  output_format <- rmarkdown::output_format(
    knitr = knitr_options,
    pandoc = NULL,
    base_format = rmarkdown::html_fragment(
      df_print = exercise$options$exercise.df_print,
      pandoc_args = c("--metadata", "title=PREVIEW")
    )
  )

  err <- NULL
  # knit the Rmd to markdown (catch and report errors)
  tryCatch({
    output_file <- rmarkdown::render(input = exercise_rmd,
                                     output_format = output_format,
                                     envir = envir,
                                     clean = FALSE,
                                     quiet = TRUE,
                                     run_pandoc = FALSE)
  }, error = function(e) {
    # make the time limit error message a bit more friendly
    err <<- e$message
    pattern <- gettext("reached elapsed time limit", domain="R")
    if (regexpr(pattern, err) != -1L) {
      err <<- timeout_error_message()
    }
  })
  if (!is.null(err)) {
    return(error_result(err))
  }

  # capture and filter dependencies
  dependencies <- attr(output_file, "knit_meta")
  dependencies <- learnr:::filter_dependencies(dependencies)

  # render the markdown
  output_file <- rmarkdown::render(input = output_file,
                                   output_format = output_format,
                                   envir = envir,
                                   quiet = TRUE,
                                   clean = FALSE)
  output <- readLines(output_file, warn = FALSE, encoding = "UTF-8")
  output <- paste(output, collapse = "\n")

  # capture output as HTML w/ dependencies
  html_output <- htmltools::attachDependencies(
    htmltools::HTML(output),
    dependencies
  )
}

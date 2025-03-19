#' Print shadow_vimp results
#'
#' Custom print function to display the key elements of the `shadow_vimp()`
#' results.
#'
#' @param x Object of class 'shadow_vimp'
#' @param ... Further arguments passed to or from other methods.
#' @seealso \code{\link{shadow_vimp}}
#' @export
print.shadow_vimp <- function(x, ...) {
  cat("shadowVIMP Result\n\n")

  # removing extra white spaces from the call
  call_str <- paste(deparse(x$call), collapse = " ")
  call_str <- gsub("\\s+", " ", call_str)
  cat("Call:\n", call_str, "\n\n")

  cat("Alpha:  ", x$alpha, "\n")
  cat("Are the results from one of the pre-selection steps?:  ", x$result_taken_from_previous_step, "\n")

  # Depending on the selected method, display the results
  if (!is.null(x$final_dec_pooled)) {
    cat("Final Decision (pooled):\n")
    print(x$final_dec_pooled, ...)
  } else if (!is.null(x$final_dec_per_variable)) {
    cat("Final Decision (per variable):\n")
    print(x$final_dec_per_variable, ...)
  }
}

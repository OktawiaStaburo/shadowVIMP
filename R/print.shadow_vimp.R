#' Print shadow_vimp results
#'
#' Custom print function to display the key elements of the `shadow_vimp()`
#' results.
#'
#' @param x Object of class 'shadow_vimp'
#' @param ... Further arguments passed to or from other methods.
#' @seealso \code{\link{shadow_vimp}}
#' @import stringr
#' @importFrom magrittr %>%
#' @import dplyr
#' @export
print.shadow_vimp <- function(x, ...) {
  cat("shadowVIMP Result\n\n")

  # removing extra white spaces from the call
  call_str <- paste(deparse(x$call), collapse = " ")
  call_str <- gsub("\\s+", " ", call_str)
  cat("Call:\n", call_str, "\n\n")

  retained_vars <- data.frame(
    Step = character(),
    Alpha = numeric(),
    Retained_Covariates = numeric()
  )

  pre_sel_num <- length(x$pre_selection)
  # Capture results of pre-selection
  if (pre_sel_num > 0) {
    for (i in 1:pre_sel_num) {
      step_name <- paste0("step_", i)
      idx_dec <- grep("decision", names(x$pre_selection[[step_name]]))

      number <- x$pre_selection[[step_name]][[idx_dec]] %>%
        summarise(retained = sum(.data[["Type1_confirmed"]], na.rm = T)) %>%
        pull(.data[["retained"]])
      new_row <- data.frame(
        Step = step_name,
        Alpha = x$alpha[[i]],
        Retained_Covariates = number
      )

      retained_vars <- rbind(retained_vars, new_row)
    }
  }

  retained_vars <- retained_vars %>%
    mutate(Step = .data[["Step"]] %>% str_replace("_", " ") %>% str_to_title())

  idx_final_dec <- grep("final_dec", names(x))
  # If some covariates survived the pre-selection:
  if (x$step_all_covariates_removed == 0) {
    # Number of covariates that survived last step of the procedure
    retained_vars <- rbind(
      retained_vars,
      data.frame(
        Step = paste("Step", length(x$alpha)),
        Alpha = last(x$alpha),
        Retained_Covariates = x[[idx_final_dec]] %>%
          # select(Type1_confirmed) %>%
          summarise(retained = sum(.data[["Type1_confirmed"]], na.rm = T)) %>%
          pull(.data[["retained"]])
      )
    )
    colnames(retained_vars) <- c("Step", "Alpha", "Retained Covariates")
    print(retained_vars, row.names = FALSE, ...)
    cat("\n")
  } else {
    # None of the covariates survived the pre-selection step
    # so 0 covariates were Type 1 - confirmed in the last step
    retained_vars <- rbind(
      retained_vars,
      data.frame(
        Step = paste("Step", length(x$alpha)),
        Alpha = last(x$alpha),
        Retained_Covariates = 0
      )
    )
    colnames(retained_vars) <- c("Step", "Alpha", "Retained Covariates")
    print(retained_vars, row.names = FALSE, ...)
    cat("\n")
  }

  # Is it pooled or per_variable approach?
  method <- str_extract(names(x), "(?<=final_dec_)[A-Za-z0-9_]+") %>%
    str_replace_all("_", " ")
  method <- method[!is.na(method)]

  # Idx of the last step:
  res_from_step <- length(x$alpha)
  # Summarise results from the alst step:
  res <- x[[idx_final_dec]] %>%
    summarise(across(ends_with("confirmed"), ~ sum(., na.rm = T)))

  column_names <- data.frame(old_name = x[[idx_final_dec]] %>%
    select(ends_with("confirmed")) %>%
    colnames()) %>%
    mutate(new_name = case_when(
      old_name == "Type1_confirmed" ~ "Type-1 Confirmed",
      old_name == "FDR_confirmed" ~ "FDR Confirmed",
      old_name == "FWER_confirmed" ~ "FWER Confirmed"
    )) %>%
    pull(.data[["new_name"]])
  colnames(res) <- column_names

  cat(paste("Count of significant covariates from step", res_from_step, "per p-value correction method using the", method, "approach\n"))
  print(res, row.names = FALSE, ...)
}

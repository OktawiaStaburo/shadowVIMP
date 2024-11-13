#' Preselect the most promising covariates and discover the significant ones in
#' the final step
#'
#' `vim_perm_sim_wrapper()` does the preselection of variables first. This
#' allows non-informative covariates to be excluded from subsequent steps, thus
#' reducing runtime. Then, in the final step with the lowest alpha level, the
#' significant variables are confirmed in the same way as in the pre-selection
#' phase.
#'
#' @param alphas Numeric vector, significance level values for each step of the
#'   procedure, default `c(0.3, 0.10, 0.05)`.
#' @param nsims  Numeric vector, number of permutations to be performed in each
#'   step of the procedure, default `c(30, 120, 1500)`.
#' @inheritParams vim_perm_sim
#' @inheritParams add_test_results
#' @param save_vimp_history Character, one of `"all"`, `"last"` or `"none"`.
#'  * `"all"` (the default) - variable importances from the simulation of all
#'   steps are saved.
#'  * `"last"` - only the variable importances from the simulation of the last
#'   step will be saved.
#'  * `"none"` - no variable importances are saved from the simulation results.
#' @param method Character, one of `"pooled"` or `"per_variable"`.
#'  * `"pooled"` (the default) - the output shows pooled p-values and decisions
#'   based on them.
#'  * `"per_variable"` - the output shows per variable p-values and decisions
#'   based on them.
#' @return List with the following entries:
#'  * `vimp_history`- if `save_vimp_history` is set to `"all"` or `"last"` then
#'   it is a data frame with variable importance of covariates and their shadows
#'   from the last step. If `save_vimp_history` is set to `"none"`, then it is
#'   `NULL`.
#'  * `final_dec_pooled` (the default) or `final_dec_per_variable` - a data
#'   frame containing, depending on the specified value of the `to_show`
#'   parameter, p-values and corresponding decisions regarding variable
#'   informativeness from the last step of the procedure.
#'  * `alpha` - numeric, significance level used in last step.
#'  * `result_taken_from_previous_step` - a boolean indicating whether the
#'   reported results are actually the results obtained in the last step. If
#'   `TRUE`, then no variables survived the preselection process, so the
#'   reported results are taken from one of the previous steps.
#'  * `time_elapsed` - list containing the runtime of each step and the total
#'   time taken to execute the code.
#'  * `pre_selection` -  list in which the results of the pre-selection are
#'   stored. The exact form of this element depends on the chosen value of the
#'   `save_vimp_history` parameter.
#'  * `call` - the call formula used to generate the output.
#' @export
#' @import dplyr
#' @importFrom magrittr %>%
#' @examples
#' data(mtcars)
#'
#' # When working with real data, use higher values for the nsims and num.trees
#' # parameters --> here these parameters are set to small values to reduce the
#' # runtime.
#'
#' # Standard use - sequential computing
#' out1 <- vim_perm_sim_wrapper(
#'   entire_data = mtcars, outcome_var = "vs",
#'   nsims = c(10, 20, 30), num.trees = 30
#' )
#'
#' # Parallelisation provided by ranger::ranger() function --> increase the
#' # value of the num.threads parameter to speed up the computation
#' \donttest{
#' out2 <- vim_perm_sim_wrapper(
#'   entire_data = mtcars, outcome_var = "vs",
#'   nsims = c(10, 20, 30), num.threads = 2, num.trees = 30)
#'
#' # Parallel computing using a cluster
#' out3 <- vim_perm_sim_wrapper(
#'   entire_data = mtcars, outcome_var = "vs",
#'   nsims = c(10, 20, 30), num_cores_parallel = 2, num.trees = 30)
#'
#' # Save the simulated variable importance values for the last step only
#' out4 <- vim_perm_sim_wrapper(
#'   entire_data = mtcars, outcome_var = "vs",
#'   nsims = c(10, 20, 30), save_vimp_history = "last", num.trees = 30
#' )
#'
#' # Print unadjusted and FDR-adjusted p-values together with the corresponding
#' # decisions
#' out5 <- vim_perm_sim_wrapper(
#'   entire_data = mtcars, outcome_var = "vs",
#'   nsims = c(10, 20, 30), to_show = "FDR", num.trees = 30
#' )
#'
#' # Use per variable p-values to decide in the final step whether a covariate
#' # is informative or not
#' out6 <- vim_perm_sim_wrapper(
#'   entire_data = mtcars, outcome_var = "vs",
#'   nsims = c(10, 20, 30), method = "per_variable", num.trees = 30
#' )}
vim_perm_sim_wrapper <- function(alphas = c(0.3, 0.10, 0.05),
                                 nsims = c(30, 120, 1500),
                                 entire_data,
                                 outcome_var, # y,
                                 num.threads = NULL,
                                 num_cores_parallel = NULL,
                                 save_vimp_history = c("all", "last", "none"),
                                 to_show = c("FWER", "FDR", "unadjusted"),
                                 method = c("pooled", "per_variable"),
                                 ...) {
  cl <- match.call()
  cl[[1]] <- as.name("vim_perm_sim_wrapper")

  # Check if there is the same number of alpha and nsim parameters
  if (length(alphas) != length(nsims)) {
    stop("alphas and nsims must have the same length!")
  }
  if (is.unsorted(rev(alphas)) | any(alphas <= 0 | any(alphas >= 1))) {
    stop("Alphas must be in descending order. All alphas must be greater than 0 and less than 1.")
  }

  # Ensure save_vimp_history, to_show and method are one of the allowed values
  save_vimp_history <- match.arg(save_vimp_history)
  method <- match.arg(method)
  to_show <- match.arg(to_show)

  # Capture the name of the entire_data object
  data_name <- deparse(substitute(entire_data))

  # Capture the number of covariates (needed for correction of p-values)
  init_num_vars <- ncol(entire_data) - 1

  # for loop over alphas
  replicate <- NULL

  for (j in 1:length(alphas)) {
    # runtime start
    start_time <- Sys.time()

    cat("alpha ", alphas[j], " \n")

    if (j > 1 && length(replicate[[j - 1]]$variables_remaining_for_replicate_pooled) == 0) {
      result_from_previous_step_bool <- TRUE

      warning("None of the variables have passed the pre-selection process to the final step.\n The results presented do not correspond to the smallest alpha specified.")
    } else {
      # run algorithm
      vimpermsim <- vim_perm_sim(
        entire_data = if (j > 1) {
          entire_data %>%
            select(all_of(c(replicate[[j - 1]]$variables_remaining_for_replicate_pooled, outcome_var)))
        } else {
          entire_data
        }, # pooled pre selection
        outcome_var = outcome_var, # y
        nsim = nsims[j],
        importance = "permutation",
        num.threads = num.threads,
        data_name = data_name,
        num_cores_parallel = num_cores_parallel,
        ...
      )

      result_from_previous_step_bool <- FALSE
    }

    # If there are some variables remaining after the pre-selection
    if (result_from_previous_step_bool == FALSE) {
      vimpermsim <- add_test_results(vimpermsim,
        alpha = alphas[j],
        init_num_vars = init_num_vars,
        to_show = to_show
      )

      variables_remaining_for_replicate_pooled <- vimpermsim$test_results$pooled %>%
        filter(.data[["quantile_pooled"]] >= 1 - alphas[j]) %>%
        select("varname") %>%
        unlist() %>%
        unname()

      cat("Variables remaining: ", length(variables_remaining_for_replicate_pooled), "\n")

      # runtime end
      end_time <- Sys.time()

      replicate[[j]] <- list(
        "alpha" = alphas[j],
        "result_taken_from_previous_step" = result_from_previous_step_bool,
        "vimpermsim" = vimpermsim,
        "variables_remaining_for_replicate_pooled" = variables_remaining_for_replicate_pooled,
        "time_elapsed" = difftime(end_time, start_time, units = "mins")
      )
    } else {
      # no variables survived the pre-selection process --> take results from previous step
      end_time <- Sys.time()
      last_res <- replicate[[j - 1]]
      last_res$result_taken_from_previous_step <- result_from_previous_step_bool
      last_res$time_elapsed <- difftime(end_time, start_time, units = "mins")
      last_res$warning <- "No important variables remained until the last step, results taken from previous step."
      replicate[[j]] <- last_res
    }
  }

  # time elapsed
  time <- list()
  for (i in 1:length(replicate)) {
    step_name <- paste0("step_", i)
    time[[step_name]] <- replicate[[i]]$time_elapsed %>%
      as.numeric()
  }

  time[["total_time_mins"]] <- time %>%
    unlist() %>%
    sum()

  # Clean vimp history if "none" or "last" option is selected
  if (save_vimp_history == "none") {
    for (i in 1:length(replicate)) {
      replicate[[i]]$vimpermsim$vim_simulated <- NULL
    }
  } else if (save_vimp_history == "last") {
    for (i in 1:(length(replicate) - 1)) {
      replicate[[i]]$vimpermsim$vim_simulated <- NULL
    }
  }

  # If pre-selection has been done - save its results
  if(length(alphas) > 1){
    # Create pre_selection list storing results of the pre-selection steps
    pre_selection <- list()

    for (i in 1:(length(alphas) - 1)) {
      step_name <- paste0("step_", i)
      pre_selection[[step_name]][["vimp_history"]] <- replicate[[i]]$vimpermsim$vim_simulated

      if (method == "pooled") {
        pre_selection[[step_name]][["decision_pooled"]] <- replicate[[i]]$vimpermsim$test_results$pooled
      } else {
        pre_selection[[step_name]][["decision_per_variable"]] <- replicate[[i]]$vimpermsim$test_results$per_variable
      }

      pre_selection[[step_name]][["alpha"]] <- replicate[[i]]$alpha
      pre_selection[[step_name]][["result_taken_from_previous_step"]] <- replicate[[i]]$vimpermsim$result_taken_from_previous_step
    }
  }

  # Create list storing results of the final step
  last_idx <- length(alphas)

  output <- list(
    "vimp_history" = replicate[[last_idx]]$vimpermsim$vim_simulated,
    "alpha" = replicate[[last_idx]]$alpha,
    "result_taken_from_previous_step" = replicate[[last_idx]]$result_taken_from_previous_step,
    "time_elapsed" = time,
    "pre_selection" = pre_selection,
    "call" = cl
  )

  if (method == "pooled") {
    final_dec <- replicate[[last_idx]]$vimpermsim$test_results$pooled
  } else {
    final_dec <- replicate[[last_idx]]$vimpermsim$test_results$per_variable
  }

  sublist_name <- paste0("final_dec_", method)
  output[[sublist_name]] <- final_dec

  return(output)
}

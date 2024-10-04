#' Wrapper function that allows pre-selection of variables and selection of
#' final set of covariates
#'
#'
#' @param alphas  numeric (vector), significance level values for each step of
#'   the procedure, default is `c(0.3, 0.10, 0.05)`
#' @param nsims  numeric (vector), number of permutations to be performed in
#'   each step of the procedure, default is `c(30, 120, 1500)`
#' @param entire_data data frame
#' @param outcome_var character, name of the column containing the outcome
#'   variable
#' @param write.forest logical, indicator whether to save `ranger.forest`
#'   object, default is `FALSE`
#' @param num.threads numeric, number of threads, default is number of CPUs
#'   available
#' @param num_cores_parallel numeric greater than 0 and and less than or equal to the
#'   number of cores available on your computer (`detectCores()`). This
#'   parameter specifies the number of cores to use to create a cluster by
#'   calling `parallel::makeCluster(num_cores_parallel)`. The default is `NULL`, which
#'   means that sequential computation is used.
#' @param save_vimp_history character, one of "all", "last" or "none". If "all"
#'   is selected, the variable importance from simulation of all steps,
#'   including the preselection, will be saved.If "last" is selected, only the
#'   variable importance from the simulation of the last step is saved. If
#'   "none" is selected, no variable importance is saved from the simulation
#'   results. Default is "all".
#' @param to_show character, one of "FWER", "FDR" or "unadjusted". If
#'   "unadjusted" is selected, then the final output will contain only raw,
#'   unadjusted p-values together with the decision. If "FDR" is selected, the
#'   final output will include both unadjusted and Benjamini-Hochberg adjusted
#'   p-values along with the decision. The last option "FWER" includes
#'   unadjusted, Benjamini-Hochberg and Holm adjusted p-values together with the
#'   decision. Default is "FWER".
#' @param ... additional parameters passed directly to `ranger::ranger()`
#'
#' @return list with the following entries:
#' \enumerate{
#'  \item `final_dec_per_variable` - table of decisions on covariate importance
#'   when using unadjusted, FDR and FWER adjusted per_variable p-values,
#'  \item `final_dec_pooled` - table of decisions on covariate importance
#'   when using unadjusted, FDR and FWER adjusted pooled p-values,
#'  \item `vimp_history` variable importance of covariates and their shadows
#'  obtained in the last step of the procedure for each of the `nsim`
#'  permutations.
#'  \item `alpha` significance level used in the last step,
#'  \item `control_parameters` - list of control parameters used,
#'  \item `result_taken_from_previous_step` a logical parameter that indicates
#'  whether the reported results were taken from the previous step. If `TRUE`
#'  then no variables were significantly important in the last step, so nothing
#'  is left for the next step
#'  \item `time_elapsed_last_step` numeric, run time of the last step of algorithm,
#'  \item `pre_selection` list, stores the results of all steps before the last
#'  one,
#'  \item `call` stores the call formula used to generate the output.
#' }
#' @export
#' @import magrittr dplyr
#'
#' @examples
vim_perm_sim_wrapper <- function(alphas = c(0.3, 0.10, 0.05),
                                 nsims = c(30, 120, 1500),
                                 entire_data,
                                 outcome_var, #y,
                                 write.forest = F,
                                 num.threads = NULL,
                                 num_cores_parallel = NULL,
                                 save_vimp_history = c("all", "last", "none"),
                                 to_show = c("FWER", "FDR", "unadjusted"),
                                 method = c("pooled", "per_variable"),
                                 ...
) {

  cl <- match.call()
  cl[[1]] <- as.name('vim_perm_sim_wrapper')

  # Check if there is the same number of alpha and nsim parameters
  if(length(alphas) != length(nsims)){
    stop("alphas and nsims must have the same length!")
  }
  if(is.unsorted(rev(alphas)) | any(alphas <= 0 | any(alphas >= 1))) {
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

  #for loop over alphas
  replicate <- NULL

  for(j in 1:length(alphas)) {

    #runtime start
    start_time <- Sys.time()

    cat("alpha ", alphas[j], " \n")

    if(j>1 && length(replicate[[j-1]]$variables_remaining_for_replicate_pooled)==0) {

      result_from_previous_step_bool = TRUE

      warning("None of the variables have passed the pre-selection process to the final step.\n The results presented do not correspond to the smallest alpha specified.")

    } else{

      #run algorithm
      vimpermsim <-  vim_perm_sim(entire_data = entire_data %>% { if(j>1)
        select(., all_of(c(replicate[[j-1]]$variables_remaining_for_replicate_pooled, outcome_var))) else .},  #pooled pre selection
        outcome_var = outcome_var, # y
        nsim = nsims[j],
        importance = "permutation",
        write.forest = write.forest,
        num.threads = num.threads,
        data_name = data_name,
        num_cores_parallel = num_cores_parallel,
        ...)

      result_from_previous_step_bool = FALSE

    }

    # If there are some variables remaining after the pre-selection
    if(result_from_previous_step_bool == FALSE){

      vimpermsim <- add_test_results(vimpermsim,
                                     alpha = alphas[j],
                                     init_num_vars = init_num_vars,
                                     to_show = to_show)

      variables_remaining_for_replicate_pooled <- vimpermsim$test_results$pooled %>%
        dplyr::filter(quantile_pooled >= 1-alphas[j]) %>%
        select(varname) %>%
        unlist() %>%
        unname()

      cat("Variables remaining: ", length(variables_remaining_for_replicate_pooled), "\n")

      #runtime end
      end_time <- Sys.time()

      replicate[[j]] <- list("alpha" = alphas[j],
                             "result_taken_from_previous_step" = result_from_previous_step_bool,
                             "vimpermsim" = vimpermsim,
                             "variables_remaining_for_replicate_pooled" = variables_remaining_for_replicate_pooled,
                             "time_elapsed" = difftime(end_time, start_time, units = "mins")
      )
    } else{
      #no variables survived the pre-selection process --> take results from previous step
      end_time <- Sys.time()
      last_res <- replicate[[j-1]]
      last_res$result_taken_from_previous_step <- result_from_previous_step_bool
      last_res$time_elapsed <- difftime(end_time, start_time, units = "mins")
      last_res$warning <- "No important variables remained until the last step, results taken from previous step."
      replicate[[j]] <- last_res
    }

  }

  # time elapsed
  time <- list()
  for(i in 1:length(replicate)){
    step_name <- paste0("step_", i)
    time[[step_name]] <- replicate[[i]]$time_elapsed %>% as.numeric()
  }

  time[["total_time_mins"]] <- time %>% unlist() %>% sum()

  # Clean vimp history if "none" or "last" option is selected
  if(save_vimp_history == "none"){
    for(i in 1:length(replicate)){
      replicate[[i]]$vimpermsim$vim_simulated <- NULL
    }
  } else if(save_vimp_history == "last"){
    for(i in 1:(length(replicate) - 1)){
      replicate[[i]]$vimpermsim$vim_simulated <- NULL
    }
  }

  # Create pre_selection list storing results of the pre-selection steps
  pre_selection <- list()

  for(i in 1:(length(alphas)-1)){
    step_name <- paste0("step_", i)
    pre_selection[[step_name]][["vimp_history"]] <- replicate[[i]]$vimpermsim$vim_simulated

    if(method == "pooled"){
      pre_selection[[step_name]][["decision_pooled"]] <- replicate[[i]]$vimpermsim$test_results$pooled
    } else{
      pre_selection[[step_name]][["decision_per_variable"]] <- replicate[[i]]$vimpermsim$test_results$per_variable
    }

    pre_selection[[step_name]][["alpha"]] <- replicate[[i]]$alpha
    pre_selection[[step_name]][["control_parameters"]] <- replicate[[i]]$vimpermsim$controls
    pre_selection[[step_name]][["result_taken_from_previous_step"]] <- replicate[[i]]$vimpermsim$result_taken_from_previous_step
  }

  # Create list storing results of the final step
  last_idx = length(alphas)

  output <- list(
    "vimp_history" = replicate[[last_idx]]$vimpermsim$vim_simulated,
    "alpha" = replicate[[last_idx]]$alpha,
    "control_parameters" = replicate[[last_idx]]$vimpermsim$controls,
    "result_taken_from_previous_step" = replicate[[last_idx]]$result_taken_from_previous_step,
    "time_elapsed" = time,
    "pre_selection" = pre_selection,
    "call" = cl
  )

  if(method == "pooled"){
    final_dec <- replicate[[last_idx]]$vimpermsim$test_results$pooled
  } else{
    final_dec <- replicate[[last_idx]]$vimpermsim$test_results$per_variable
  }

  sublist_name <- paste0("final_dec_", method)
  output[[sublist_name]] <- final_dec

  return(output)

}

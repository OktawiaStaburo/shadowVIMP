# vim_perm_sim() for survival data
# TODO: update documentation
#' Compute the variable importance of the predictors and their row-wise shadows
#'
#' `vim_perm_sim()` calculates repeatedly (`niters` times) the variable
#' importance of the original values of the predictors and their row-wise
#' permuted shadows. Each shadow's variable importance is computed based on a
#' new permutation of the initial predictor values.
#'
#' @param data Input data frame.

#' @param niters Numeric, number of permutations of the initial predictor
#'   values, default is 100.
#' @param importance Character, the type of variable importance to be calculated
#'   for each independent variable. Argument passed to [ranger::ranger()],
#'   default is `permutation`.
#'
#' @param num.trees Numeric, number of trees. Passed to [ranger::ranger()],
#'   default is `max(2 * (ncol(data) - 1), 10000)`.
#' @param data_name Character, name of the object passed as `data`. In
#'   `shadow_vimp()` it is set automatically.
#' @param ... Additional parameters passed to [ranger::ranger()].
#' @return List containing `niters` variable importance values for both the
#'   original and row-wise permuted predictors.
#' @noRd
#' @import rlang dplyr
#' @importFrom magrittr %>%
#' @importFrom stats runif
#' @importFrom randomForestSRC rfsrc
#' @importFrom purrr map
#' @examples
#' data(mtcars)
#' # When working with real data, increase num.trees value or keep the default
#' # Here this parameter is set to a small value in order to reduce the runtime
#'
#' # Function to make sure proper number of cores is specified for multithreading
#' safe_num_threads <- function(n) {
#'   available <- parallel::detectCores()
#'   if (n > available) available else n
#' }
#'
#' #' # Standard use:
#' out_seq <- vim_perm_sim(
#'   data = mtcars, outcome_var = "vs", niters = 30,
#'   num.trees = 50, num.threads = safe_num_threads(1)
#' )
#'
# TODO: update documentation and examples according to the changes you implemented
# notes about what should be included in the documentation
# 1. explanation of handling NAs
# 2. Info that input data must be only real valued, integer, factor or logical - NO characters allowed
vim_perm_sim_survival <- function(data,
                         time_column,
                         status_column,
                         niters = 100,
                         importance = "permute",
                         num.trees = max(2 * (ncol(data) - 1), 10000),
                         data_name = NULL,
                         na.action = c("na.omit", "na.impute"),
                         ...) {

  # Check if niters parameter has a correct format
  if (is.numeric(niters) == F) {
    stop("`niters` parameter must be numeric.")
  }

  # Capture the name of the object passed as data - needed for progress tracking
  if (is.null(data_name)) {
    data_name <- deparse(substitute(data))
  }

  # Check if passed data_name argument has a correct format
  if (length(data_name) > 1) {
    stop("`data_name` must be a character of length 1.")
  }

  if(!is.character(time_column) || !is.character(status_column) || length(time_column) != 1 || length(status_column) != 1){
    stop("`time_column` and  `status_column` must be characters of length 1.")
  }

  p <- ncol(data) - 2
  n <- nrow(data)

  # Splitting predictors
  predictors <- data %>% select(-all_of(c(status_column, time_column)))

  if (sum(grepl("_permuted$", names(predictors))) > 0) {
    stop("One or more variables ending with _permuted. Please rename them.")
  }

  # Creating permuted predictors
  predictors_p <- predictors[sample(1:n), , drop = FALSE]

  names(predictors_p) <- paste0(names(predictors_p), "_permuted")

  # Concatenating predictors, permuted predictors and label
  dt <- cbind.data.frame(
    predictors,
    predictors_p,
    data %>% select(all_of(c(status_column, time_column)))
    )

  # Prepare formula for RF
  # turn characters into a symbols
  time_sym <- ensym(time_column)
  status_sym <- ensym(status_column)

  formula_rf <- as.formula(
    expr( Surv(!!time_sym, !!status_sym) ~ . )
  )

  # rfsrc() has no analogue of respect.unordered.factors = "order" from ranger
  # Therefore I do the ordering of factors by hand in the same way as in ranger
  # --> for survival data factors are ordered by the median survival time
  to_convert <- dt %>%
    select(-all_of(c(status_column, time_column))) %>%
    select(where(is.factor)) %>%
    names()

  for (var in to_convert) {
    var_sym <- sym(var)

    order <- dt %>%
      group_by(!!var_sym) %>%
      summarise(median_time = median(!!time_sym, na.rm = TRUE),
                .groups = "drop") %>%
      arrange(median_time) %>%
      pull(!!var_sym)

    dt <- dt %>%
      mutate(!!var_sym := factor(
        !!var_sym,
        levels  = order,
        ordered = TRUE))
  }

  # Simulation
  vimp_sim <- NULL

  for (i in 1:niters) {
    if ((i %% 50 == 0) | (i == 1)) {

      message(paste0(format(Sys.time()), ": dataframe = ", data_name, " niters = ",
                     niters, " num.trees = ", num.trees, ". Running step ", i, "\n"))
    }

    # reshuffle row wise
    dt[, (ncol(predictors_p) + 1):(2 * ncol(predictors_p))] <- dt[sample(1:n), (ncol(predictors_p) + 1):(2 * ncol(predictors_p))]

    vimp_sim[[i]] <- rfsrc(
      formula = formula_rf,
      data = dt,
      ntree = num.trees,
      splitrule = "logrankCR",
      importance = importance,
      na.action = na.action, # How to handle NAs
      samptype = "swr", # Sample with replacement
      save.memory= TRUE, # Set to save memory and speed up computation
      ...
    )$importance %>%
      as.data.frame()
  }

  # Get the competing events names
  event_names <- colnames(vimp_sim[[1]])

  # Collect VIMPs separately for each of competing events
  results_sim <- list()
  for(event in event_names){
    sim_event <-  map(vimp_sim, ~.x %>%
                        select(all_of(event)) %>%
                        t() %>%
                        data.frame(row.names = NULL)
                      ) %>%
      bind_rows()
    results_sim[[event]] <- sim_event
  }

  # Check if for any event any covariate has always VIMP = 0
  for(event in event_names){
    sd_shadow <- results_sim[[event]] %>%
      select(ends_with("_permuted")) %>%
      summarise(across(everything(), ~ sd(.x))) %>%
      pivot_longer(cols = everything(), names_to = "variable", values_to = "sd") %>%
      filter(sd == 0)

    if (nrow(sd_shadow) > 0) {
      warning("One or more shadow variables always have VIMP equal to zero.")
    }
  }

  return(results_sim)

}


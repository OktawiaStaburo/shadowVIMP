# vim_perm_sim() for survival data
# Once the code for survival data is fully developed (both code- and theorywise) and tested
# you can modify the code of main shadow_vimp() function such that it will be able to
# handle regression/classification as well as survival cases
# Technical comment: if - else approach: add parameter specifying whether it' s survival case and  use appropaite function
#' Compute the variable importance of the predictors and their row-wise shadows
#'
#' `vim_perm_sim()` calculates repeatedly (`niters` times) the variable
#' importance of the original values of the predictors and their row-wise
#' permuted shadows. Each shadow's variable importance is computed based on a
#' new permutation of the initial predictor values.
#'
#' @param data Input data frame.
#' @param outcome_var Character, name of the column containing the outcome
#'   variable.
#' @param niters Numeric, number of permutations of the initial predictor
#'   values, default is 100.
#' @param importance Character, the type of variable importance to be calculated
#'   for each independent variable. Argument passed to [ranger::ranger()],
#'   default is `permutation`.
#' @param num.threads Numeric. The number of threads used by [ranger::ranger()]
#'   for parallel tree building. If `NULL` (the default), half of the available
#'   CPU threads are used (this is the default behavior in `shadow_vimp()`,
#'   which is different from the default in [ranger::ranger()]). See the
#'   [ranger::ranger()] documentation for more details.
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
#' # `num.threads` sets the number of threads for multithreading in
#' # `ranger::ranger`
#' out_par <- vim_perm_sim(
#'   data = mtcars, outcome_var = "vs", niters = 30,
#'   num.threads = safe_num_threads(2), num.trees = 50
#' )
vim_perm_sim_survival <- function(data,
                         #outcome_var,
                         time_column,
                         censoring_column,
                         niters = 100,
                         importance = "permutation",
                         num.threads = NULL,
                         num.trees = max(2 * (ncol(data) - 1), 10000),
                         data_name = NULL,
                         ...) {
  # Check whether num.threads is correctly specified
  max_cores <- parallel::detectCores()

  # By default set num.threads to half of the available cores
  if (is.null(num.threads)) {
    num.threads <- ifelse(max_cores == 1, 1, floor(max_cores / 2))
  }

  if (num.threads > max_cores || is.numeric(num.threads) == F) {
    stop("Specified value of `num.threads` is too big or it is not numeric. Use parallel::detectCores() to check the maximal possible value of `num.threads` parameter.")
  }

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

  if(!is.character(time_column) || !is.character(censoring_column) || length(time_column) != 1 || length(censoring_column) != 1){
    stop("`time_column` and  `censoring_column` must be characters of length 1.")
  }

  # Check whether the input data has already a column called "y"
  # is_y <- "y" %in% colnames(data)
  #
  # # If there is already a column "y" and it's not outcome_var, force user to rename it
  # if (is_y == TRUE && outcome_var != "y") {
  #   stop("Before running the function, rename the column 'y' in the input data.")
  # }

  # Renaming outcome variable to y
  # data <- data %>%
  #   rename(y = all_of(outcome_var))

  p <- ncol(data) - 2
  n <- nrow(data)


  # Splitting predictors
  predictors <- data %>% select(-all_of(c(censoring_column, time_column)))

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
    data %>% select(all_of(c(censoring_column, time_column)))
    )


  # Simulation
  vimp_sim <- NULL

  for (i in 1:niters) {
    if ((i %% 50 == 0) | (i == 1)) {

      message(paste0(format(Sys.time()), ": dataframe = ", data_name, " niters = ",
                     niters, " num.trees = ", num.trees, ". Running step ", i, "\n"))
    }

    # reshuffle row wise
    dt[, (ncol(predictors_p) + 1):(2 * ncol(predictors_p))] <- dt[sample(1:n), (ncol(predictors_p) + 1):(2 * ncol(predictors_p))]

    vimp_sim[[i]] <- (ranger::ranger(
      # y = dt$y,
      # x = dt %>% select(-"y"),
      data = dt,
      dependent.variable.name = time_column, #"time",
      status.variable.name = censoring_column, #"status",
      importance = importance,
      replace = TRUE,
      num.trees = num.trees,
      scale.permutation.importance = TRUE,
      num.threads = num.threads,
      write.forest = FALSE,
      respect.unordered.factors = "order",
      ...
    ))$variable.importance %>%
      t() %>%
      as.data.frame()
  }

  # putting all results in a df
  df_sim <- as.data.frame(do.call(rbind, vimp_sim))

  sd_shadow <- df_sim %>%
    select(ends_with("_permuted")) %>%
    summarise(across(everything(), ~ sd(.x))) %>%
    pivot_longer(cols = everything(), names_to = "variable", values_to = "sd") %>%
    filter(sd == 0)

  if (nrow(sd_shadow) > 0) {
    warning("One or more shadow variables always have VIMP equal to zero.")
  }

  res <- list(
    vim_simulated = df_sim
  )
}


#' Compute the variable importance of the predictors and their row-wise shadows
#'
#' `vim_perm_sim()` calculates repeatedly (`nsim` times) the variable importance
#' of the original values of the predictors and their row-wise permuted shadows.
#' Each shadow's variable importance is computed based on a new permutation of
#' the initial predictor values.
#'
#' @param entire_data Input data frame.
#' @param outcome_var Character, name of the column containing the outcome
#'   variable.
#' @param nsim Numeric, number of permutations of the initial predictor values,
#'   default is 100.
#' @param importance Character, the type of variable importance to be calculated
#'   for each independent variable. Argument passed to [ranger::ranger()],
#'   default is `permutation`.
#' @param num.threads Numeric, number of threads. Argument passed to
#'   [ranger::ranger()], default is `NULL`.
#' @param num.trees Numeric, number of trees. Passed to [ranger::ranger()],
#'   default is `max(2 * (ncol(entire_data) - 1), 10000)`.
#' @param data_name Character, name of the object passed as `entire_data`. In
#'   `vim_perm_sim_wrapper()` it is set automatically.
#' @param num_cores_parallel Numeric greater than 0 and and less than or equal
#'   to the number of cores available on your computer (check by running
#'   [parallel::detectCores()]). This parameter specifies the number of cores to
#'   use to create a cluster when calling
#'   `parallel::makeCluster(num_cores_parallel)`. For example, setting
#'   `num_cores_parallel` to 4 will use 4 cores to create a cluster. The default
#'   is `NULL`, which means that sequential computation is used.
#' @param ... Additional parameters passed to [ranger::ranger()].
#' @return List consisting of `vim_simulated` - a data frame with `n_sim`
#'   variable importances calculated based on the original and row-wise permuted
#'   values of the predictors.
#' @export
#' @import foreach doRNG rlang dplyr
#' @importFrom magrittr %>%
#' @importFrom stats runif
#' @importFrom doParallel registerDoParallel
#' @examples
#' data(mtcars)
#' # When working with real data, increase num.trees value or leave default
#' # Here this parameter is set to a small value in order to reduce the runtime
#'
#' # Sequential computing mode:
#' out_seq <- vim_perm_sim(
#'   entire_data = mtcars, outcome_var = "vs", nsim = 30,
#'   num.trees = 50
#' )
#'
#' # Parallel computing - using a cluster:
#' \donttest{
#' out_par_cores <- vim_perm_sim(
#'   entire_data = mtcars, outcome_var = "vs",
#'   nsim = 30, num_cores_parallel = 2, num.trees = 50
#' )
#' }
#'
#' # Parallelism through num.threads parameter from ranger::ranger()
#' out_par <- vim_perm_sim(
#'   entire_data = mtcars, outcome_var = "vs", nsim = 30,
#'   num.threads = 2, num.trees = 50
#' )
vim_perm_sim <- function(entire_data,
                         outcome_var, # y
                         nsim = 100,
                         importance = "permutation",
                         num.threads = NULL,
                         num.trees = max(2 * (ncol(entire_data) - 1), 10000),
                         data_name = NULL,
                         num_cores_parallel = NULL,
                         ...) {
  # Check whether num_cores_parallel is correctly specified
  max_cores <- parallel::detectCores()
  if (is.null(num_cores_parallel) == FALSE) {
    if (!is.numeric(num_cores_parallel) || num_cores_parallel < 0 || num_cores_parallel > max_cores) {
      stop("The specified number of cores `num_cores_parallel` is incorrect or it is not numeric.")
    }

    # Avoid oversubscription
    if (is.null(num.threads) == F && num.threads * num_cores_parallel > max_cores) {
      stop("The total number of threads (`num.threads`*`num_cores_parallel`) exceeds the number of available cores.\n Make sure that the product of `num.threads` and `num_cores_parallel` parameter values does not exceed parallel::detectCores().")
    }
  }

  # Avoid oversubscription in sequential mode
  if (is.null(num.threads) == F && (num.threads > max_cores || is.numeric(num.threads) == F)) {
    stop("Specified value of `num.threads` is too big or it is not numeric. Use parallel::detectCores() to check the maximal possible value of `num.threads` parameter.")
  }

  # Check if nsim parameter has a correct format
  if (is.numeric(nsim) == F) {
    stop("`nsim` parameter must be numeric.")
  }

  # Capture the name of the object passed as entire_data - needed for progress tracking
  if (is.null(data_name)) {
    data_name <- deparse(substitute(entire_data))
  }

  # Check if passed data_name argument has a correct format
  if (length(data_name) > 1) {
    stop("`data_name` must be a character of length 1.")
  }

  # Check whether the input data has already a column called "y"
  is_y <- "y" %in% colnames(entire_data)

  # If there is already a column "y" and it's not outcome_var, force user to rename it
  if (is_y == TRUE && outcome_var != "y") {
    stop("Before running the function, rename the column 'y' in the input data.")
  }

  # Renaming outcome variable to y
  entire_data <- entire_data %>%
    rename(y = all_of(outcome_var))

  p <- ncol(entire_data) - 1
  n <- nrow(entire_data)


  # Splitting predictors
  predictors <- entire_data %>% select(-"y")

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
    entire_data %>% select("y")
  )

  # Simulation
  if (is.null(num_cores_parallel) == TRUE) {
    # Sequential implementation
    vimp_sim <- NULL

    for (i in 1:nsim) {
      if ((i %% 50 == 0) | (i == 1)) {
        cat(paste0(format(Sys.time()), ": dataframe = ", data_name, " nsim = ", nsim, " num.trees = ", num.trees, ". Running step ", i, "\n"))
      }

      # reshuffle row wise
      dt[, (ncol(predictors_p) + 1):(2 * ncol(predictors_p))] <- dt[sample(1:n), (ncol(predictors_p) + 1):(2 * ncol(predictors_p))]

      vimp_sim[[i]] <- (ranger::ranger(
        y = dt$y,
        x = dt %>% select(-"y"),
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
  } else {
    # Parallel implementation with cluster specified by the number of cores
    # Register cluster
    cluster <- parallel::makeCluster(num_cores_parallel)
    doParallel::registerDoParallel(cluster)

    # Ensure the cluster is stopped when the function exits
    #on.exit(parallel::stopCluster(cluster))

    # # Progress track
    # progressr::handlers("txtprogressbar")  # Choose the text progress bar handler
    # p <- progressr::progressor(along = 1:nsim)

    # Control seed
    set.seed(1807)
    seed_list <- floor(runif(nsim, min = 1, max = 999999))

    # vimp_sim <-  progressr::with_progress({
    vimp_sim <-  foreach(
        i = 1:nsim,
        .packages = c("ranger", "dplyr")#,
        # .export = c("p", "seed_list")
    ) %dorng% {

      set.seed(seed_list[i])
      #p()

      # Reshuffle row-wise
      dt[, (ncol(predictors_p) + 1):(2 * ncol(predictors_p))] <- dt[sample(1:n), (ncol(predictors_p) + 1):(2 * ncol(predictors_p))]

      vimp <- (ranger::ranger(
        y = dt$y,
        x = dt %>% select(-"y"),
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

      vimp # Return the result of each iteration
    }#})

    parallel::stopCluster(cluster)
    # Come back to sequential computing
    foreach::registerDoSEQ()

  }

  # putting all results in a df
  df_sim <- as.data.frame(do.call(rbind, vimp_sim))

  # Storing result to be returned
  res <- list(
    vim_simulated = df_sim
  )
}

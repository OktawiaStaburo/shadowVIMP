#' Calculate variable importance of original values of predictors and its
#' row-wise permuted shadows
#'
#' Dependencies: dplyr, ranger, magrittr, parallel, foreach, doSNOW/doParallel, doRNG
#'
#' @param entire_data data frame
#' @param outcome_var character, name of the column containing the outcome
#'   variable
#' @param nsim numeric, number of permutations, by default 100
#' @param importance character, variable importance mode passed directly to
#'   `ranger::ranger()`, default is "permutation"
#' @param num.threads numeric, number of threads, default is number of CPUs
#'   available
#' @param write.forest logical, indicator whether to save `ranger.forest`
#'   object, default is `FALSE`
#' @param num.trees numeric, number of trees
#' @param data_name character, name of the object passed as `entire_data`. In
#'   `vim_perm_sim_wrapper()` set automatically
#' @param num_cores_parallel numeric greater than 0 and and less than or equal to the
#'   number of cores available on your computer (`detectCores()`). This
#'   parameter specifies the number of cores to use to create a cluster by
#'   calling `parallel::makeCluster(num_cores_parallel)`. The default is `NULL`, which
#'   means that sequential computation is used.
#' @param ... additional parameters passed directly to `ranger::ranger()` or
#'   `party::cforest()`
#'
#' @return List where:
#' \enumerate{
#'  \item `vim_simulated` stores `n_sim` variable importances calculated based
#'  on the original and row-wise permuted values of the predictors,
#'  \item `controls` stores used control parameters, by default - number of
#'  permutations `n_sim`.
#' }
#' @export
#'
#' @import dplyr ranger magrittr parallel foreach doSNOW doRNG utils
#' @importFrom rlang .data
#' @examples
vim_perm_sim <- function(entire_data,
                         outcome_var, #y
                         nsim = 100,
                         importance = "permutation",
                         num.threads = NULL,
                         write.forest = F,
                         num.trees = max(2*(ncol(entire_data)-1), 10000),
                         data_name = NULL,
                         num_cores_parallel = NULL,
                         ...) {

  # Check whether num_cores_parallel is correctly specified
  max_cores <- parallel::detectCores()
  if(is.null(num_cores_parallel) == FALSE){

    num_cores_parallel <- round(num_cores_parallel)

    if(!is.numeric(num_cores_parallel) || num_cores_parallel < 0 || num_cores_parallel > max_cores){
      stop("The specified number of cores `num_cores_parallel` is incorrect.")
    }

    # Avoid oversubscription
    if(num.threads*num_cores_parallel > max_cores){
      stop("The total number of threads (`num.threads`*`num_cores_parallel`) exceeds the number of available cores.\n Make sure that the product of `num.threads` and `num_cores_parallel` parameter values does not exceed parallel::detectCores().")
    }
  }

  # Avoid oversubscription in sequential mode
  if(num.threads > max_cores){
    stop("Specified value of `num.threads` is too big. Use parallel::detectCores() to check the maximal possible value of `num.threads` parameter.")
  }

  # Capture the name of the object passed as entire_data - needed for progress tracking
  if (is.null(data_name)) {
    data_name <- deparse(substitute(entire_data))
  }

  # Check whether the input data has already a column called "y"
  is_y <- "y" %in% colnames(entire_data)

  # If there is already a column "y" and it's not outcome_var, force user to rename it
  if(is_y == TRUE && outcome_var != "y"){
    stop("Before running the function, rename the column 'y' in the input data.")
  }

  # Renaming outcome variable to y
  entire_data <- entire_data %>%
    dplyr::rename(y = .data[[outcome_var]])

  p <- ncol(entire_data)-1
  n <- nrow(entire_data)


  #Splitting predictors
  predictors <- entire_data %>% select(- c(y))

  if(length(grep('$_permuted',names(predictors)))>0){
    stop('One or more variables ending with _permuted. Please rename them.')
  }

  #Creating permuted predictors
  predictors_p <- predictors[sample(1:n),,drop=FALSE]

  names(predictors_p)<-paste0(names(predictors_p), "_permuted")

  #Concatenating predictors, permuted predictors and label
  dt <- cbind.data.frame(predictors,
                         predictors_p,
                         entire_data %>% select(y)
  )

  # Simulation
  if(is.null(num_cores_parallel) == TRUE){
    # Sequential implementation
    vimp_sim <- NULL

    for (i in 1:nsim){
      if((i %% 50 == 0) | (i==1)) {
        cat(paste0(format(Sys.time()), ": dataframe = ", data_name, " nsim = ", nsim, " num.trees = ", num.trees,". Running step ", i, "\n"))
      }

      #reshuffle row wise
      dt[,(ncol(predictors_p)+1):(2*ncol(predictors_p))] <- dt[sample(1:n), (ncol(predictors_p)+1):(2*ncol(predictors_p))]

      vimp_sim[[i]] <- (ranger::ranger(y = dt$y, x = dt %>% select(-y),
                                       importance = importance,
                                       replace = TRUE,
                                       num.trees = num.trees,
                                       scale.permutation.importance = TRUE,
                                       num.threads = num.threads,
                                       write.forest = write.forest,
                                       respect.unordered.factors = 'order',
                                       ...))$variable.importance %>%
        t() %>%
        as.data.frame()

    }

  } else{

    # Parallel implementation with cluster specified by the number of cores
    # Register cluster
    cluster <- parallel::makeCluster(num_cores_parallel)
    #doParallel::registerDoParallel(cluster)
    doSNOW::registerDoSNOW(cluster)

    # Progress track
    pb <- utils::txtProgressBar(max = nsim, style = 3)
    progress <- function(n) utils::setTxtProgressBar(pb, n)
    opts <- list(progress = progress)

    vimp_sim <- foreach(i = 1:nsim,
                        .packages = c('ranger', 'dplyr'),
                        .options.snow = opts) %dorng% {

                          # Reshuffle row-wise
                          dt[,(ncol(predictors_p)+1):(2*ncol(predictors_p))] <- dt[sample(1:n), (ncol(predictors_p)+1):(2*ncol(predictors_p))]

                          vimp <- (ranger::ranger(y = dt$y,
                                                  x = dt %>% select(-y),
                                                  importance = importance,
                                                  replace = TRUE,
                                                  num.trees = num.trees,
                                                  scale.permutation.importance = TRUE,
                                                  num.threads = num.threads,
                                                  write.forest = write.forest,
                                                  respect.unordered.factors = 'order'))$variable.importance %>%
                            t() %>%
                            as.data.frame()

                          vimp  # Return the result for each iteration
                        }

    close(pb)
    # Stop the parallel backend
    #parallel::stopCluster(cluster)
    parallel::stopCluster(cluster)

    # Come back to sequential computing
    foreach::registerDoSEQ()
  }

  # putting all results in a df
  df_sim <- as.data.frame(do.call(rbind, vimp_sim))


  # Storing result to be returned
  res <- list(vim_simulated = df_sim,
              controls = list(..., nsim = nsim))
}



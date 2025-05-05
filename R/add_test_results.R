#' Identify significant covariates with FWER, FDR, or no multiple testing
#' adjustment
#'
#' Calculate p-values using pooled or per variable approach and identify
#' significant variables using FWER, FDR or no multiple testing adjustment of
#' p-values for a given alpha level of significance.
#'
#' @param vimpermsim List, an output of the `vim_perm_sim()` function that
#'   stores `niters` variable importance values for both the original and
#'   row-wise permuted predictors.
#' @param alpha Numeric, the significance level, must be between 0 and 1.
#' @param init_num_vars Numeric, the number of covariates originally included in
#'   the data. Required to correctly apply the Benjamini-Hochberg (FDR) and Holm
#'   (FWER) p-value correction
#' @param to_show Character, one of `"FWER"`, `"FDR"` or `"unadjusted"`.
#'  * `"FWER"` (the default) - the output includes unadjusted,
#'   Benjamini-Hochberg (FDR) and Holm (FWER) adjusted p-values together with
#'   the decision whether the variable is significant or not (1 - significant, 0
#'   means not significant) according to the chosen criterium.
#'  * `"FDR"` - the output includes both unadjusted and FDR adjusted p-values along
#'   with the decision.
#'  * `"unadjusted:` - the output contains only raw, unadjusted p-values together
#'   with the decision.
#' @return A list of length 3 containing the following elements:
#'  * `vim_simulated` -  a data frame with variable importances stored in a
#'   `vimpermsim` input object (obtained from the `vim_perm_sim()` function).
#'  * `test_results` - a list consisting of 2 data frames called `pooled` and
#'   `per_variable`. The `pooled` data frame contains p-values obtained using
#'   the "pooled" approach. The `per_variable` data frame stores p-values
#'   obtained by using the "per variable" approach. Both data frames also
#'   contain decisions about variable importance based on the displayed
#'   p-values. The type of decisions displayed (based on FWER/FDR/unadjusted
#'   p-values) depends on the selected value of the `to_show` parameter.
#'
#'   In fact, the output of the `add_test_results()` function is the output of
#'   `vim_perm_sim()` with an additional layer - the `test_results` list.
#' @noRd
#' @import dplyr
#' @importFrom magrittr %>%
#' @importFrom stats p.adjust median ecdf sd
#' @examples
#' data(mtcars)
#' # Create vimpermsim object first
#' # When working with real data, increase num.trees value or leave default
#' # Here this parameter is set to a small value in order to reduce the runtime
#'# Function to make sure proper number of cores is specified for multithreading
#' safe_num_threads <- function(n) {
#'   available <- parallel::detectCores()
#'   if (n > available) available else n
#' }
#' cars_vps <- vim_perm_sim(
#'   data = mtcars, outcome_var = "vs", niters = 30,
#'   num.trees = 50, num.threads = safe_num_threads(1)
#' )
#' init_num_vars <- ncol(x = mtcars) - 1
#'
#' # Display decisions based on all available p-values (FWER, FDR, unadjusted)
#' cars_add_fwer <- add_test_results(
#'   vimpermsim = cars_vps, alpha = 0.05,
#'   init_num_vars = init_num_vars
#' )
#'
#' # Display decisions based on FDR adjusted and unadjusted p-values,
#' # expected warning
#' cars_add_fdr <- suppressWarnings(add_test_results(
#'   vimpermsim = cars_vps, alpha = 0.05,
#'   init_num_vars = init_num_vars, to_show = "FDR"
#' ))
#'
#' # Display decisions based on unadjusted p-values (Type1_confirmed column),
#' # expected warning
#' cars_add_unadj <- suppressWarnings(add_test_results(
#'   vimpermsim = cars_vps, alpha = 0.05,
#'   init_num_vars = init_num_vars, to_show = "unadjusted"
#' ))
add_test_results <- function(vimpermsim,
                             alpha = 0.05,
                             init_num_vars,
                             to_show = c("FWER", "FDR", "unadjusted")) {
  # Ensure to_show is one of the allowed values
  to_show <- match.arg(to_show)

  # Ensure alpha is numeric
  if (is.numeric(alpha) == FALSE) {
    stop("`alpha` must be numeric.")
  } else if (length(alpha) > 1 || alpha > 1 || alpha < 0) {
    stop("`alpha` must be a single numeric between 0 and 1.")
  }

  vim_simulated <- vimpermsim$vim_simulated

  # split data
  originals <- vim_simulated %>%
    select(-ends_with("_permuted"))
  shadows <- vim_simulated %>%
    select(ends_with("_permuted"))

  # calculate medians of original variables
  medians <- originals %>% summarise(across(everything(), median))
  varnames_originals <- names(medians)

  # per variable
  # compare medians to quantiles of respective shadow variable
  quants_per_variable <- lapply(varnames_originals, function(x) {
    ec <- ecdf(c(shadows[[paste0(x, "_permuted")]], Inf))
    ec(medians[[x]])
  })

  # p-value correction - per variable method
  quants_per_variable_df <- data.frame(
    "varname" = varnames_originals,
    "quantile_per_variable" = unlist(quants_per_variable)
  ) %>%
    arrange(desc(.data[["quantile_per_variable"]])) %>%
    mutate(
      p_unadj = 1 - .data[["quantile_per_variable"]],
      p_adj_FDR = p.adjust(1 - .data[["quantile_per_variable"]], "BH", n = init_num_vars),
      p_adj_FWER = p.adjust(1 - .data[["quantile_per_variable"]], "holm", n = init_num_vars)
    ) %>%
    mutate(
      Type1_confirmed = ifelse(.data[["p_unadj"]] <= alpha, 1, 0),
      FDR_confirmed = ifelse(.data[["p_adj_FDR"]] <= alpha, 1, 0),
      FWER_confirmed = ifelse(.data[["p_adj_FWER"]] <= alpha, 1, 0)
    )


  # pooled
  # compare medians to pooled quantiles
  quants_pooled_fun <- function(varnames_originals) {
    # compute standard deviations to make shadows of variables with different cardinalities comparable
    sd_of_shadows <- sapply(shadows, sd)
    mean_of_shadows <- sapply(shadows, mean)

    # divide medians and shadows by sd of shadow
    medians_divided_sd <- (medians - mean_of_shadows) / sd_of_shadows
    shadows_centered <- sweep(shadows, 2, mean_of_shadows, FUN = "-")
    shadows_divided_sd <- sweep(shadows_centered, 2, sd_of_shadows, FUN = "/")

    # ecdf function
    ec_pool <- ecdf(c(unlist(shadows_divided_sd), Inf))

    # return for all varnames remaining
    return(lapply(varnames_originals, function(x) {
      ec_pool(medians_divided_sd[[x]])
    }))
  }

  quants_pooled <- quants_pooled_fun(varnames_originals)

  # p-value correction - pooled method
  quants_pooled_df <- data.frame(
    "varname" = varnames_originals,
    "quantile_pooled" = unlist(quants_pooled)
  ) %>%
    arrange(desc(.data[["quantile_pooled"]])) %>%
    mutate(
      p_unadj = 1 - .data[["quantile_pooled"]],
      p_adj_FDR = stats::p.adjust(1 - .data[["quantile_pooled"]], "BH", n = init_num_vars),
      p_adj_FWER = stats::p.adjust(1 - .data[["quantile_pooled"]], "holm", n = init_num_vars)
    ) %>%
    mutate(
      Type1_confirmed = ifelse(.data[["p_unadj"]] <= alpha, 1, 0),
      FDR_confirmed = ifelse(.data[["p_adj_FDR"]] <= alpha, 1, 0),
      FWER_confirmed = ifelse(.data[["p_adj_FWER"]] <= alpha, 1, 0)
    )

  all_cols_names <- colnames(quants_pooled_df)

  if (to_show == "unadjusted") {
    to_select <- grep("varname|unadj|Type1", all_cols_names, value = TRUE)

    quants_pooled_df <- quants_pooled_df %>%
      select(all_of(c(to_select, "quantile_pooled")))

    quants_per_variable_df <- quants_per_variable_df %>%
      select(all_of(c(to_select, "quantile_per_variable")))

    warning("By setting the `to_show` parameter to `unadjusted` you will not be able to use the `plot_vimps()` function.")
  } else if (to_show == "FDR") {
    to_select <- grep("varname|unadj|Type1|FDR", all_cols_names, value = TRUE)

    quants_pooled_df <- quants_pooled_df %>%
      select(all_of(c(to_select, "quantile_pooled")))

    quants_per_variable_df <- quants_per_variable_df %>%
      select(all_of(c(to_select, "quantile_per_variable")))

    warning("By setting the `to_show` parameter to `FDR` you will not be able to use the `plot_vimps()` function.")
  }

  vimpermsim$test_results$pooled <- quants_pooled_df
  vimpermsim$test_results$per_variable <- quants_per_variable_df

  return(vimpermsim)
}

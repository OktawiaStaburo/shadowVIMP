#' Calculate p-values (both per_variable and pooled) and identify important
#' variables when using FWER, FDR or no p-value adjustment
#'
#'
#' @param vimpermsim list, an output from `vim_perm_sim()` function
#' @param alpha numeric, the significance level, must be between 0 and 1
#' @param init_num_vars numeric, the number of covariates originally included in
#'   the data, required to correctly apply the Benjamini-Hochberg and Holm
#'   p-value correction
#' @param to_show character, one of "FWER", "FDR" or "unadjusted". If
#'   "unadjusted" is selected, then the final output will contain only raw,
#'   unadjusted p-values together with the decision. If "FDR" is selected, the
#'   final output will include both unadjusted and Benjamini-Hochberg adjusted
#'   p-values along with the decision. The last option "FWER" includes
#'   unadjusted, Benjamini-Hochberg and Holm adjusted p-values together with the
#'   decision. Default is "FWER".
#' @return list, contains the results stored in the output of the
#'   `vim_perm_sim()` function Additionally, `$test_results$per_variable` and
#'   `out_add$test_results$pooled` show which variables were found to be
#'   important when applying FWER, FDR and no p-value adjustments and when using
#'   "per_variable" and "pooled" p-values respectively.
#' @export
#' @import magrittr dplyr stats
#' @examples
add_test_results <- function(vimpermsim,
                             alpha = 0.05,
                             init_num_vars,
                             to_show = c("FWER", "FDR", "unadjusted")) {
  # Ensure to_show is one of the allowed values
  to_show <- match.arg(to_show)

  # Ensure alpha is numeric
  if(is.numeric(alpha) == FALSE){
    stop("`alpha` must be numeric.")
  } else if(length(alpha) > 1|| alpha > 1 || alpha < 0){
    stop("`alpha` must be a single numeric between 0 and 1.")
  }

  vim_simulated <- vimpermsim$vim_simulated

  # split data
  originals <- vim_simulated %>%
    select(-ends_with("_permuted"))
  shadows <- vim_simulated %>%
    select(ends_with("_permuted"))

  # calculate medians of original variables
  medians <- originals %>% summarise_all(median) #TODO - summarise_all() has been suppressed - change for the currently supported solution
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
    arrange(desc(quantile_per_variable)) %>%
    mutate(
      p_unadj = 1 - quantile_per_variable,
      p_adj_FDR = stats::p.adjust(1 - quantile_per_variable, "BH", n = init_num_vars),
      p_adj_FWER = stats::p.adjust(1 - quantile_per_variable, "holm", n = init_num_vars)
    ) %>%
    mutate(
      Type1_confirmed = ifelse(p_unadj <= alpha, 1, 0),
      FDR_confirmed = ifelse(p_adj_FDR <= alpha, 1, 0),
      FWER_confirmed = ifelse(p_adj_FWER <= alpha, 1, 0)
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
    arrange(desc(quantile_pooled)) %>%
    mutate(
      p_unadj = 1 - quantile_pooled,
      p_adj_FDR = stats::p.adjust(1 - quantile_pooled, "BH", n = init_num_vars),
      p_adj_FWER = stats::p.adjust(1 - quantile_pooled, "holm", n = init_num_vars)
    ) %>%
    mutate(
      Type1_confirmed = ifelse(p_unadj <= alpha, 1, 0),
      FDR_confirmed = ifelse(p_adj_FDR <= alpha, 1, 0),
      FWER_confirmed = ifelse(p_adj_FWER <= alpha, 1, 0)
    )

  all_cols_names <- colnames(quants_pooled_df)

  if (to_show == "unadjusted") {
    to_select <- grep("varname|unadj|Type1", all_cols_names, value = TRUE)

    quants_pooled_df <- quants_pooled_df %>%
      select(all_of(to_select), "quantile_pooled")

    quants_per_variable_df <- quants_per_variable_df %>%
      select(all_of(to_select), "quantile_per_variable")

    warning("By setting the `to_show` parameter to `unadjusted` you will not be able to use the `plot_vimps()` function.")
  } else if (to_show == "FDR") {
    to_select <- grep("varname|unadj|Type1|FDR", all_cols_names, value = TRUE)

    quants_pooled_df <- quants_pooled_df %>%
      select(all_of(to_select), "quantile_pooled")

    quants_per_variable_df <- quants_per_variable_df %>%
      select(all_of(to_select), "quantile_per_variable")

    warning("By setting the `to_show` parameter to `FDR` you will not be able to use the `plot_vimps()` function.")
  }

  vimpermsim$test_results$pooled <- quants_pooled_df
  vimpermsim$test_results$per_variable <- quants_per_variable_df

  return(vimpermsim)
}

# Proper

# Errors
test_that("plot_vimps throws an error when inappropiate inputs are provided", {
  # df data defined in helper.R
  out_wrapper <- vim_perm_sim_wrapper(
    alphas = c(0.3, 0.10, 0.05), nsims = c(10, 15, 20),
    entire_data = df, outcome_var = "diagnosis", method = "per_variable"
  )

  expect_error(plot_vimps(wrapper_object = out_wrapper, pooled = 1), class = "simpleError")
  expect_error(plot_vimps(wrapper_object = out_wrapper, pooled = "cat"), class = "simpleError")
  expect_error(plot_vimps(wrapper_object = out_wrapper, filter_vars = -1), class = "simpleError")
  expect_error(plot_vimps(wrapper_object = out_wrapper, filter_vars = "24"), class = "simpleError")
  expect_error(plot_vimps(wrapper_object = out_wrapper, filter_vars = TRUE), class = "simpleError")
  expect_error(plot_vimps(wrapper_object = out_wrapper), class = "simpleError")

})


test_that("plot_vimps throws an error when `to_show` in wrapper is set to `FDR` or `unadjusted`", {
  out_fdr <- suppressWarnings(vim_perm_sim_wrapper(
    alphas = c(0.3, 0.10, 0.05), nsims = c(10, 15, 20),
    entire_data = df, outcome_var = "diagnosis", to_show = "FDR"
  ))

  out_unadj <- suppressWarnings(vim_perm_sim_wrapper(
    alphas = c(0.3, 0.10, 0.05), nsims = c(10, 15, 20),
    entire_data = df, outcome_var = "diagnosis", to_show = "unadjusted"
  ))

  expect_error(plot_vimps(wrapper_object = out_fdr), class = "simpleError")
  expect_error(plot_vimps(wrapper_object = out_unadj), class = "simpleError")
})


test_that("plot_vimps throws an error when `save_vimp_history` is set to `none`",{
  out_none <- vim_perm_sim_wrapper(
    alphas = c(0.3, 0.10, 0.05), nsims = c(10, 15, 20),
    entire_data = df, outcome_var = "diagnosis", save_vimp_history = "none"
  )

  expect_error(plot_vimps(wrapper_object = out_none), class = "simpleError")
})


# Warnings
# TODO - 2 tests below are problematic - fix them

# test_that("plot_vimps gives a warning if more than 40 variables are to be plotted", {
#   df_large <- data.frame(diagnosis = c(rep(0, 50), rep(1, 50)))
#   n_rows <- nrow(df_large)
#   for (i in 1:50) {
#     df_large[[paste0("v", i)]] <- c(rep(i, 50), rep(i+1, 50))
#   }
#
#   out_large <- vim_perm_sim_wrapper(
#     alphas = c(0.3, 0.10, 0.05), nsims = c(10, 15, 20),
#     entire_data = df_large, outcome_var = "diagnosis"
#   )
#
#   expect_warning(plot_vimps(wrapper_object = out_large), class =  "simpleWarning")
#
# })
#
# test_that("plot_vimps gives a warning if you try to plot more variables than the
#           number of available variables", {
#   out <- vim_perm_sim_wrapper(
#     alphas = c(0.3, 0.10, 0.05), nsims = c(10, 15, 20),
#     entire_data = df, outcome_var = "diagnosis"
#   )
#
#   #change for 18 in filtering
#   expect_warning(plot_vimps(wrapper_object = out, filter_vars = 25), class =  "simpleWarning")
# })
#







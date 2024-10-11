# df data defined in helper.R
# Normal usage
test_that("plot_vimps works with appropiate arguments",{
  out_pooled <- vim_perm_sim_wrapper(alphas = c(0.3, 0.10, 0.05), nsims = c(10, 15, 20),
    entire_data = df, outcome_var = "diagnosis")

  expect_no_error(plot_vimps(wrapper_object = out_pooled, pooled = TRUE))
  expect_no_error(plot_vimps(wrapper_object = out_pooled, filter_vars = 5))
  expect_no_error(plot_vimps(wrapper_object = out_pooled, text_size = 4))

  out_per_variable <- vim_perm_sim_wrapper(alphas = c(0.3, 0.10, 0.05), nsims = c(10, 15, 20),
                                           entire_data = df, outcome_var = "diagnosis",
                                           method = "per_variable")

  expect_no_error(plot_vimps(wrapper_object = out_per_variable, pooled = FALSE))
  expect_no_error(plot_vimps(wrapper_object = out_per_variable, pooled = FALSE, filter_vars = 5))
  expect_no_error(plot_vimps(wrapper_object = out_per_variable, pooled = FALSE, text_size = 5))
})

test_that("plot_vimps creates a ggplot output", {
  out_pooled <- vim_perm_sim_wrapper(alphas = c(0.3, 0.10, 0.05), nsims = c(10, 15, 20),
                                     entire_data = df, outcome_var = "diagnosis")

  plot <- plot_vimps(wrapper_object = out_pooled)
  plot_filter <- plot_vimps(wrapper_object = out_pooled, filter_vars = 5)

  expect_s3_class(plot, "gg")
  expect_s3_class(plot_filter, "gg")

})


# Errors
test_that("plot_vimps throws an error when inappropiate inputs are provided", {
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
test_that("plot_vimps gives a warning if user tries to filter another than integer number of covariates", {
  out <- vim_perm_sim_wrapper(nsims = c(10,15,20), entire_data = df_large, outcome_var = "diagnosis")
  expect_warning(plot_vimps(wrapper_object = out, filter_vars = 9.5), class = "simpleWarning")
})

test_that("plot_vimps gives a warning if more than 40 variables are to be plotted", {
  out_large <- vim_perm_sim_wrapper(
    alphas = c(0.3, 0.10, 0.05), nsims = c(10, 15, 20),
    entire_data = df_large, outcome_var = "diagnosis"
  )

  expect_warning(plot_vimps(wrapper_object = out_large), class =  "simpleWarning")

})

test_that("plot_vimps gives a warning if you try to plot more variables than the
          number of available variables", {
  out <- vim_perm_sim_wrapper(
    alphas = c(0.3, 0.10, 0.05), nsims = c(10, 15, 20),
    entire_data = df, outcome_var = "diagnosis"
  )
  expect_warning(plot_vimps(wrapper_object = out, filter_vars = 18), class =  "simpleWarning")
})

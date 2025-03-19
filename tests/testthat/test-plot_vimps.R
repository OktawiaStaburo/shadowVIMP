# df data defined in helper.R
# Normal usage
test_that("plot_vimps works with appropiate arguments", {
  out_pooled <- shadow_vimp(
    alphas = c(0.3, 0.10, 0.05), niters = c(10, 15, 20),
    data = df, outcome_var = "diagnosis"
  )

  expect_no_error(plot_vimps(shadow_vimp_out = out_pooled, pooled = TRUE))
  expect_no_error(plot_vimps(shadow_vimp_out = out_pooled, filter_vars = 5))
  expect_no_error(plot_vimps(shadow_vimp_out = out_pooled, text_size = 5))
  expect_no_error(plot_vimps(shadow_vimp_out = out_pooled, p_val_labels = FALSE))
  expect_no_error(plot_vimps(shadow_vimp_out = out_pooled, legend.position = "none"))
  expect_no_error(plot_vimps(shadow_vimp_out = out_pooled, legend.position = "bottom"))
  expect_no_error(plot_vimps(
    shadow_vimp_out = out_pooled,
    category_colors = c(
      "FWER conf." = "red",
      "FDR conf." = "blue",
      "Unadjusted conf." = "green",
      "Not significant" = "yellow"
    )
  ))
  expect_no_error(plot_vimps(shadow_vimp_out = out_pooled, helper.legend = FALSE))

  out_per_variable <- shadow_vimp(
    alphas = c(0.3, 0.10, 0.05), niters = c(10, 15, 20),
    data = df, outcome_var = "diagnosis",
    method = "per_variable"
  )

  expect_no_error(plot_vimps(shadow_vimp_out = out_per_variable, pooled = FALSE))
  expect_no_error(plot_vimps(shadow_vimp_out = out_per_variable, pooled = FALSE, filter_vars = 5))
  expect_no_error(plot_vimps(shadow_vimp_out = out_per_variable, pooled = FALSE, text_size = 5))
})

test_that("plot_vimps creates a ggplot output", {
  out_pooled <- shadow_vimp(
    alphas = c(0.3, 0.10, 0.05), niters = c(10, 15, 20),
    data = df, outcome_var = "diagnosis"
  )

  plot <- plot_vimps(shadow_vimp_out = out_pooled)
  plot_filter <- plot_vimps(shadow_vimp_out = out_pooled, filter_vars = 5)

  expect_s3_class(plot, "gg")
  expect_s3_class(plot_filter, "gg")
})


# Errors
test_that("plot_vimps throws an error when inappropiate inputs are provided", {
  out_wrapper <- shadow_vimp(
    alphas = c(0.3, 0.10, 0.05), niters = c(10, 15, 20),
    data = df, outcome_var = "diagnosis", method = "per_variable"
  )

  expect_error(plot_vimps(shadow_vimp_out = out_wrapper, pooled = 1), class = "simpleError")
  expect_error(plot_vimps(shadow_vimp_out = out_wrapper, pooled = "cat"), class = "simpleError")

  expect_error(plot_vimps(shadow_vimp_out = out_wrapper, filter_vars = -1), class = "simpleError")
  expect_error(plot_vimps(shadow_vimp_out = out_wrapper, filter_vars = "24"), class = "simpleError")
  expect_error(plot_vimps(shadow_vimp_out = out_wrapper, filter_vars = TRUE), class = "simpleError")

  expect_error(plot_vimps(shadow_vimp_out = out_wrapper), class = "simpleError")

  expect_error(plot_vimps(shadow_vimp_out = out_wrapper, p_val_labels = 1), class = "simpleError")
  expect_error(plot_vimps(shadow_vimp_out = out_wrapper, p_val_labels = "yes"), class = "simpleError")

  expect_error(plot_vimps(shadow_vimp_out = out_wrapper, helper.legend = "yes"), class = "simpleError")
  expect_error(plot_vimps(shadow_vimp_out = out_wrapper, helper.legend = 1), class = "simpleError")

  expect_error(plot_vimps(shadow_vimp_out = out_wrapper, legend.position = "upper corner"), class = "simpleError")
  expect_error(plot_vimps(shadow_vimp_out = out_wrapper, legend.position = 4), class = "simpleError")
  expect_error(plot_vimps(shadow_vimp_out = out_wrapper, legend.position = TRUE), class = "simpleError")

  expect_error(plot_vimps(shadow_vimp_out = out_wrapper, category_colors = c(1:4)), class = "simpleError")
  expect_error(plot_vimps(shadow_vimp_out = out_wrapper, category_colors = c("cat", "dog", "hamster")), class = "simpleError")
  expect_error(plot_vimps(shadow_vimp_out = out_wrapper, category_colors = c("cat", "dog", "hamster", "squirrel")), class = "simpleError")
  expect_error(plot_vimps(shadow_vimp_out = out_wrapper, category_colors = c(
    "FWER conf." = "cat",
    "FDR conf." = "dog",
    "Unadjusted conf." = "hamster",
    "squirrel" = "squirrel"
  )), class = "simpleError")
})


test_that("plot_vimps throws an error when `to_show` in wrapper is set to `FDR` or `unadjusted`", {
  out_fdr <- suppressWarnings(shadow_vimp(
    alphas = c(0.3, 0.10, 0.05), niters = c(10, 15, 20),
    data = df, outcome_var = "diagnosis", to_show = "FDR"
  ))

  out_unadj <- suppressWarnings(shadow_vimp(
    alphas = c(0.3, 0.10, 0.05), niters = c(10, 15, 20),
    data = df, outcome_var = "diagnosis", to_show = "unadjusted"
  ))

  expect_error(plot_vimps(shadow_vimp_out = out_fdr), class = "simpleError")
  expect_error(plot_vimps(shadow_vimp_out = out_unadj), class = "simpleError")
})


test_that("plot_vimps throws an error when `save_vimp_history` is set to `none`", {
  out_none <- shadow_vimp(
    alphas = c(0.3, 0.10, 0.05), niters = c(10, 15, 20),
    data = df, outcome_var = "diagnosis", save_vimp_history = "none"
  )

  expect_error(plot_vimps(shadow_vimp_out = out_none), class = "simpleError")
})

# Warnings
test_that("plot_vimps gives a warning if user tries to filter another than integer number of covariates", {
  out <- shadow_vimp(niters = c(10, 15, 20), data = df_large, outcome_var = "diagnosis")
  expect_warning(plot_vimps(shadow_vimp_out = out, filter_vars = 9.5), class = "simpleWarning")
})

test_that("plot_vimps gives a warning if more than 40 variables are to be plotted", {
  out_large <- shadow_vimp(
    alphas = c(0.3, 0.10, 0.05), niters = c(10, 15, 20),
    data = df_large, outcome_var = "diagnosis"
  )

  expect_warning(plot_vimps(shadow_vimp_out = out_large), class = "simpleWarning")
})

test_that("plot_vimps gives a warning if you try to plot more variables than the
          number of available variables", {
  out <- shadow_vimp(
    alphas = c(0.3, 0.10, 0.05), niters = c(10, 15, 20),
    data = df, outcome_var = "diagnosis"
  )
  expect_warning(plot_vimps(shadow_vimp_out = out, filter_vars = 18), class = "simpleWarning")
})

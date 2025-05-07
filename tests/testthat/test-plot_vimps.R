# df data defined in helper.R
# We use suppressWarnings() because in the below tests we use too small number
# of niters to get reliable results so shadow_vimp() is giving a warning
# Here it's more importnat to run tests fast, not to get statistically reliable results

# Normal usage of plot function
test_that("plot_vimps works with appropiate arguments", {
  out_pooled <- suppressWarnings(shadow_vimp(
    alphas = c(0.3, 0.10, 0.05), niters = c(10, 15, 20),
    data = df, outcome_var = "diagnosis", num.threads = 1
  ))

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
  expect_no_error(plot_vimps(shadow_vimp_out = out_pooled, helper_legend = FALSE))

  out_per_variable <- suppressWarnings(shadow_vimp(
    alphas = c(0.3, 0.10, 0.05), niters = c(10, 15, 20),
    data = df, outcome_var = "diagnosis",
    method = "per_variable", num.threads = 1
  ))

  expect_no_error(plot_vimps(shadow_vimp_out = out_per_variable, pooled = FALSE))
  expect_no_error(plot_vimps(shadow_vimp_out = out_per_variable, pooled = FALSE, filter_vars = 5))
  expect_no_error(plot_vimps(shadow_vimp_out = out_per_variable, pooled = FALSE, text_size = 5))
})

test_that("plot_vimps creates a ggplot output", {
  out_pooled <- suppressWarnings(shadow_vimp(
    alphas = c(0.3, 0.10, 0.05), niters = c(10, 15, 20),
    data = df, outcome_var = "diagnosis", num.threads = 1
  ))

  plot <- plot_vimps(shadow_vimp_out = out_pooled)
  plot_filter <- plot_vimps(shadow_vimp_out = out_pooled, filter_vars = 5)

  expect_s3_class(plot, "gg")
  expect_s3_class(plot_filter, "gg")
})


# Errors
test_that("plot_vimps throws an error when inappropiate inputs are provided", {
  out_wrapper <- suppressWarnings(shadow_vimp(
    alphas = c(0.3, 0.10, 0.05), niters = c(10, 15, 20),
    data = df, outcome_var = "diagnosis", method = "per_variable", num.threads = 1
  ))

  expect_error(plot_vimps(shadow_vimp_out = out_wrapper, pooled = 1), class = "simpleError")
  expect_error(plot_vimps(shadow_vimp_out = out_wrapper, pooled = "cat"), class = "simpleError")

  expect_error(plot_vimps(shadow_vimp_out = out_wrapper, filter_vars = -1), class = "simpleError")
  expect_error(plot_vimps(shadow_vimp_out = out_wrapper, filter_vars = "24"), class = "simpleError")
  expect_error(plot_vimps(shadow_vimp_out = out_wrapper, filter_vars = TRUE), class = "simpleError")

  expect_error(plot_vimps(shadow_vimp_out = out_wrapper), class = "simpleError")

  expect_error(plot_vimps(shadow_vimp_out = out_wrapper, p_val_labels = 1), class = "simpleError")
  expect_error(plot_vimps(shadow_vimp_out = out_wrapper, p_val_labels = "yes"), class = "simpleError")

  expect_error(plot_vimps(shadow_vimp_out = out_wrapper, helper_legend = "yes"), class = "simpleError")
  expect_error(plot_vimps(shadow_vimp_out = out_wrapper, helper_legend = 1), class = "simpleError")

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
    data = df, outcome_var = "diagnosis", to_show = "FDR", num.threads = 1
  ))

  out_unadj <- suppressWarnings(shadow_vimp(
    alphas = c(0.3, 0.10, 0.05), niters = c(10, 15, 20),
    data = df, outcome_var = "diagnosis", to_show = "unadjusted", num.threads = 1
  ))

  expect_error(plot_vimps(shadow_vimp_out = out_fdr), class = "simpleError")
  expect_error(plot_vimps(shadow_vimp_out = out_unadj), class = "simpleError")
})


test_that("plot_vimps throws an error when `save_vimp_history` is set to `none`", {
  out_none <- suppressWarnings(shadow_vimp(
    alphas = c(0.3, 0.10, 0.05), niters = c(10, 15, 20),
    data = df, outcome_var = "diagnosis", save_vimp_history = "none",
    num.threads = 1
  ))

  expect_error(plot_vimps(shadow_vimp_out = out_none), class = "simpleError")
})

# Warnings
test_that("plot_vimps gives a warning if user tries to filter another than integer number of covariates", {
  out <- suppressWarnings(shadow_vimp(
    niters = c(10, 15, 20),
    data = df_large, outcome_var = "diagnosis", num.threads = 1
  ))
  expect_warning(plot_vimps(shadow_vimp_out = out, filter_vars = 9.5), class = "simpleWarning")
})

test_that("plot_vimps gives a warning if more than 40 variables are to be plotted", {
  out_large <- suppressWarnings(shadow_vimp(
    alphas = c(0.3, 0.10, 0.05), niters = c(10, 15, 20),
    data = df_large, outcome_var = "diagnosis", num.threads = 1
  ))

  expect_warning(plot_vimps(shadow_vimp_out = out_large), class = "simpleWarning")
})

test_that("plot_vimps gives a warning if you try to plot more variables than the
          number of available variables", {
  out <- suppressWarnings(shadow_vimp(
    alphas = c(0.3, 0.10, 0.05), niters = c(10, 15, 20),
    data = df, outcome_var = "diagnosis", num.threads = 1
  ))
  expect_warning(plot_vimps(shadow_vimp_out = out, filter_vars = 18), class = "simpleWarning")
})

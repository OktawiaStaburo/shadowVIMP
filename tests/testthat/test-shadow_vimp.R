# df defined in helper.R
trees <- 5
# We use suppressWarnings() because in the below tests we use too small number
# of niters to get reliable results so shadow_vimp() is giving a warning
# Here it's more importnat to run tests fast, not to get statistical reliable results
test_that("shadow_vimp works as expected with default settings.", {
  out <- suppressWarnings(shadow_vimp(
    alphas = c(0.3, 0.10, 0.05), niters = c(10, 10, 10),
    data = df, outcome_var = "diagnosis", num.trees = trees, num.threads = 1
  ))

  expect_length(out, 7)
  expect_s3_class(out$vimp_history, class = "data.frame")
  expect_s3_class(out$final_dec_pooled, class = "data.frame")
  expect_type(out$alpha, type = "double")
  expect_type(out$step_all_covariates_removed, type = "double")
  expect_equal(out$step_all_covariates_removed %in% c(0:length(out$alpha)), TRUE)
  expect_type(out$time_elapsed, type = "list")
  expect_type(out$pre_selection, type = "list")
  expect_type(out$call, type = "language")
})

test_that("Different variants of save_vimp_history parameter work as expected.", {
  out_history_none <- suppressWarnings(shadow_vimp(
    alphas = c(0.3, 0.10, 0.05), niters = c(10, 10, 10),
    data = df, outcome_var = "diagnosis", num.trees = trees,
    save_vimp_history = "none", num.threads = 1
  ))

  out_history_last <- suppressWarnings(shadow_vimp(
    alphas = c(0.3, 0.10, 0.05), niters = c(10, 10, 10),
    data = df, outcome_var = "diagnosis", num.trees = trees,
    save_vimp_history = "last", num.threads = 1
  ))

  expect_type(out_history_none$vimp_history, type = "NULL")
  expect_length(out_history_none$pre_selection$step_1, n = 2)
  expect_length(out_history_none$pre_selection$step_2, n = 2)
  expect_s3_class(out_history_last$vimp_history, class = "data.frame")
  expect_length(out_history_last$pre_selection$step_1, n = 2)
  expect_length(out_history_last$pre_selection$step_1, n = 2)
})

test_that("Different values of method parameter work as expected.", {
  out_pooled <- suppressWarnings(shadow_vimp(
    alphas = c(0.3, 0.10, 0.05), niters = c(10, 10, 10),
    data = df, outcome_var = "diagnosis", num.trees = trees,
    method = "pooled", num.threads = 1
  ))

  out_per_var <- suppressWarnings(shadow_vimp(
    alphas = c(0.3, 0.10, 0.05), niters = c(10, 10, 10),
    data = df, outcome_var = "diagnosis", num.trees = trees,
    method = "per_variable", num.threads = 1
  ))

  expect_s3_class(out_pooled$final_dec_pooled, class = "data.frame")
  expect_s3_class(out_pooled$pre_selection$step_1$decision_pooled, class = "data.frame")
  expect_s3_class(out_pooled$pre_selection$step_2$decision_pooled, class = "data.frame")
  expect_s3_class(out_per_var$final_dec_per_variable, class = "data.frame")
  expect_s3_class(out_per_var$pre_selection$step_1$decision_pooled, class = "data.frame")
  expect_s3_class(out_per_var$pre_selection$step_2$decision_pooled, class = "data.frame")
})

# Errors
test_that("shadow_vimp() fails when inappropiate inputs are passed", {
  expect_error(shadow_vimp(
    alphas = c(0.3, 0.10, 0.05), niters = c(10, 20), num.trees = trees,
    data = df, outcome_var = "diagnosis", num.threads = 1
  ), class = "simpleError")
  expect_error(shadow_vimp(
    alphas = c(0.3, 0.05, 0.15), niters = c(10, 20, 30), num.trees = trees,
    data = df, outcome_var = "diagnosis", num.threads = 1
  ), class = "simpleError")
  expect_error(shadow_vimp(
    alphas = c(-0.3, 0.05, 0.15), niters = c(10, 20, 30), num.trees = trees,
    data = df, outcome_var = "diagnosis", num.threads = 1
  ), class = "simpleError")
  expect_error(shadow_vimp(
    alphas = c(0.3, 0.15, 1.15), niters = c(10, 20, 30), num.trees = trees,
    data = df, outcome_var = "diagnosis", num.threads = 1
  ), class = "simpleError")
  expect_error(
    shadow_vimp(
      alphas = c(0.3, 0.10, 0.05), niters = c(10, 20, 30), num.trees = trees,
      data = df, outcome_var = "diagnosis", save_vimp_history = "cat",
      num.threads = 1
    ),
    class = "simpleError"
  )
  expect_error(
    shadow_vimp(
      alphas = c(0.3, 0.10, 0.05), niters = c(10, 20, 30), num.trees = trees,
      data = df, outcome_var = "diagnosis", to_show = "cat", num.threads = 1
    ),
    class = "simpleError"
  )
  expect_error(
    shadow_vimp(
      alphas = c(0.3, 0.10, 0.05), niters = c(10, 20, 30), num.trees = trees,
      data = df, outcome_var = "diagnosis", method = "cat", num.threads = 1
    ),
    class = "simpleError"
  )
})

# Warnings:
test_that("shadow_vimp() returns a warning when no wariables survive pre-selection process and all shadow variables always have VIMP equal to zero.", {
  nonsense_df <- data.frame(
    diagnosis = c(rep(1, 10), rep(0, 10)),
    v1 = rep(45, 20), v2 = rep(55, 20), v3 = rep(65, 20)
  )

  # Capture all warnings
  warnings <- capture_warnings(
    shadow_vimp(
      alphas = c(0.3, 0.10, 0.05), niters = c(10, 15, 20), num.trees = trees,
      data = nonsense_df, outcome_var = "diagnosis", num.threads = 1
    )
  )

  expect_equal(length(warnings) > 1, TRUE)
  expect_true(any(grepl("None of the variables have passed the pre-selection process to the final step.", warnings)))
})

test_that("shadow_vimp() returns a warning when one or more shadow variables have VIMP always equal to 0.", {
  out <- capture_warnings(shadow_vimp(
    alphas = c(0.3, 0.10, 0.05), niters = c(10, 15, 20), num.trees = trees,
    data = df_const, outcome_var = "diagnosis", num.threads = 1
  ))

  expect_true(any(grepl("One or more shadow variables always have VIMP equal to zero.", out)))
})

test_that("shadow_vimp() issues a warning when niters is set to a value that is too low for the pooled method.", {
  # Too small niters for all steps
  warnings <- capture_warnings(
    out <- shadow_vimp(
      alphas = c(0.3, 0.10, 0.05), niters = c(3, 9, 10),
      data = df, outcome_var = "diagnosis", num.threads = 1, num.trees = 10
    )
  )
  expect_true(any(grepl("Not enough iterations for any positives after FDR/FWER adjustment.\n Increase the number of iterations in the pre-selection phase to get reliable results.", warnings)))

  # Too small niters for the 2nd and the last step
  warnings2 <- capture_warnings(
    out <- shadow_vimp(
      alphas = c(0.3, 0.10, 0.05), niters = c(5, 9, 12),
      data = df, outcome_var = "diagnosis", num.threads = 1, num.trees = 10
    )
  )
  expect_true(any(grepl("Not enough iterations for any positives after FDR/FWER adjustment.\n Increase the number of iterations in the pre-selection phase to get reliable results.", warnings2)))

  # Too small niters for the last step
  warnings3 <- capture_warnings(
    out <- shadow_vimp(
      alphas = c(0.3, 0.10, 0.05), niters = c(5, 40, 12),
      data = df, outcome_var = "diagnosis", num.threads = 1, num.trees = 10
    )
  )
  expect_true(any(grepl("Not enough iterations for any positives after FDR/FWER adjustment.\n Increase the number of iterations in the final step to get reliable results.", warnings3)))
})

test_that("shadow_vimp() issues a warning when niters is set to a value that is too low for the per_variable method.", {
  # Too small niters for all steps
  warnings <- capture_warnings(
    out <- shadow_vimp(
      alphas = c(0.3, 0.10, 0.05), niters = c(3, 9, 10),
      data = df, outcome_var = "diagnosis",
      num.threads = 1,
      method = "per_variable", num.trees = 50
    )
  )
  expect_true(any(grepl("Not enough iterations for any positives after FDR/FWER adjustment.\n Increase the number of iterations in the pre-selection phase to get reliable results.", warnings)))

  # Too small niters for the 2nd and the last step
  warnings2 <- capture_warnings(
    out <- shadow_vimp(
      alphas = c(0.3, 0.10, 0.05), niters = c(70, 10, 15),
      data = df, outcome_var = "diagnosis", num.threads = 1,
      method = "per_variable", num.trees = 50
    )
  )
  expect_true(any(grepl("Not enough iterations for any positives after FDR/FWER adjustment.\n Increase the number of iterations in the pre-selection phase to get reliable results.", warnings2)))

  # Too small niters for the last step
  warnings3 <- capture_warnings(
    out <- shadow_vimp(
      alphas = c(0.3, 0.10, 0.05), niters = c(70, 205, 12),
      data = df, outcome_var = "diagnosis",
      num.threads = 1, num.trees = 50
    )
  )
  expect_true(any(grepl("Not enough iterations for any positives after FDR/FWER adjustment.\n Increase the number of iterations in the final step to get reliable results.", warnings3)))
})

# df defined in helper.R
test_that("shadow_vimp works as expected with default settings.", {
  out <- shadow_vimp(
    alphas = c(0.3, 0.10, 0.05), niters = c(10, 15, 20),
    entire_data = df, outcome_var = "diagnosis"
  )

  expect_length(out, 7)
  expect_s3_class(out$vimp_history, class = "data.frame")
  expect_s3_class(out$final_dec_pooled, class = "data.frame")
  expect_type(out$alpha, type = "double")
  expect_type(out$result_taken_from_previous_step, type = "logical")
  expect_type(out$time_elapsed, type = "list")
  expect_type(out$pre_selection, type = "list")
  expect_type(out$call, type = "language")
})

test_that("Different variants of save_vimp_history parameter work as expected.", {
  out_history_none <- shadow_vimp(
    alphas = c(0.3, 0.10, 0.05), niters = c(10, 15, 20),
    entire_data = df, outcome_var = "diagnosis",
    save_vimp_history = "none"
  )

  out_history_last <- shadow_vimp(
    alphas = c(0.3, 0.10, 0.05), niters = c(10, 15, 20),
    entire_data = df, outcome_var = "diagnosis",
    save_vimp_history = "last"
  )

  expect_type(out_history_none$vimp_history, type = "NULL")
  expect_length(out_history_none$pre_selection$step_1, n = 2)
  expect_length(out_history_none$pre_selection$step_2, n = 2)
  expect_s3_class(out_history_last$vimp_history, class = "data.frame")
  expect_length(out_history_last$pre_selection$step_1, n = 2)
  expect_length(out_history_last$pre_selection$step_1, n = 2)
})

test_that("Different values of method parameter work as expected.", {
  out_pooled <- shadow_vimp(
    alphas = c(0.3, 0.10, 0.05), niters = c(10, 15, 20),
    entire_data = df, outcome_var = "diagnosis",
    method = "pooled"
  )

  out_per_var <- shadow_vimp(
    alphas = c(0.3, 0.10, 0.05), niters = c(10, 15, 20),
    entire_data = df, outcome_var = "diagnosis",
    method = "per_variable"
  )

  expect_s3_class(out_pooled$final_dec_pooled, class = "data.frame")
  expect_s3_class(out_pooled$pre_selection$step_1$decision_pooled, class = "data.frame")
  expect_s3_class(out_pooled$pre_selection$step_2$decision_pooled, class = "data.frame")
  expect_s3_class(out_per_var$final_dec_per_variable, class = "data.frame")
  expect_s3_class(out_per_var$pre_selection$step_1$decision_per_variable, class = "data.frame")
  expect_s3_class(out_per_var$pre_selection$step_2$decision_per_variable, class = "data.frame")
})

# Errors
test_that("shadow_vimp() fails when inappropiate inputs are passed", {
  expect_error(shadow_vimp(
    alphas = c(0.3, 0.10, 0.05), niters = c(10, 20),
    entire_data = df, outcome_var = "diagnosis"
  ), class = "simpleError")
  expect_error(shadow_vimp(
    alphas = c(0.3, 0.05, 0.15), niters = c(10, 20, 30),
    entire_data = df, outcome_var = "diagnosis"
  ), class = "simpleError")
  expect_error(shadow_vimp(
    alphas = c(-0.3, 0.05, 0.15), niters = c(10, 20, 30),
    entire_data = df, outcome_var = "diagnosis"
  ), class = "simpleError")
  expect_error(shadow_vimp(
    alphas = c(0.3, 0.15, 1.15), niters = c(10, 20, 30),
    entire_data = df, outcome_var = "diagnosis"
  ), class = "simpleError")
  expect_error(
    shadow_vimp(
      alphas = c(0.3, 0.10, 0.05), niters = c(10, 20, 30),
      entire_data = df, outcome_var = "diagnosis", save_vimp_history = "cat"
    ),
    class = "simpleError"
  )
  expect_error(
    shadow_vimp(
      alphas = c(0.3, 0.10, 0.05), niters = c(10, 20, 30),
      entire_data = df, outcome_var = "diagnosis", to_show = "cat"
    ),
    class = "simpleError"
  )
  expect_error(
    shadow_vimp(
      alphas = c(0.3, 0.10, 0.05), niters = c(10, 20, 30),
      entire_data = df, outcome_var = "diagnosis", method = "cat"
    ),
    class = "simpleError"
  )
})

# Warnings
test_that("shadow_vimp() throws a warning when no wariables survive pre-selection process.", {
  nonsense_df <- data.frame(
    diagnosis = c(rep(1, 50), rep(0, 50)),
    v1 = rep(45, 100), v2 = rep(55, 100), v3 = rep(65, 100)
  )

  # Capture all warnings
  warnings <- capture_warnings(
    shadow_vimp(
      alphas = c(0.3, 0.10, 0.05), niters = c(10, 20, 30),
      entire_data = nonsense_df, outcome_var = "diagnosis"
    )
  )

  expect_equal(length(warnings) > 1, TRUE)
})

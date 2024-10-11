# df defined in helper.R
test_that("vim_perm_sim_wrapper works as expected with default settings.", {
  out <- vim_perm_sim_wrapper(
    alphas = c(0.3, 0.10, 0.05), nsims = c(10, 15, 20),
    entire_data = df, outcome_var = "diagnosis"
  )

  expect_length(out, 8)
  expect_s3_class(out$vimp_history, class = "data.frame")
  expect_s3_class(out$final_dec_pooled, class = "data.frame")
  expect_type(out$alpha, type = "double")
  expect_type(out$control_parameters, type = "list")
  expect_type(out$result_taken_from_previous_step, type = "logical")
  expect_type(out$time_elapsed, type = "list")
  expect_type(out$pre_selection, type = "list")
  expect_type(out$call, type = "language")
})

test_that("Different variants of save_vimp_history parameter work as expected.", {
  out_history_none <- vim_perm_sim_wrapper(
    alphas = c(0.3, 0.10, 0.05), nsims = c(10, 15, 20),
    entire_data = df, outcome_var = "diagnosis",
    save_vimp_history = "none"
  )

  out_history_last <- vim_perm_sim_wrapper(
    alphas = c(0.3, 0.10, 0.05), nsims = c(10, 15, 20),
    entire_data = df, outcome_var = "diagnosis",
    save_vimp_history = "last"
  )

  expect_type(out_history_none$vimp_history, type = "NULL")
  expect_length(out_history_none$pre_selection$step_1, n = 3)
  expect_length(out_history_none$pre_selection$step_2, n = 3)
  expect_s3_class(out_history_last$vimp_history, class = "data.frame")
  expect_length(out_history_last$pre_selection$step_1, n = 3)
  expect_length(out_history_last$pre_selection$step_1, n = 3)
})

test_that("Different values of method parameter work as expected.", {
  out_pooled <- vim_perm_sim_wrapper(
    alphas = c(0.3, 0.10, 0.05), nsims = c(10, 15, 20),
    entire_data = df, outcome_var = "diagnosis",
    method = "pooled"
  )

  out_per_var <- vim_perm_sim_wrapper(
    alphas = c(0.3, 0.10, 0.05), nsims = c(10, 15, 20),
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
test_that("vim_perm_sim_wrapper() fails when inappropiate inputs are passed", {
  expect_error(vim_perm_sim_wrapper(
    alphas = c(0.3, 0.10, 0.05), nsims = c(10, 20),
    entire_data = df, outcome_var = "diagnosis"
  ), class = "simpleError")
  expect_error(vim_perm_sim_wrapper(
    alphas = c(0.3, 0.05, 0.15), nsims = c(10, 20, 30),
    entire_data = df, outcome_var = "diagnosis"
  ), class = "simpleError")
  expect_error(vim_perm_sim_wrapper(
    alphas = c(-0.3, 0.05, 0.15), nsims = c(10, 20, 30),
    entire_data = df, outcome_var = "diagnosis"
  ), class = "simpleError")
  expect_error(vim_perm_sim_wrapper(
    alphas = c(0.3, 0.15, 1.15), nsims = c(10, 20, 30),
    entire_data = df, outcome_var = "diagnosis"
  ), class = "simpleError")
  expect_error(
    vim_perm_sim_wrapper(
      alphas = c(0.3, 0.10, 0.05), nsims = c(10, 20, 30),
      entire_data = df, outcome_var = "diagnosis", save_vimp_history = "cat"
    ),
    class = "simpleError"
  )
  expect_error(
    vim_perm_sim_wrapper(
      alphas = c(0.3, 0.10, 0.05), nsims = c(10, 20, 30),
      entire_data = df, outcome_var = "diagnosis", to_show = "cat"
    ),
    class = "simpleError"
  )
  expect_error(
    vim_perm_sim_wrapper(
      alphas = c(0.3, 0.10, 0.05), nsims = c(10, 20, 30),
      entire_data = df, outcome_var = "diagnosis", method = "cat"
    ),
    class = "simpleError"
  )
})

# Warnings
test_that("vim_perm_sim_wrapper() throws a warning when no wariables survive pre-selection process.", {
  nonsense_df <- data.frame(
    diagnosis = c(rep(1, 50), rep(0, 50)),
    v1 = rep(45, 100), v2 = rep(55, 100), v3 = rep(65, 100)
  )

  # Capture all warnings
  warnings <- capture_warnings(
    vim_perm_sim_wrapper(
      alphas = c(0.3, 0.10, 0.05), nsims = c(10, 20, 30),
      entire_data = nonsense_df, outcome_var = "diagnosis"
    )
  )

  expect_equal(length(warnings) > 1, TRUE)
})

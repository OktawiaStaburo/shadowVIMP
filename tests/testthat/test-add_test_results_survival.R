test_that("alpha validation works", {
  toy <- list(event1 = data.frame(x1 = 1, x1_permuted = 1))

  expect_error(
    add_test_results_survival(toy, alpha = "0.05", init_num_vars = 1),
    "`alpha` must be numeric."
  )
  expect_error(
    add_test_results_survival(toy, alpha = c(0.01, 0.05), init_num_vars = 1),
    "`alpha` must be a single numeric between 0 and 1."
  )
  expect_error(
    add_test_results_survival(toy, alpha = -1, init_num_vars = 1),
    "`alpha` must be a single numeric between 0 and 1."
  )
  expect_error(
    add_test_results_survival(toy, alpha = 1.1, init_num_vars = 1),
    "`alpha` must be a single numeric between 0 and 1."
  )
})

test_that("computes pooled and per-variable quantiles correctly", {
  vim_df <- data.frame(
    x1 = c(-1, 0, 0, 0, 1),
    x2 = c(1, 2, 2, 2, 3),
    x1_permuted = c(-2, -1, 0, 1, 2),
    x2_permuted = c(-1, 0, 1, 2, 3)
  )
  vim <- list(event1 = vim_df)
  out <- add_test_results_survival(vim, alpha = 0.3, init_num_vars = 2, to_show = "FWER")

  # Structure
  expect_true("test_res_pooled" %in% names(out))
  expect_true("test_res_per_variable" %in% names(out))
  expect_s3_class(out$test_res_pooled$event1, "data.frame")
  expect_s3_class(out$test_res_per_variable$event1, "data.frame")
})

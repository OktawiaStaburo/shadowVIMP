# df, df_mod and df_mod2 defined in tests/testthat/helper.R
trees <- 5
# Typical usage
# By default vim_perm_sim() uses half of available CPU cores
# But to avoid any issues with parallel computing for tests we set num.threads to 1
# when testing on CRAN or running on CI environment
test_that("vim_perm_sim works as expected", {
  df1 <- df %>% select(diagnosis, v1, v2, v3)
  expect_length(vim_perm_sim(data = df1, outcome_var = "diagnosis", niters = 10,
                             num.threads = 1, num.trees = 10), n = 1)
})

test_that("vim_perm_sim works as expected in parallel mode", {
  skip_on_cran() # it's not advisable to test parallel code on CRAN
  skip_on_ci() # skipping parallel test on CI environment
  df1 <- df %>% select(diagnosis, v1, v2, v3)
  expect_length(vim_perm_sim(data = df1, outcome_var = "diagnosis", niters = 10,
                             num.trees = trees, num.threads = 4), n = 1)
})

# Check if malformed input results in a specific kind of error
test_that("vim_perm_sim fails when inappropiate inputs are passed", {
  expect_error(vim_perm_sim(data = as.vector(df), outcome_var = "diagnosis", niters = 10, num.trees = trees, num.threads = 1), class = "simpleError")
  expect_error(vim_perm_sim(data = as.list(df), outcome_var = "diagnosis", niters = 10, num.trees = trees, num.threads = 1), class = "simpleError")
  expect_error(vim_perm_sim(data = df, outcome_var = 45, niters = 10, num.threads = 1, num.trees = trees), class = "rlang_error")
  expect_error(vim_perm_sim(data = df, outcome_var = "dog", niters = 10, num.trees = trees, num.threads = 1), class = "rlang_error")
  expect_error(vim_perm_sim(data = df, outcome_var = TRUE, niters = 10, num.trees = trees, num.threads = 1), class = "rlang_error")
  expect_error(vim_perm_sim(data = df_mod, outcome_var = "diagnosis", num.trees = trees, niters = 10, num.threads = 1), class = "simpleError")
  expect_error(vim_perm_sim(data = df_mod2, outcome_var = "diagnosis", num.trees = trees, niters = 10, num.threads = 1), class = "simpleError")
  expect_error(vim_perm_sim(data = df, outcome_var = diagnosis, num.trees = trees, niters = "ten", num.threads = 1), class = "simpleError")
  expect_error(vim_perm_sim(data = df, outcome_var = diagnosis, num.trees = trees, niters = TRUE, num.threads = 1), class = "simpleError")
  expect_error(vim_perm_sim(data = df, outcome_var = diagnosis, niters = 10, num.trees = trees, num.threads = "ten", num.threads = 1), class = "simpleError")
  expect_error(vim_perm_sim(data = df, outcome_var = diagnosis, niters = 10, num.trees = trees, num.threads = 10^7), class = "simpleError")
  expect_error(vim_perm_sim(data = df, outcome_var = diagnosis, niters = 10, num.trees = trees, data_name = c(1:3), num.threads = 1), class = "simpleError")
})

test_that("vim_perm_sim throws a warning when inappropiate inputs are passed", {
  expect_warning(vim_perm_sim(data = df, outcome_var = "diagnosis", niters = c(1:3), num.trees = trees, num.threads = 1), class = "simpleWarning")
})


# test_that("vim_perm_sim gives expected output in parallel mode with specified number of threads", {
#   skip_on_cran()
#   skip_on_ci()
#   data <- data.frame(diag = rep(1, 50), v1 = rep(2, 50), v2 = rep(3, 50))
#   expect_snapshot(vim_perm_sim(data = data, outcome_var = "diag", niters = 10, num.threads = 4) %>% print())
# })

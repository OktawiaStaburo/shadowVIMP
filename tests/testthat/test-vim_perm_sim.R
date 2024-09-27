# Typical usage
test_that("vim_perm_sim works as expected", {

  df <- data.frame(diagnosis = c(rep(1, 50), rep(0, 50)))
  n_rows <- nrow(df)
  for (i in 1:20) {

    mean_val <- runif(1, min = 0, max = 10)
    sd_val <- runif(1, min = 1, max = 5)

    df[[paste0("v", i)]] <- rnorm(n_rows, mean = mean_val, sd = sd_val)
  }

  expect_length(vim_perm_sim(entire_data = df, outcome_var = "diagnosis", nsim = 10), n = 2)
  expect_length(vim_perm_sim(entire_data = df, outcome_var = "diagnosis", nsim = 10, num.threads = 4), n = 2)
  expect_length(vim_perm_sim(entire_data = df, outcome_var = "diagnosis", nsim = 10, num.threads = 2, num_cores_parallel = 3), n = 2)
  expect_length(vim_perm_sim(entire_data = df, outcome_var = "diagnosis", nsim = 10, num_cores_parallel = 6), n = 2)
})

# Check if malformed input results in a specific kind of error
test_that("vim_perm_sim throws an error when inappropiate inputs are passed", {
  df <- data.frame(diagnosis = c(rep(1, 50), rep(0, 50)))
  n_rows <- nrow(df)
  for (i in 1:20) {

    mean_val <- runif(1, min = 0, max = 10)
    sd_val <- runif(1, min = 1, max = 5)

    df[[paste0("v", i)]] <- rnorm(n_rows, mean = mean_val, sd = sd_val)
  }

  df_mod <- df %>% dplyr::rename(y = v1)
  df_mod2 <- df %>% dplyr::rename(v1_permuted = v1)

  expect_error(vim_perm_sim(entire_data = as.vector(df), outcome_var = "diagnosis", nsim = 10), class = "simpleError")
  expect_error(vim_perm_sim(entire_data = as.list(df), outcome_var = "diagnosis", nsim = 10), class = "simpleError")
  expect_error(vim_perm_sim(entire_data = df, outcome_var = diagnosis, nsim = 10), class = "simpleError")
  expect_error(vim_perm_sim(entire_data = df, outcome_var = 45, nsim = 10), class = "rlang_error")
  expect_error(vim_perm_sim(entire_data = df, outcome_var = "dog", nsim = 10), class = "rlang_error")
  expect_error(vim_perm_sim(entire_data = df, outcome_var = TRUE, nsim = 10), class = "rlang_error")
  expect_error(vim_perm_sim(entire_data =  df_mod, outcome_var = "diagnosis", nsim = 10), class = "simpleError")
  expect_error(vim_perm_sim(entire_data =  df_mod2, outcome_var = "diagnosis", nsim = 10), class = "simpleError")
  expect_error(vim_perm_sim(entire_data = df, outcome_var = diagnosis, nsim = "ten"), class = "simpleError")
  expect_error(vim_perm_sim(entire_data = df, outcome_var = diagnosis, nsim = TRUE), class = "simpleError")
  expect_error(vim_perm_sim(entire_data = df, outcome_var = diagnosis, nsim = 10, num.threads = "ten"), class = "simpleError")
  expect_error(vim_perm_sim(entire_data = df, outcome_var = diagnosis, nsim = 10, num.threads = 10^7), class = "simpleError")
  expect_error(vim_perm_sim(entire_data = df, outcome_var = diagnosis, nsim = 10, data_name = c(1:3)), class = "simpleError")
  expect_error(vim_perm_sim(entire_data = df, outcome_var = diagnosis, nsim = 10, num_cores_parallel = "dog"), class = "simpleError")
  expect_error(vim_perm_sim(entire_data = df, outcome_var = diagnosis, nsim = 10, num_cores_parallel = -10), class = "simpleError")
  expect_error(vim_perm_sim(entire_data = df, outcome_var = diagnosis, nsim = 10, num_cores_parallel = 10^7), class = "simpleError")
  expect_error(vim_perm_sim(entire_data = df, outcome_var = diagnosis, nsim = 10, num_cores_parallel = 35, num.threads = 30), class = "simpleError")

})

test_that("vim_perm_sim throws a warning when inappropiate inputs are passed", {
  df <- data.frame(diagnosis = c(rep(1, 50), rep(0, 50)))
  n_rows <- nrow(df)
  for (i in 1:20) {

    mean_val <- runif(1, min = 0, max = 10)
    sd_val <- runif(1, min = 1, max = 5)

    df[[paste0("v", i)]] <- rnorm(n_rows, mean = mean_val, sd = sd_val)
  }

  expect_warning(vim_perm_sim(entire_data = df, outcome_var = "diagnosis", nsim = c(1:3)), class =  "simpleWarning")
})

# I cannot add snapshot test in this form due the hour and date printed by the vim_perm_sim() function when tracking the progress
# test_that("Meaningless data give unimportant results", {
#   data <- data.frame(diag = rep(1, 50), v1 = rep(2, 50), v2 = rep(3, 50))
#   expect_snapshot(vim_perm_sim(entire_data = data, outcome_var = "diag", nsim = 10) %>% print())
# })

test_that("vim_perm_sim gives expected output in parallel mode", {
  data <- data.frame(diag = rep(1, 50), v1 = rep(2, 50), v2 = rep(3, 50))
  expect_snapshot(vim_perm_sim(entire_data = data, outcome_var = "diag", nsim = 10, num_cores_parallel = 2) %>% print())
})

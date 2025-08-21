test_that("input validation works correctly", {
  set.seed(1)
  df <- data.frame(x1 = rnorm(20), x2 = rnorm(20),
                   time = rexp(20), status = sample(c(0, 1, 2), 20, TRUE))
  expect_error(
    vim_perm_sim_survival(df, time_column = "time", status_column = "status",
                          niters = "ten"),
    regexp = "`niters` parameter must be numeric."
  )

  expect_error(
    vim_perm_sim_survival(df, time_column = 1, status_column = "status"),
    "`time_column` and  `status_column` must be characters of length 1."
  )
  expect_error(
    vim_perm_sim_survival(df, time_column = c("time","time"), status_column = "status"),
    "`time_column` and  `status_column` must be characters of length 1."
  )

  # variables ending with _permuted are rejected
  df2 <- df |> rename(x1_permuted = x1)
  expect_error(
    vim_perm_sim_survival(df2, time_column = "time", status_column = "status", niters = 2),
    "One or more variables ending with _permuted. Please rename them."
  )
})

test_that("checking output structure", {
  set.seed(2)
  df <- data.frame(x1 = rnorm(20),
                   x2 = rnorm(20),
                   x3 = sample(letters[1:3], 20, TRUE)|> as.factor(), # To check if factor ordering works
                   time = rexp(20),
                   status = sample(c(0, 1, 2), 20, TRUE))
  niters <- 2
  # I use suppressWarnings() in case some of covariates have VIMP == 0
  res <- suppressWarnings(vim_perm_sim_survival(data = df,
                               time_column = "time",
                               status_column = "status",
                               niters = niters,
                               num.trees = 10))

  # Output is a list
  expect_type(res, "list")

  # Number of elements in the output = number of competing events
  expect_true(length(res) == 2)
  # Each element of the output list is a data frame with niters rows
  lapply(res, function(elem) {
    expect_s3_class(elem, "data.frame")
    expect_equal(nrow(elem), niters)
  })

  # Each data frame has VIMP for original and shadow variables
  lapply(res, function(elem){
    expect_true(any(grepl("_permuted$", names(elem))))
    expect_true(any(!grepl("_permuted$", names(elem))))
    expect_true(ncol(elem) == 2*(ncol(df) - 2))
  })

  # Factor ordering works - TBD
})

test_that("reproducibility: same seed + same inputs => identical results", {
  df <- data.frame(
    x1 = rnorm(20),
    x2 = rnorm(20),
    time = rexp(20),
    status = sample(c(0,1,2), 20, TRUE)
  )

  set.seed(777)
  a <- suppressWarnings(
    vim_perm_sim_survival(df, "time", "status", niters = 2,
                             importance = "permute", num.trees = 10)
  )
  set.seed(777)
  b <- suppressWarnings(
    vim_perm_sim_survival(df, "time", "status", niters = 2,
                             importance = "permute", num.trees = 10)
  )

  expect_equal(a, b, tolerance = 1e-8)

})

# df defined in helper.R
# We use suppressWarnings() because in the below tests we use too small number
# of niters to get reliable results so shadow_vimp() is giving a warning
# Here it's more importnat to run tests fast, not to get statistical reliable results

# test print function for pooled approach
test_that("print.shadow_vimp prints correct output using pooled approach", {
  out <- suppressWarnings(shadow_vimp(
    alphas = c(0.3, 0.10, 0.05), niters = c(10, 15, 20),
    data = df, outcome_var = "diagnosis", num.threads = 1
  ))

  expect_s3_class(out, class = "shadow_vimp")

  output <- capture.output(out)
  expect_true(any(grepl("shadowVIMP Result", output)))
  expect_true(any(grepl("Call:", output)))
  expect_true(any(grepl(" Step Alpha Retained Covariates", output)))


  step_lines <- output[ grep("^ Step [0-9]+", output) ]
  # Convert selected lines into df:
  df_raw <- read.table(
    text = step_lines,
    stringsAsFactors = FALSE,
    header = FALSE
  )
  # Merge together first 2 columns (so that df corresponds to the output of print function):
  df <- data.frame(
    Step = paste0(df_raw$V1, " ", df_raw$V2),
    Alpha = df_raw$V3,
    Retained_Covariates = df_raw$V4,
    stringsAsFactors = FALSE
  )
  # Results for three steps are reported:
  expect_equal(df$Step, c("Step 1", "Step 2", "Step 3"))
  # Alpha values printed are exactly the same as those specified in the shadow_vimp() function:
  expect_equal(df$Alpha,  c(0.30, 0.10, 0.05))

  expect_true(any(grepl("Count of significant covariates from step 3 per p-value correction method using the pooled approach",
                        output)))
  expect_true(any(grepl(" Type-1 Confirmed FDR Confirmed FWER Confirmed", output)))
})

# Test for the per-variable approach
test_that("print.shadow_vimp prints correct output using per-variable approach", {
  out <- suppressWarnings(shadow_vimp(
    alphas = c(0.3, 0.10, 0.05), niters = c(10, 15, 20),
    data = df, outcome_var = "diagnosis", num.threads = 1,
    method = "per_variable"
  ))

  expect_s3_class(out, class = "shadow_vimp")

  output <- capture.output(out)
  expect_true(any(grepl("shadowVIMP Result", output)))
  expect_true(any(grepl("Call:", output)))
  expect_true(any(grepl(" Step Alpha Retained Covariates", output)))


  step_lines <- output[ grep("^ Step [0-9]+", output) ]
  # Convert selected lines into df:
  df_raw <- read.table(
    text = step_lines,
    stringsAsFactors = FALSE,
    header = FALSE
  )
  # Merge together first 2 columns (so that df corresponds to the output of print function):
  df <- data.frame(
    Step = paste0(df_raw$V1, " ", df_raw$V2),
    Alpha = df_raw$V3,
    Retained_Covariates = df_raw$V4,
    stringsAsFactors = FALSE
  )
  # Results for three steps are reported:
  expect_equal(df$Step, c("Step 1", "Step 2", "Step 3"))
  # Alpha values printed are exactly the same as those specified in the shadow_vimp() function:
  expect_equal(df$Alpha,  c(0.30, 0.10, 0.05))

  expect_true(any(grepl("Count of significant covariates from step 3 per p-value correction method using the per variable approach",
                        output)))
  expect_true(any(grepl(" Type-1 Confirmed FDR Confirmed FWER Confirmed", output)))
})


# test print function with arbitrary number of steps
test_that("print.shadow_vimp prints correct output when using arbitrary number of steps", {
  out1 <- suppressWarnings(shadow_vimp(
    alphas = c(0.3), niters = c(10),
    data = df, outcome_var = "diagnosis", num.threads = 1
  ))

  out2 <- suppressWarnings(shadow_vimp(
    alphas = c(0.3, 0.1), niters = c(10, 20),
    data = df, outcome_var = "diagnosis", num.threads = 1
  ))

  expect_s3_class(out1, class = "shadow_vimp")
  expect_s3_class(out2, class = "shadow_vimp")

  output1 <- capture.output(out1)
  output2 <- capture.output(out2)

  # 1 step:
  step_lines1 <- output1[grep("^ Step [0-9]+", output1)]
  # Convert selected lines into df:
  df_raw1 <- read.table(
    text         = step_lines1,
    stringsAsFactors = FALSE,
    header       = FALSE
  )
  # Merge together first 2 columns (so that df corresponds to the output of print function):
  df1 <- data.frame(
    Step = paste0(df_raw1$V1, " ", df_raw1$V2),
    Alpha = df_raw1$V3,
    Retained_Covariates = df_raw1$V4,
    stringsAsFactors = FALSE
  )

  expect_equal(df1$Step, c("Step 1"))
  expect_equal(df1$Alpha,  c(0.30))

  # 2 steps:
  step_lines2 <- output2[grep("^ Step [0-9]+", output2)]
  # Convert selected lines into df:
  df_raw2 <- read.table(
    text         = step_lines2,
    stringsAsFactors = FALSE,
    header       = FALSE
  )
  # Merge together first 2 columns (so that df corresponds to the output of print function):
  df2 <- data.frame(
    Step = paste0(df_raw2$V1, " ", df_raw2$V2),
    Alpha = df_raw2$V3,
    Retained_Covariates = df_raw2$V4,
    stringsAsFactors = FALSE
  )

  expect_equal(df2$Step, c("Step 1", "Step 2"))
  expect_equal(df2$Alpha,  c(0.30, 0.10))


})


# Testing different settings of to_show parameter
test_that("print.shadow_vimp gives correct output when `to_show` is set to `FDR` or `unadjusted`", {
  out1 <- suppressWarnings(shadow_vimp(
    alphas = c(0.3), niters = c(10),
    data = df, outcome_var = "diagnosis", num.threads = 1,
    to_show = "FDR"
  ))

  out2 <- suppressWarnings(shadow_vimp(
    alphas = c(0.3, 0.1), niters = c(10, 20),
    data = df, outcome_var = "diagnosis", num.threads = 1,
    to_show = "unadjusted"
  ))

  expect_s3_class(out1, class = "shadow_vimp")
  expect_s3_class(out2, class = "shadow_vimp")

  output1 <- capture.output(out1)
  output2 <- capture.output(out2)

  expect_true(any(grepl(" Type-1 Confirmed FDR Confirmed", output1)))
  expect_true(any(grepl(" Type-1 Confirmed", output2)))

})





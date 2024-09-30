# fake_data_vps data defined in tests/testthat/helper.R

test_that("add_test_results works as expected", {
  output <- add_test_results(vimpermsim = fake_data_vps, alpha = 0.1, init_num_vars = 5, to_show = "FWER")
  expect_length(output, n = 3)
  expect_s3_class(output$vim_simulated, "data.frame")
  expect_length(output$test_results, n = 2)
  expect_s3_class(output$test_results[[1]], "data.frame")
  expect_s3_class(output$test_results[[2]], "data.frame")

})

# Warnings
test_that("add_test_results throws a warning when `to_show` is set to `FDR` or `unadjusted`", {
  expect_warning(add_test_results(vimpermsim = fake_data_vps, alpha = 0.1, init_num_vars = 5, to_show = "FDR"), class = "simpleWarning")
  expect_warning(add_test_results(vimpermsim = fake_data_vps, alpha = 0.1, init_num_vars = 5, to_show = "unadjusted"), class = "simpleWarning")
})

# Check if malformed input results in a specific kind of error
test_that("add_test_results fails when inappropiate inputs are passed", {
  empty_list <- list(vim_simulated = data.frame(), controls = list(nsim = c()))

  fake_dat_2 <- fake_data_vps
  fake_dat_2$vim_simulated <- fake_dat_2$vim_simulated %>% select(-ends_with("_permuted"))

  fake_dat_3 <- fake_data_vps
  fake_dat_3$vim_simulated <- fake_dat_3$vim_simulated %>% select(ends_with("_permuted"))

  expect_error(add_test_results(vimpermsim = fake_data_vps$vim_simulated, alpha = 0.1, init_num_vars = 5), class = "simpleError")
  expect_error(add_test_results(vimpermsim = empty_list, alpha = 0.1, init_num_vars = 5), class = "rlang_error")
  expect_error(add_test_results(vimpermsim = fake_dat_2, alpha = 0.1, init_num_vars = 5), class = "subscriptOutOfBoundsError")
  expect_error(add_test_results(vimpermsim = fake_dat_3, alpha = 0.1, init_num_vars = 5), class = "rlang_error")
  expect_error(add_test_results(vimpermsim = fake_data_vps, alpha = 10, init_num_vars = 5), class = "simpleError")
  expect_error(add_test_results(vimpermsim = fake_data_vps, alpha = -10, init_num_vars = 5), class = "simpleError")
  expect_error(add_test_results(vimpermsim = fake_data_vps, alpha = c(1:10), init_num_vars = 5), class = "simpleError")
  expect_error(add_test_results(vimpermsim = fake_data_vps, alpha = "0.1", init_num_vars = 5), class = "simpleError")
  expect_error(add_test_results(vimpermsim = fake_data_vps, alpha = 0.1, init_num_vars = 5, to_show = "cat"), class = "simpleError")
})

# Scenarios to test:
# what will happen if some variable in the data is character/ categorical coded with character
# run tests coverage -- come up with tests that you should further perform

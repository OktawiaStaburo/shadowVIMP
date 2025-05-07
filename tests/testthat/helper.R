# Define objects used repetitively in multiple tests
# Data used in test-vim_perm_sim.R and test-shadow_vimp.R
df <- data.frame(diagnosis = c(rep(1, 50), rep(0, 50)))
n_rows <- nrow(df)
for (i in 1:10) {
  mean_val <- stats::runif(1, min = 0, max = 10)
  sd_val <- stats::runif(1, min = 1, max = 5)

  df[[paste0("v", i)]] <- stats::rnorm(n_rows, mean = mean_val, sd = sd_val)
}

for (i in 1:10) {
  df[[paste0("v", i + 10)]] <- c(rep(i, 50), rep(i + 1, 50))
}

df_mod <- df %>% dplyr::rename(y = v1)
df_mod2 <- df %>% dplyr::rename(v1_permuted = v1)
df_const <- df %>% mutate(dummy = rep(33, 100))

# Data used in test-add_test_results.R
var_names <- paste0("var", c(1:5))
var_permuted <- paste0(var_names, "_permuted")
fake_vimp <- stats::rnorm(50, mean = 2, sd = 4)

fake_vimp_df <- matrix(fake_vimp, nrow = 5, ncol = 10) %>%
  as.data.frame()
colnames(fake_vimp_df) <- c(var_names, var_permuted)

fake_data_vps <- list(
  vim_simulated = fake_vimp_df
)

# Data used in test-plot_vimps.R
df_large <- data.frame(diagnosis = c(rep(0, 50), rep(1, 50)))
for (i in 1:50) {
  df_large[[paste0("v", i)]] <- c(rep(i, 50), rep(i + 1, 50))
}

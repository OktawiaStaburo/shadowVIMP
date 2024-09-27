# Define objects repetitively used in multiple tests
df <- data.frame(diagnosis = c(rep(1, 50), rep(0, 50)))
n_rows <- nrow(df)
for (i in 1:20) {

  mean_val <- runif(1, min = 0, max = 10)
  sd_val <- runif(1, min = 1, max = 5)

  df[[paste0("v", i)]] <- rnorm(n_rows, mean = mean_val, sd = sd_val)
}

df_mod <- df %>% dplyr::rename(y = v1)
df_mod2 <- df %>% dplyr::rename(v1_permuted = v1)

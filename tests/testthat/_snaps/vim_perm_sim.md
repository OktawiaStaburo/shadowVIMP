# vim_perm_sim gives expected output in parallel mode with specified number of threads

    Code
      vim_perm_sim(data = data, outcome_var = "diag", niters = 10, num.threads = 4) %>%
        print()
    Output
      2025-05-07 11:58:44: dataframe = data niters = 10 num.trees = 10000. Running step 1
    Condition
      Warning in `vim_perm_sim()`:
      One or more shadow variables always have VIMP equal to zero.
    Output
      $vim_simulated
         v1 v2 v1_permuted v2_permuted
      1   0  0           0           0
      2   0  0           0           0
      3   0  0           0           0
      4   0  0           0           0
      5   0  0           0           0
      6   0  0           0           0
      7   0  0           0           0
      8   0  0           0           0
      9   0  0           0           0
      10  0  0           0           0
      


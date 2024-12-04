# vim_perm_sim gives expected output in parallel mode

    Code
      vim_perm_sim(entire_data = data, outcome_var = "diag", nsim = 10,
        num_cores_parallel = 2) %>% print()
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
      


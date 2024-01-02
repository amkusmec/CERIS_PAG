### env_mean_trait = data frame of environment-wise mean phenotypes
### env_paras      = data frame of environmental covariates
### max_days       = maximum number of days after planting for searching
### Paras          = names of environmental covariates
CERIS <- function(env_mean_trait, env_paras, Paras, max_days = NULL) {
  if (is.null(max_days)) max_days <- max(env_paras$DAP)
  dap_x <- dap_y <- max_days
  nParas <- length(Paras)
  
  # Flag for leave-one-environment-out cross-validation (off by default)
  LOO <- 0
  
  # Population code -- there is no indication of what this is for and no way
  # to change it or impact on the analysis code
  p <- 1
  
  # Setup storage for search
  # dap_win <- max_days*max_days/2
  dap_win <- sum(seq(length(1:(dap_y - 6)), 1, -1))
  pop_cors_matrix <- matrix(ncol = 5 + (2*nParas), nrow = dap_win*1)
  pop_corP_matrix <- matrix(ncol = 5 + (1*nParas), nrow = dap_win*1)
  colnames(pop_cors_matrix) <- c("pop_code", 'Day_x', 'Day_y', 'window', 'midXY', 
                                 paste0('R_', Paras), paste0('P_', Paras))
  
  # Point to pop_cors_matrix
  n <- 0
  
  ### Main search loop
  # Minimum window size of 7 days
  for (d1 in 1:(dap_y - 6)) {
    for (d2 in (d1 + 6):dap_y) {
      n <- n + 1
      days <- c(d1:d2)
      
      # Compile average covariate values for the window
      env_facts_matrix <- matrix(nrow = nrow(env_mean_trait), ncol = nParas)
      for (e_i in 1:nrow(env_mean_trait)) {
        e <- env_mean_trait$env_code[e_i]
        env_facts_matrix[e_i, ] <- colMeans(subset(env_paras, env_code == e)[d1:d2, -(1:2)])
      }
      Ymean_envPara <- cbind(env_facts_matrix, env_mean_trait$meanY)
      
      rs <- ps <- c()
      if (LOO == 0) {
        # No cross-validation
        for (k in 1:nParas) {
          c_test <- cor.test(Ymean_envPara[, nParas + 1], Ymean_envPara[, k], 
                             use = "complete.obs")
          rs[k] <- round(c_test$estimate, digits = 4)
          ps[k] <- round(-log10(c_test$p.value), digits = 4)
        }
      } else {
        # Cross-validation
        loo_rs_matrix <- matrix(nrow = nrow(Ymean_envPara) + 0, ncol = nParas)
        loo_ps_matrix <- matrix(nrow = nrow(Ymean_envPara) + 0, ncol = nParas)
        
        for (k in 1:nParas) {
          for (e_x in c(1:nrow(Ymean_envPara))) {
            c_test <- cor.test(Ymean_envPara[-e_x, nParas + 1], 
                               Ymean_envPara[-e_x, k], use = "complete.obs")
            loo_rs_matrix[e_x, k] <- c_test$estimate
            loo_ps_matrix[e_x, k] <- -log10(c_test$p.value)
          }
        }
        
        rs <- round(apply(loo_rs_matrix, 2, median), digits = 4)
        ps <- round(apply(loo_ps_matrix, 2, median), digits = 4)
      }
      
      pop_cors_matrix[n, ] <- c(p, d1, d2, d2 - d1, (d2 + d1)/2, rs, ps)
      
      # Search progress bar
      if (n %% 100 == 0) {
        p_complete <- n/dap_win
        n_bars <- p_complete %/% 0.02
        
        cat("[", paste(rep("=", n_bars), collapse = ""), 
            paste(rep(" ", 50 - n_bars), collapse = ""), 
            "] ", round(100*p_complete, digits = 2), "%\r", 
            sep = "")
      }
    } # d2 loop
  } # d1 loop
  
  cat(rep(" ", 80), "\r", sep = "")
  cat("[", rep("=", 50), "] 100%", sep = "")
  
  return(pop_cors_matrix)
}

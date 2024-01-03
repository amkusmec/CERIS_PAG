oneTo3CV <- function(gFold, gIteration, SNPs, lm_ab_matrix, env_mean_trait, 
                     exp_trait) {
  line_codes <- lm_ab_matrix[, "line_code"]
  total_s <- length(line_codes)
  
  # Create the CV folds
  block_idx <- floor(seq(1, total_s, length = gFold + 1))
  if (block_idx[length(block_idx)] < total_s) {
    block_idx[length(block_idx)] <- total_s
  }
  
  lm_ab_matrix <- as.data.frame(lm_ab_matrix[, c(1, 3:4)])
  for (i in 2:3) lm_ab_matrix[[i]] <- as.numeric(lm_ab_matrix[[i]])
  
  for (n in 1:gIteration) {
    # Shuffle the lines
    env_idx <- sample(1:total_s, total_s)
    
    for (bi in 1:(length(block_idx) - 1)) {
      cat("Iteration [", n, "/", gIteration, "]; Fold [", bi, "/", gFold, "]\r", 
          sep = "")
      
      # Identify the fold indices
      block_s <- block_idx[bi]
      block_e <- block_idx[bi + 1] - 1
      if (bi == (length(block_idx) - 1)) {
        block_e <- block_idx[bi + 1]
      }
      
      prd_idx <- sort(env_idx[block_s:block_e])
      
      # Predict slope and intercept
      ab_prd <- predRRBlup(lm_ab_matrix, SNPs, prd_idx, n)
      
      # Predict the phenotype
      for (e_i in 1:nrow(env_mean_trait)) {
        Y_prd <- round(ab_prd$Intcp_para_prd + 
                         ab_prd$Slope_para_prd*env_mean_trait$kPara[e_i], digits = 3)
        prd_result <- data.frame(line_code = ab_prd$ID_code, 
                                 env_code = rep(env_mean_trait$env_code[e_i]), 
                                 Yprd = Y_prd)
        prd_result <- merge(prd_result, exp_trait, all.x = TRUE)
        prd_result$Rep <- rep(n, nrow(ab_prd))
        if (e_i == 1 & n == 1 & bi == 1) { 
          PrefPred_df <- prd_result
        } else { 
          PrefPred_df <- rbind(PrefPred_df, prd_result) 
        }
      }
      
      cat(rep(" ", 80), "\r", sep = "")
    }
  }
  
  PrefPred_df <- PrefPred_df[!is.na(PrefPred_df$Yobs), ]
  return(PrefPred_df)
}

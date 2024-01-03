oneTo4CV <- function(gFold, gIteration, SNPs, env_mean_trait, exp_trait) {
  
  for (e_i in 1:nrow(env_mean_trait)) {
    # Drop a single environment and recalculate the reaction norm parameters
    meanY_kPara_Loo <- env_mean_trait[-e_i, ] 
    exp_trait_Loo <- subset(exp_trait, env_code != env_mean_trait$env_code[e_i])
    
    lm_ab_matrix <- slopeIntercept(exp_trait_Loo, meanY_kPara_Loo)
    lm_ab_matrix <- as.data.frame(lm_ab_matrix[, c("line_code", "Intcp_para", "Slope_para")])
    for (i in 2:3) lm_ab_matrix[[i]] <- as.numeric(lm_ab_matrix[[i]])
    
    # Create the CV folds
    block_idx <- floor(seq(1, nrow(lm_ab_matrix), length = gFold + 1))
    if (block_idx[length(block_idx)] < nrow(lm_ab_matrix)) {
      block_idx[length(block_idx)] <- nrow(lm_ab_matrix)
    }
    
    for (n in 1:gIteration) {
      # Shuffle the lines
      env_idx <- sample(1:nrow(lm_ab_matrix), nrow(lm_ab_matrix))
      
      for (bi in 1:(length(block_idx) - 1)) {
        cat("Environment [", e_i, "/", nrow(env_mean_trait), "]; Iteration [", 
            n, "/", gIteration, "]; Fold [", bi, "/", gFold, "]\r", sep = "")
        
        # Identify the fold indices
        block_s <- block_idx[bi]
        block_e <- block_idx[bi + 1] - 1
        if (bi == (length(block_idx) - 1)) {
          block_e <- block_idx[bi + 1]
        }
        
        # Predict slope and intercept
        prd_idx <- sort(env_idx[block_s:block_e])
        ab_prd <- predRRBlup(lm_ab_matrix, SNPs, prd_idx, n)
        
        # Predict the phenotype
        Y_prd <- round(ab_prd$Intcp_para_prd + 
                         ab_prd$Slope_para_prd*env_mean_trait$kPara[e_i], 
                       digits = 3)
        prd_result <- data.frame(line_code = ab_prd$ID_code, 
                                 env_code = rep(env_mean_trait$env_code[e_i]), 
                                 Yprd = Y_prd)
        prd_result <- merge(prd_result, exp_trait)
        prd_result$Rep <- rep(n, nrow(prd_result))
        
        if (e_i == 1 & n == 1 & bi == 1) { 
          PrefPred_df <- prd_result
        } else { 
          PrefPred_df <- rbind(PrefPred_df, prd_result)
        }
      }
    }
    
    cat(rep(" ", 80), "\r", sep = "")
  }
  
  PrefPred_df <- PrefPred_df[!is.na(PrefPred_df$Yobs), ]
  return(PrefPred_df)
}

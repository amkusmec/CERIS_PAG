oneTo2CV <- function(env_mean_trait, exp_trait) {
  
  for (e_i in 1:nrow(env_mean_trait)) {
    # Drop a single environment and recalculate the reaction norm parameters
    meanY_kPara_Loo <- env_mean_trait[-e_i, ] 
    exp_trait_Loo <- subset(exp_trait, env_code != env_mean_trait$env_code[e_i])
    
    lm_ab_matrix <- slopeIntercept(exp_trait_Loo, meanY_kPara_Loo)
    lm_ab_matrix <- as.data.frame(lm_ab_matrix[, c("line_code", "Intcp_para", "Slope_para")])
    for (i in 2:3) lm_ab_matrix[[i]] <- as.numeric(lm_ab_matrix[[i]])
    
    # Predict the phenotype
    Y_prd <- round(lm_ab_matrix$Intcp_para + 
                     lm_ab_matrix$Slope_para*env_mean_trait$kPara[e_i], 
                   digits = 3)
    prd_result <- data.frame(line_code = lm_ab_matrix$line_code, 
                             env_code = rep(env_mean_trait$env_code[e_i]), 
                             Yprd = Y_prd)
    prd_result <- merge(prd_result, exp_trait)
    prd_result$Rep <- rep(1, nrow(prd_result)) # No random sampling = no iterations
    
    if (e_i == 1) { 
      PrefPred_df <- prd_result
    } else { 
      PrefPred_df <- rbind(PrefPred_df, prd_result)
    }
  }
  
  PrefPred_df <- PrefPred_df[!is.na(PrefPred_df$Yobs), ]
  return(PrefPred_df)
}

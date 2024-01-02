slopeIntercept <- function(exp_trait, env_mean_trait) {
  line_codes <- unique(exp_trait$line_code)
  exp_trait_kPara <- merge(exp_trait, env_mean_trait[, c(1:2, 8)], 
                           by = "env_code")
  lm_ab_matrix <- matrix(ncol = 5, nrow = length(line_codes))
  
  for (l in seq_along(line_codes)) {
    l_trait <- subset(exp_trait_kPara, line_code == line_codes[l])
    
    lm_Para <- lm(Yobs ~ kPara, data = l_trait)
    
    # Adjusted by the population mean
    a_Para <- drop(round(predict(lm_Para, 
                                 data.frame(kPara = mean(env_mean_trait$kPara))), 
                         digits = 4))
    a_Para_ori <- round(coef(lm_Para)[1], digits = 4)
    b_Para <- round(coef(lm_Para)[2], digits = 4)
    R_Para <- round(summary(lm_Para)$r.squared, digits = 2)
    
    lm_ab_matrix[l, ] <- c(line_codes[l], a_Para, a_Para_ori, b_Para, R_Para)
  }
  
  lm_ab_matrix <- lm_ab_matrix[!is.na(lm_ab_matrix[, 2]), ]
  colnames(lm_ab_matrix) <- c('line_code', 'Intcp_para_adj', 'Intcp_para', 
                              'Slope_para', 'R2_para')
  
  return(lm_ab_matrix)
} 

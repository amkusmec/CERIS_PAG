FW_Model <- function(line_by_env_df, env_mean_trait) {
  ## line_code, a, a_meanEnv, b_mean, r2
  line_codes <- unique(line_by_env_df$line_code)
  lm_ab_matrix <- matrix(ncol = 5, nrow = length(line_codes))
  
  for (i in 1:nrow(line_by_env_df)) {
    df3 <- data.frame(meanY = env_mean_trait$meanY, Yobs = as.numeric(line_by_env_df[i, -1]));
    df3 <- df3[!is.na(df3$Yobs),];
    if(nrow(df3) >= 4) {
      lm_ab <- lm(Yobs ~ meanY, data = df3)
      a_Mean <- as.vector(round(predict(lm_ab, data.frame(meanY = mean(env_mean_trait$meanY))), 4)); ## adjusted by the population mean
      b_Mean <- as.vector(round(lm_ab$coefficient[2], 4))
      R_Mean <- round(summary(lm_ab)$r.squared, 4)
      lm_ab_matrix[i,] <- c(line_by_env_df[i, 1], round(coef(lm_ab)[1], 4), 
                            a_Mean, b_Mean, R_Mean)
    }
  }
  
  lm_ab_matrix <- lm_ab_matrix[!is.na(lm_ab_matrix[,2]),]
  colnames(lm_ab_matrix) <- c('line_code', 'Intcp', 'Intcp_mean', 
                              'Slope_mean','R2_mean')
  
  return(lm_ab_matrix)
}

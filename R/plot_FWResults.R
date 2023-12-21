plot_FWResults <- function(env_mean_trait, line_by_env_df, lm_ab_matrix, trait) {
  layout(matrix(1:2, ncol = 2))
  
  ### Plot of FW regression lines
  par(mar = c(2.0, 2.0, 1, 0.5) , mgp = c(1, 0.1, 0), tck = -0.01, cex.axis = .7, family = "mono");
  plot(0, 0, col = "white", xlim = range(env_mean_trait$meanY), 
       ylim = range(exp_trait$Yobs, na.rm = T), cex.lab = .9,  
       ylab = trait,  xlab = 'Environmental mean', fg = "gray50")
  for (i in 1:nrow(line_by_env_df)) {
    df3 <- data.frame(meanY = env_mean_trait$meanY, 
                      Yobs = as.numeric(line_by_env_df[i, -1]))
    df3 <- df3[!is.na(df3$Yobs), ]
    points(df3$meanY, df3$Yobs, col = gray_alpha,  pch = 19, cex = 0.3)
  }
  
  for (i in 1:nrow(lm_ab_matrix)) {
    abline(coef = lm_ab_matrix[i, c(2, 4)], col = gray_alpha)
  }
  
  mtext('A', side = 3, at = min(env_mean_trait$meanY)) 
  
  ### Plot of FW R^2
  par(mar = c(2.0, 2.0, 1, 0.5) , mgp = c(1, 0.1, 0), tck = -0.01, cex.axis = .7, family = "mono")
  hist(as.numeric(lm_ab_matrix[, 5]), xlab = expression('F-W R^2'), 
       ylab = 'Count', main = '')
  mtext('B', side = 3, at = min(lm_ab_matrix[, 4])) 
}

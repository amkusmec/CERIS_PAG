plot_slopeIntercept <- function(exp_trait, res_para, trait, kPara_Name) {
  layout(matrix(1:2, ncol = 2))
  
  ### Plot of FW regression lines
  par(mar = c(2.0, 2.0, 1, 0.5) , mgp = c(1, 0.1, 0), tck = -0.01, 
      cex.axis = 0.7, family = "mono")
  plot(0, 0, col = "white", xlim = range(exp_trait$kPara), 
       ylim = range(exp_trait$Yobs, na.rm = T), cex.lab = 0.9,  
       ylab = trait,  xlab = kPara_Name, fg = "gray50")
  points(exp_trait$kPara, exp_trait$Yobs, col = gray_alpha,  pch = 19, cex = 0.3)
  
  for (i in 1:nrow(lm_ab_matrix)) {
    abline(coef = as.numeric(lm_ab_matrix[i, c(3, 4)]), col = gray_alpha)
  }
  
  mtext('A', side = 3, at = min(exp_trait$kPara)) 
  
  ### Plot of parameter R^2
  par(mar = c(2.0, 2.0, 1, 0.5) , mgp = c(1, 0.1, 0), tck = -0.01, 
      cex.axis = 0.7, family = "mono")
  hist(as.numeric(lm_ab_matrix[, 5]), xlab = expression(paste("Parameter ", R^2)), 
       ylab = 'Count', main = '')
  mtext('B', side = 3, at = min(lm_ab_matrix[, 4])) 
}

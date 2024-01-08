plot_traitMean_kPara <- function(env_mean_trait, trait, kPara_name, 
                                 maxR_dap1, maxR_dap2) {
  op <- par(mar = c(2.5, 2.0, 1, 0.5) , mgp = c(0.7, 0.01, 0), 
      tck = -0.01, family = "mono")
  plot(env_mean_trait$kPara, env_mean_trait$meanY, 
       xlab = paste0(kPara_name, ' (', maxR_dap1, ' to ', maxR_dap2, ' DAP)'), 
       ylab = paste(trait, 'mean'),  pch = 19, col = env_cols, 
       cex.lab = 1.2)
  abline(lm(meanY ~ kPara, data = env_mean_trait), lty = 2)
  r1 <- round(cor(env_mean_trait$meanY, env_mean_trait$kPara), 3)
  legend("bottom", paste0('r = ', r1), bty = "n", cex = 1.2)
  
  # par(op)
}

plot_envMeans <- function(env_mean_trait, line_by_env_df, trait) {
  x_tick <- diff(env_mean_trait[, 2]) / 50
  
  op <- par(mar = c(2.0, 2.0, 1, 0.5) , mgp = c(1, 0.1, 0), tck = -0.01, cex.axis = 1, cex.lab = 1.2, family = "mono")
  plot(0, 0, col = "white", xlim = range(env_mean_trait$meanY), ylim = range(exp_trait$Yobs, na.rm = T), cex.lab = 0.9,  ylab = trait,  xlab = 'Environmental mean', fg = "gray50")
  
  # Plot the data for each line
  for (i in 1:nrow(line_by_env_df)) {
    df3 <- data.frame(meanY = env_mean_trait$meanY, Yobs = as.numeric(line_by_env_df[i, -1]))
    df3 <- df3[!is.na(df3$Yobs), ]
    points(df3$meanY, df3$Yobs, col = gray_alpha, type = "l", pch = 19, lwd = 0.3)
    points(df3$meanY, df3$Yobs, col = gray_alpha,  pch = 19, cex = 0.3)
  }
  
  abline(a = 0, b = 1, lty = 2, col = "grey")
  points(env_mean_trait$meanY, env_mean_trait$meanY, col = "black", cex = 0.8, pch = 19)
  # legend("topleft", as.vector(env_mean_trait$env_code), col = env_cols[match(as.vector(env_mean_trait$env_code), all_env_codes )], pch = 19, bty = "n", cex = .4)
  
  # par(op)
}

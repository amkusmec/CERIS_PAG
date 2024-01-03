plotCVResults <- function(cv_res, all_env_codes) {
  layout(matrix(seq_along(cv_res), nrow = 1))
  
  for (i in seq_along(cv_res)) {
    # Use only the first replicate
    CVs <- cv_res[[i]]
    if (max(CVs$Rep) > 1) CVs <- subset(CVs, CVs$Rep == 1)
    
    xy_lim <- range(CVs$Yprd, CVs$Yobs, na.rm = T)
    
    op <- par(mar = c(2.5, 2.0, 1, 0.5), mgp = c(0.7, 0.01, 0), tck = -0.01, 
        family = "mono")
    plot(CVs$Yprd, CVs$Yobs, col = env_cols[match(CVs$env_code, all_env_codes)], 
         xlab = 'Predicted', ylab = 'Observed', pch = 19, xlim = xy_lim, 
         ylim = xy_lim)
    abline(a = 0, b = 1, col = "black", lty = 2, lwd = 1.5)
    
    r1 <- round(cor(CVs$Yprd, CVs$Yobs, use = "complete.obs"), digits = 2)
    legend("bottom", legend = substitute(paste(italic('r'), " = ", R1), list(R1 = r1)), bty = "n")
    legend("topleft", paste('1 to ', i + 2, ' prediction', sep = ''), bty = "n")
  }
  
  par(op)
  layout(matrix(1))
}

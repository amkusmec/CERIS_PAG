plot_CERIS <- function(pop_cors_matrix, Paras, max_days) {
  dap_x <- dap_y <- max_days
  nParas <- length(Paras)
  
  # Population code -- there is no indication of what this is for and no way
  # to change it or impact on the analysis code
  p <- 1
  
  ### For each covariate, group by window midpoint and identify the window
  ### with the greatest -log10 p-value (WHY?)
  corPs <- as.data.frame(pop_cors_matrix)
  Para_idx_p <- match(paste0('P_', Paras), colnames(corPs))
  Para_idx_r <- match(paste0('R_', Paras), colnames(corPs))
  
  maxP_Paras_idx <- aggregate(corPs[, Para_idx_p], list(corPs$midXY), which.max)
  colnames(maxP_Paras_idx)[1] <- 'midXY'
  
  maxPR_m <- matrix(ncol = 6, nrow = nrow(maxP_Paras_idx)*nParas)
  
  m <- 1
  for (k in 1:nrow(maxP_Paras_idx)) {
    mid_xy <- maxP_Paras_idx[k, 1]
    
    for (h in 1:nParas) {
      max_rp <- subset(corPs, corPs$midXY == mid_xy)[maxP_Paras_idx[k, h + 1], ]
      max_r <- max_rp[1, Para_idx_r[h]]
      max_p <- max_rp[1, Para_idx_p[h]]
      maxPR_m[m, ] <- c(h, max_rp$Day_x[1], max_rp$Day_y[1], mid_xy, max_r, max_p)
      m <- m + 1
    }
    
  }
  maxPR_m[, 1] <- Paras[maxPR_m[, 1]]
  colnames(maxPR_m) <- c('Para', 'Day_x', 'Day_y', 'midXY', 'R', 'P')
  
  
  pop_cors <- subset(as.data.frame(pop_cors_matrix), pop_code == p)

  layout(rbind(matrix(c(1:nParas), 1, nParas, byrow = T), 
               rep(nParas + 1, nParas), rep(nParas + 2, nParas)))
  
  # Plots heatmaps of the correlations for each covariate
  for (k in 1:nParas) {
    # Collect a single correlation column
    pop_cor_0 <- subset(pop_cors, pop_cors$pop_code == p) # Redundant
    pop_cor <- pop_cor_0[, c(1:4, k + 5)]
    colnames(pop_cor)[5] <- 'R'
    pop_cor <- pop_cor[order(pop_cor$R), ]
    
    
    xs <- pop_cor$Day_x
    ys <-  pop_cor$Day_y
    mid_R <- median(pop_cor$R)
    
    pop_cor$cell_col <- floor(pop_cor$R*12) + 13 # color scale
    
    pop_cor_6 <- subset(pop_cor, pop_cor$window > 6)
    max_R <- pop_cor_6[which.max(pop_cor_6$R)[1], ]
    
    # Sets up the plot
    op <- par(mar = c(0.5, 1.0, 1, 0.5) , mgp = c(0.05, 0.1, 0), tck = -0.01, bty = "n")
    plot(-50, -50, xlim = c(0, dap_x), ylim = c(0, dap_x), col = "white", 
         xlab = '', xaxt = "n", yaxt = "n", ylab = '', bty = "n")
    
    # Plot the heatmap
    rect(xs - 0.5, ys - 0.5, xs + 0.5, ys + 0.5, 
         col = col_palette[pop_cor$cell_col], border = "NA")
    rect(max(pop_cor$Day_x) - 0.5, max(pop_cor$Day_y) - 0.5, 
         max(pop_cor$Day_x) + 0.5, max(pop_cor$Day_y) + 0.5, border = "NA", 
         col = "white", lwd = 0.001)
    
    # Sets up the x-axis
    arrows(10, dap_y + 4, dap_x - 10, dap_y + 4, angle = 15, length = 0.05, 
           lwd = 0.5, col = "grey20")
    mtext("DAP", side = 3, at = dap_x/2, line = -0.1, cex = 0.7)
    mtext(c(1, dap_y), side = 3, at = c(1, dap_y), line = -0.5, 
          cex = 0.6)
    
    # Sets up the y-axis
    arrows(-2, 10, -2, dap_y - 10, length = 0.05, angle = 15, lwd = 0.5, 
           col = "grey20")
    mtext("DAP", side = 2, at = dap_y/2, line = 0.1, cex = 0.7)
    mtext(c(1, dap_y), side = 2, at = c(1, dap_y), line = 0, cex = 0.6)
    
    # Marks the maximum correlation
    # arrows(max_R$Day_x + 4,  max_R$Day_y - 4,  max_R$Day_x,  max_R$Day_y, 
    #        length = 0.1, angle = 15, lwd = 0.75, col = "black")
    arrows(0.55*max_days,  max_days/3,  max_R$Day_x,  max_R$Day_y, 
           length = 0.1, angle = 15, lwd = 0.75, col = "black")
    max_r_lab <- paste0( 'r = ', sprintf( "%.3f", max_R$R))
    legend(0.55*max_days, max_days/3,
           c(paste(max_R$Day_x, 'to', max_R$Day_y, 'DAP'), max_r_lab),
           cex = 0.9, bty = "n", xjust = 0.5, y.intersp = 1.2)
    
    # Construct the color legend
    box_ys <- seq(1, 50, by = 2)
    box_xs <- rep(dap_x - 15, 25)
    rect(box_xs - 0.5*2, box_ys - 0.5*2, box_xs + 0.5*2, box_ys + 0.5*2, 
         border = "NA", col = col_palette)
    text(dap_x - 14, 57, 'r', cex = 1.25)
    
    r_lab_top <- 1; r_lab_mid <- 0; r_lab_bottom <- -1
    text(dap_x - 10 + 3, 50, r_lab_top, cex = 0.9)
    text(dap_x - 10 + 3, 27, r_lab_mid, cex = 0.9)
    text(dap_x - 10 + 3, 1,  r_lab_bottom, cex = 0.9)
    
    # Label the covariate
    mtext(side = 1, Paras[k], line= -1,  cex = 1, bty = "n")
  }
  
  
  corPs <- as.data.frame(maxPR_m)
  for (i in 2:6) corPs[[i]] <- as.numeric(corPs[[i]])
  
  logP1 <- expression(paste(-log[10], '(', italic(P), ')', sep = ""))
  y_labs <- c(logP1, expression(italic('r')))
  x_labs <- c('Covariate', '')
  
  # Trace plot of correlation p-values
  op <- par(mar = c(2, 2.0, 1, 0.5) , mgp = c(1, 0.1, 0), tck = -0.01, 
            cex.axis = 1.3)
  plot(-100, -100,  xlim = c(1, max_days*nParas) - 0,  
       ylim = range(corPs$P, na.rm = T) + c(0, 3), type = "l", bty = "l", 
       xlab = "", ylab = "", xaxt = "n", cex.lab = 1.3)
  axis(side = 1, at = 1:nParas * max_days - max_days/2 , labels = Paras)
  abline(v = 1:(nParas) * max_days,  lwd = 0.5, col = "grey")
  mtext(y_labs[1], side = 3, line = -1.5, cex = 1, at = 10)
  
  lbl_step <- floor(max_days/4)
  
  for (k in 1:nParas) {
    corPs_mid_xy <- subset(corPs, corPs$Para == Paras[k])
    points(corPs_mid_xy$midXY + (k - 1)*max_days , corPs_mid_xy$P, 
           col = "cornflowerblue", type = "l", lwd = 1)
    text(seq(lbl_step, max_days, lbl_step) + (k - 1)*max_days, rep(0.2, 4), 
         seq(lbl_step, max_days, lbl_step), cex = 1)
  }
  
  # Trace plot of correlations
  op <- par(mar = c(1.0, 2.0, 0, 0.5) , mgp = c(1, 0.1, 0), tck = -0.01, 
            cex.axis = 1.3)
  plot(-100, -100,  xlim = c(1, max_days*nParas) - 0,  ylim = range(-1, 1), 
       type = "l", xlab = '', ylab = "", xaxt = "n", bty = "n", cex.lab = 1.3)
  abline(v = 1:(nParas) * max_days,  lwd = 0.5, col = "grey")
  abline(h = 0, col = "black")
  mtext(y_labs[2], side = 3, line = -1.5, cex = 1, at = 1)
  
  for (k in 1:nParas) {
    corPs_mid_xy <- subset(corPs, corPs$Para == Paras[k])
    points(corPs_mid_xy$midXY + (k - 1)*max_days, corPs_mid_xy$R, 
           col = "cornflowerblue", type = "l", lwd = 1)
  }
  
  # par(op)
  layout(matrix(1))
}

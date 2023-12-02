##############
col_wdw <- 25;
col_palette <- diverge_hcl(col_wdw + 1, h = c(260, 0), c = 100, l = c(50, 90), power = 1)

FW_Model <- function(exp_trait, exp_trait_dir, trait, all_env_codes, env_mean_trait, env_meta_info_0) {
 env_meta_info <- env_meta_info_0;
 n_envs <- nrow(env_mean_trait);
 lm_ab_matrix <- matrix(ncol = 4, nrow = length(line_codes)); ## line_code, a_mean, b_mean, r2

 n_obs_env <- c(); quantile_1 <- c(); quantile_3 <- c(); key_para <- c(); key_para2 <- c();
 for (k in 1:nrow(env_mean_trait)) {
   env_data <- subset(exp_trait, exp_trait$env_code == env_mean_trait[k,1]);
   quantiles <- quantile(env_data$Yobs, na.rm = T);
   quantile_1[k] <- quantiles[2];
   quantile_3[k] <- quantiles[4];
   n_obs_env[k] <- length(which(!is.na(env_data$Yobs)))
 }
 env_mean_trait$q1 <- quantile_1; env_mean_trait$q3 <- quantile_3; env_mean_trait$n_obs <- n_obs_env;
 
 line_codes <- unique(as.vector(exp_trait$line_code)); 
 
 gray_alpha <- rgb(128, 128, 128, alpha = 35, maxColorValue = 255);
 poly_alpha <- rgb(238, 130, 238, alpha = 55.5, maxColorValue = 255);

 env_codes <- as.vector(env_mean_trait$env_code);
 line_by_env_df <- data.frame(line_code = line_codes);
 for (e_i in 1:nrow(env_mean_trait)) {
   e <- env_codes[e_i];
   e_trait <- subset(exp_trait, exp_trait$env_code == e);
   nonNAs <- length(which(!is.na(e_trait[,3])))
#   if (nonNAs > (0.5 * length(line_codes))) {
    colnames(e_trait)[3] <- e;
    line_by_env_df <- merge(line_by_env_df, e_trait[,c(1,3)], all.x = T)
#   }
 }
 write.table(line_by_env_df, file = paste(exp_trait_dir, trait, '_', n_envs, 'Env_LbE', '.txt', sep = ''), sep = "\t", row.names = F, quote = F);

 
 trait_dist_png_file <- paste(exp_trait_dir, trait, '_', n_envs, '_FW.png', sep = '');

 png(trait_dist_png_file, width= 8, height= 2, pointsize=12, unit = "in", res = 600);

 layout(matrix(c(1:4), 1, 4, byrow = T))
 env_mean_trait <- env_mean_trait[env_mean_trait$env_code%in% colnames(line_by_env_df),]
 env_geo_order_df <- merge(env_mean_trait, env_meta_info_0);
 env_geo_order_df <- env_geo_order_df[order(env_geo_order_df$lat, env_geo_order_df$lon, env_geo_order_df$PlantingDate),];
 env_geo_order <- match(env_mean_trait$env_code, env_geo_order_df$env_code); ## 
 
 par(mar = c(5.0, 2.0, 1, 0.5) , mgp = c(1, 0.1, 0), tck = -0.01, cex.axis = .7, cex.lab = .8, family = "mono");
 plot(0, 0, col = "white", xlim = range(env_geo_order), ylim = range(exp_trait$Yobs, na.rm = T),  ylab = trait,  xlab = '', fg = "gray50", xaxt = "n");
 for (i in 1:nrow(line_by_env_df)) {
  df4 <- data.frame(env_code = env_mean_trait$env_code, env_order = env_geo_order, Yobs = as.numeric(line_by_env_df[i, -1]));
  df4 <- df4[!is.na(df4$Yobs),];
  df4 <- df4[order(df4$env_order),];
  points(df4$env_order, df4$Yobs, col = gray_alpha, type = "l", pch = 19, lwd = .6)
  points(df4$env_order, df4$Yobs, col = gray_alpha,  pch = 19, cex = .3)
 }
  points(c(1:nrow(env_geo_order_df)), env_geo_order_df$meanY, col = env_cols[match(as.vector(env_geo_order_df$env_code), all_env_codes )], cex = .8)
  points(c(1:nrow(env_geo_order_df)), env_geo_order_df$meanY, col = "black", type = "l", lwd = .5)

 mtext(env_geo_order_df$env_code, side = 1, at = c(1:nrow(env_geo_order_df)), las = 2, line = 0.5, cex = .5 )
 mtext('A', side = 3, at = 1)

 x_tick <- diff(env_mean_trait[,2]) / 50;
 par(mar = c(2.0, 2.0, 1, 0.5) , mgp = c(1, 0.1, 0), tck = -0.01, cex.axis = .7, family = "mono");
 plot(0, 0, col = "white", xlim = range(env_mean_trait$meanY), ylim = range(exp_trait$Yobs, na.rm = T), cex.lab = .9,  ylab = trait,  xlab = 'Environmental mean', fg = "gray50");
 for (i in 1:nrow(line_by_env_df)) {
   df3 <- data.frame(meanY = env_mean_trait$meanY, Yobs = as.numeric(line_by_env_df[i, -1]));
   df3 <- df3[!is.na(df3$Yobs),];
   points(df3$meanY, df3$Yobs, col = gray_alpha, type = "l", pch = 19, lwd = .3)
   points(df3$meanY, df3$Yobs, col = gray_alpha,  pch = 19, cex = .3)
 }
 abline(a = 0, b = 1, lty = 2, col = "grey")
 polygon(c(env_mean_trait[,2], rev(env_mean_trait[,2])), c( env_mean_trait$q1 , rev(env_mean_trait$q3)), col = poly_alpha, border = "NA")
 points(env_mean_trait$meanY, env_mean_trait$meanY, col = "black", cex = .4)
 legend("topleft", as.vector(env_mean_trait$env_code), col = env_cols[match(as.vector(env_mean_trait$env_code), all_env_codes )], pch = 19, bty = "n", cex = .4)
 
 for (k in 1:nrow(env_mean_trait)) {
   env_data <- subset(exp_trait, exp_trait$env_code == env_mean_trait[k,1]);
   boxplot(env_data$Yobs, add = T, boxwex = x_tick * 5 * 2, at = env_mean_trait$meanY[k], cex = .3, border = env_cols[match(as.vector(env_data$env_code), all_env_codes )], lwd = .4, boxlwd = .7, medlwd = .5, yaxt = "n")
 }
 
 mtext('B', side = 3, at = min(env_mean_trait$meanY)) 
 ###
 par(mar = c(2.0, 2.0, 1, 0.5) , mgp = c(1, 0.1, 0), tck = -0.01, cex.axis = .7, family = "mono");
 plot(0, 0, col = "white", xlim = range(env_mean_trait$meanY), ylim = range(exp_trait$Yobs, na.rm = T), cex.lab = .9,  ylab = trait,  xlab = 'Environmental mean', fg = "gray50");
 for (i in 1:nrow(line_by_env_df)) {
   df3 <- data.frame(meanY = env_mean_trait$meanY, Yobs = as.numeric(line_by_env_df[i, -1]));
   df3 <- df3[!is.na(df3$Yobs),];
   if(nrow(df3) >= 4) {
    lm_ab <- lm(Yobs ~ meanY, data = df3)
    abline(lm_ab, col = gray_alpha);
    points(df3$meanY, df3$Yobs, col = gray_alpha,  pch = 19, cex = .3)
    a_Mean <- as.vector(round(predict(lm_ab, data.frame(meanY = mean(env_mean_trait$meanY))), 4)); ## adjusted by the population mean
    b_Mean <- as.vector(round(lm_ab$coefficient[2], 4));
    R_Mean <- round(summary(lm_ab)$r.squared, 4)
    lm_ab_matrix[i,] <- c(line_by_env_df[i,1], a_Mean, b_Mean, R_Mean);
   }
 }
 mtext('C', side = 3, at = min(env_mean_trait$meanY)) 
 lm_ab_matrix <- lm_ab_matrix[!is.na(lm_ab_matrix[,2]),];
 colnames(lm_ab_matrix) <- c('line_code', 'Intcp_mean', 'Slope_mean','R2_mean');
 write.table(lm_ab_matrix, file = paste(exp_trait_dir, trait, '_', n_envs, 'Env_FW_ab', '.txt', sep = ''), sep = "\t", row.names = F, quote = F)

 par(mar = c(2.0, 2.0, 1, 0.5) , mgp = c(1, 0.1, 0), tck = -0.01, cex.axis = .7, family = "mono");
 hist(as.numeric(lm_ab_matrix[,4]), xlab = 'F-W R-squred', ylab = 'Count', main = '')
 mtext('D', side = 3, at = min(lm_ab_matrix[,4])) 
 

 dev.off()

}
##############
CERIS <- function(env_mean_trait, env_paras, searching_days, exp_trait_dir, trait, Paras, pop_cor_file, pop_corP_file) {
# env_paras <- PTT_PTR; p <- 1; dap_x <- searching_days; dap_y <- searching_days; d1_start <- 1; FTdaps <- exp_traits$FTdap
 dap_x <- searching_days;
 dap_y <- searching_days; 
 p <- 1; LOO <- 0;
 pop_cor_file <- paste(exp_trait_dir, trait, '_', nrow(env_mean_trait), 'Envirome_', LOO, 'LOO_cor.txt', sep = '');
 pop_corP_max_file <- paste(exp_trait_dir, trait, '_', nrow(env_mean_trait), 'Envirome_', LOO, 'LOO_max_corP.txt', sep = '');
 exs_png_file <- paste(exp_trait_dir,  trait, '_', nrow(env_mean_trait), 'Envs_CERIS_', LOO, 'LOO.png', sep = ''); 
 
 nParas <- length(Paras);
 if (!file.exists(pop_cor_file)) {
   dap_win <- searching_days * searching_days  / 2;
   pop_cors_matrix <- matrix(ncol = 5 + (2 * nParas), nrow = dap_win * 1);
   pop_corP_matrix <- matrix(ncol = 5 + (1 * nParas), nrow = dap_win * 1);
   colnames(pop_cors_matrix) <- c("pop_code", 'Day_x', 'Day_y', 'window', 'midXY', paste('R_', Paras, sep = ''), paste('P_', Paras, sep = ''));
   n <- 0;
   for (d1 in 1:(dap_y - 6)) {
     for (d2 in (d1 + 6):dap_y) {
      days <- c(d1:d2); 
      env_facts_matrix <- matrix(nrow = nrow(env_mean_trait), ncol = nParas);
      for (e_i in 1:nrow(env_mean_trait)) {
        e <- env_mean_trait$env_code[e_i];
        env_facts_matrix[e_i,] <- colMeans(env_paras[[match(e, names(env_paras))]][d1:d2,], na.rm = T)
      }
      n <- n + 1;
      ### leave one environment out and get the median correlation
      Ymean_envPara <- cbind(env_facts_matrix, env_mean_trait$meanY);
      rs <- c(); ps <- c();
      if (LOO == 0) {
       for (k in 1:nParas) {
        cor_r <- cor(Ymean_envPara[,nParas + 1], Ymean_envPara[,k], use = "complete.obs");
        cor_P <- -log10(cor.test(Ymean_envPara[,nParas + 1], Ymean_envPara[,k], use = "complete.obs")$p.value);
        rs[k] <- round(cor_r, digits = 4)
        ps[k] <- round(cor_P, digits = 4)
       }
      } else {
       loo_rs_matrix <- matrix(nrow = nrow(Ymean_envPara)+ 0, ncol = nParas);
       loo_ps_matrix <- matrix(nrow = nrow(Ymean_envPara)+ 0, ncol = nParas);
       for (k in 1:nParas) { ## 8 environment parameters
        for (e_x in c(1:nrow(Ymean_envPara))) {
          t_matrix <- Ymean_envPara[-e_x,];
          loo_rs_matrix[e_x, k] <- round(cor(t_matrix[,nParas + 1], t_matrix[,k], use = "complete.obs"), digits = 4)
          loo_ps_matrix[e_x, k] <- round(-log10(cor.test(t_matrix[,nParas + 1], t_matrix[,k], use = "complete.obs")$p.value), digits = 4)
         }
        }
        cor_r <- apply(loo_rs_matrix, 2, median); cor_P <- apply(loo_ps_matrix, 2, median);
        rs[k] <- round(cor_r, digits = 4)
        ps[k] <- round(cor_P, digits = 4)
      }
      pop_cors_matrix[n, ] <- c(p, d1, d2, d2 - d1,(d2 + d1) /2, rs, ps);
     }
   }
   pop_cors_matrix <- pop_cors_matrix[1:n,]
   write.table(pop_cors_matrix, file = pop_cor_file, sep = "\t", row.names = F, quote = F);
#    write.table(pop_corP_matrix, file = pop_corP_file, sep = "\t", row.names = F, quote = F);
   corPs <- read.table(pop_cor_file, head = T, sep = "\t")
   corPs <- as.data.frame(pop_cors_matrix);
   Para_idx_p <- match(paste('P_', Paras, sep = ''), colnames(corPs))
   Para_idx_r <- match(paste('R_', Paras, sep = ''), colnames(corPs))
   maxP_Paras_idx <- aggregate(corPs[,Para_idx_p], list(corPs$midXY), which.max)
   colnames(maxP_Paras_idx)[1] <- 'midXY';
   maxPR_m <- matrix(ncol = 6, nrow = nrow(maxP_Paras_idx) * nParas)
   m <- 1;
   for (k in 1:nrow(maxP_Paras_idx)) {
     mid_xy <- maxP_Paras_idx[k,1]
     for (h in 1:nParas) {
       max_rp <- subset(corPs, corPs$midXY == mid_xy)[maxP_Paras_idx[k, h + 1],]
       max_r <- max_rp[1, Para_idx_r[h]]; max_p <- max_rp[1, Para_idx_p[h]];
       maxPR_m[m,] <- c(h, max_rp$Day_x[1], max_rp$Day_y[1], mid_xy, max_r, max_p)
       m <- m + 1
     }
     
   }
   maxPR_m[,1] <- Paras[maxPR_m[,1]]
   colnames(maxPR_m) <- c('Para', 'Day_x', 'Day_y', 'midXY', 'R', 'P');
   write.table(maxPR_m, file = pop_corP_max_file, quote = F, row.names = F, sep = "\t")

 }

 pop_cors <- read.table(pop_cor_file, header = T, sep = "\t");
 pop_cors <- subset(pop_cors, pop_cors$pop_code == p);
 pop_cors <- cbind(pop_cors[,-4:nParas + 5], 0 - pop_cors[, (1:nParas) + 5]);
 # dev.off();
 # pdf(exs_pdf_file,width= nParas,height= 2,pointsize=6)
 png(exs_png_file, width = nParas * 1.5, height = 3 * 1.5, pointsize = 12, unit = "in", res = 600)
 layout(rbind(matrix(c(1:(2*nParas)), 2, nParas, byrow = T), rep(2*nParas + 1, nParas),rep(2*nParas + 2, nParas)))
 # layout(matrix(c(1:(2*nParas)), 2, nParas, byrow = T))
 
 for (k in 1:(2*nParas)) {
   pop_cor_0 <- subset(pop_cors, pop_cors$pop_code == p);
   pop_cor <- pop_cor_0[,c(1:4, k + 5)];
   colnames(pop_cor)[5] <- 'R';
   pop_cor <- pop_cor[order(pop_cor$R),];
 
   xs <- pop_cor$Day_x;  ys <-  pop_cor$Day_y;  mid_R <- median(pop_cor$R);
 
   cell_col <- floor(pop_cor$R * 12) + 13; ### the same color scale
 
   pop_cor$cell_col <- cell_col;
 
   pop_cor_6 <- subset(pop_cor, pop_cor$window > 6); max_R <- pop_cor_6[which.max(pop_cor_6$R)[1], ];
 
   par(mar = c(0.5, 1.0, 1, 0.5) , mgp = c(0.05, 0.1, 0), tck = -0.01, bty = "n");
   plot(-50, -50, xlim = c(0, dap_x), ylim = c(0, dap_x), col = "white",  xlab = '', xaxt = "n", yaxt = "n", ylab = 'Days after planting', bty = "n", cex.lab = .4);
   arrows(-1, 10, -1, dap_y - 10, length = 0.05, angle = 15, lwd = .5,  col = "grey59");
   mtext(c(1, 50, 100, dap_y), side = 2, at = c(1,50, 100, dap_y), line = -1, cex = .6)
 
   rect(xs - 0.5, ys - 0.5, xs + 0.5, ys + 0.5, col = col_palette[pop_cor$cell_col], border = "NA")
   rect(max(pop_cor$Day_x) - 0.5, max(pop_cor$Day_y) - 0.5, max(pop_cor$Day_x) + 0.5, max(pop_cor$Day_y) + 0.5, border = "NA", col = "white", lwd = 0.001)
 
   arrows(10, dap_y + 4, dap_x - 10, dap_y + 4, angle = 15, length = 0.05, lwd = .5, col = "grey59")
   mtext("Days after planting", side = 3, at = dap_x / 2, line = -0.1, cex = .4)
   mtext(c(1, 50, 100, dap_y), side = 3, at = c(1, 50, 100, dap_y), line = -1.1, cex = .6)
   arrows(max_R$Day_x + 4,  max_R$Day_y - 4,  max_R$Day_x,  max_R$Day_y, length = 0.05, angle = 15, lwd = .5, col = "grey59")
 
   box_ys <- seq(1, 50, by = 2); box_xs <- rep(dap_x - 15, 25);
   rect(box_xs - .5 * 2, box_ys - 0.5 * 2, box_xs + 0.5 * 2, box_ys + 0.5 * 2, border = "NA", col = col_palette)
   text(dap_x - 10 - 5, 52, 'r', cex = .5);
   r_lab_top <- 1; r_lab_mid <- 0; r_lab_bottom <- -1; max_r_lab <- paste( 'r = ', sprintf( "%.3f", max_R$R), sep = '');
   if (k > nParas) { r_lab_top <- -1; r_lab_bottom <- 1; max_r_lab <- paste( 'r = ', sprintf( "%.3f", 0 - max_R$R), sep = ''); mtext(side = 1, Paras[k - nParas ], line= -0.5,  cex = .5, bty = "n")}
   legend(max_R$Day_x - 4 , max_R$Day_y - 4 , c(paste( max_R$Day_x, ' to ', max_R$Day_y, ' DAP', sep = ''), max_r_lab),  cex = .6, bty = "n");
   text(dap_x - 10 + 3, 50, r_lab_top, cex = .5)
   text(dap_x - 10 + 3, 27, r_lab_mid, cex = .5);
   text(dap_x - 10 + 3, 1,  r_lab_bottom, cex = .5)
 
 }
 
 corPs <- read.table(pop_corP_max_file, head = T, sep = "\t");
 logP1 <- expression(paste('-log(', italic(P), ')', sep = ''));
 
 y_labs <- c(logP1, expression(italic('r')))
 x_labs <- c('Envirome', '')
 
 par(mar = c(2, 2.0, 1, 0.5) , mgp = c(1, 0.1, 0), tck = -0.01, cex.axis = .7);
 plot(-100, -100,  xlim = c(1, searching_days * nParas) - 0,  ylim = range(corPs$P, na.rm = T) + c(0, 3), type = "l", bty = "l", xlab = x_labs[1], ylab = y_labs[1], xaxt = "n"); ##
 axis(side = 1, at = 1:nParas * searching_days - searching_days/2 , labels = Paras)
 abline(v = 1:(nParas) * searching_days,  lwd = 0.5, col = "grey")
 for (k in 1:nParas) {
   corPs_mid_xy <- subset(corPs, corPs$Para == Paras[k])
   points(corPs_mid_xy$midXY + (k - 1) * searching_days , corPs_mid_xy$P, col = "cornflowerblue", type = "l", lwd = 1)
   text(seq(3,12, 3) * 10 + (k - 1) * searching_days, rep(0.5, 4), seq(3,12, 3) * 10, cex = .75)
 
  }
  par(mar = c(1.0, 2.0, 0, 0.5) , mgp = c(1, 0.1, 0), tck = -0.01, cex.axis = .7);
  plot(-100, -100,  xlim = c(1, searching_days * nParas) - 0,  ylim = range(-1,1), type = "l", xlab = '', ylab = y_labs[2], xaxt = "n", bty = "n"); ##
  abline(v = 1:(nParas) * searching_days,  lwd = 0.5, col = "grey")
  abline(h =0 , col = "black")
  for (k in 1:nParas) {
    corPs_mid_xy <- subset(corPs, corPs$Para == Paras[k])
    points(corPs_mid_xy$midXY + (k - 1) * searching_days , corPs_mid_xy$R, col = "cornflowerblue", type = "l", lwd = 1)
   }
 
 dev.off()
 
}
##############
Plot_Trait_mean_kPara <- function(env_mean_trait, env_paras, d1, d2, trait, exp_trait_dir, env_cols, kPara_name, kPara_ind, envMeanPara_file){
  nParas <- length(Paras); ##Paras_ind <- match(Paras,  colnames(env_paras))
  days <- c(d1:d2); 
  env_facts_matrix <- matrix(nrow = nrow(env_mean_trait), ncol = nParas );
  kPara_values <- c()
  for (e_i in 1:nrow(env_mean_trait)) {
    e <- env_mean_trait$env_code[e_i];
    env_mean <- colMeans(env_paras[[match(e, names(env_paras))]][days,], na.rm = T)[kPara_ind]; ### DL, GDD, PTT, PTR, PTS
    kPara_values[e_i] <-  round(env_mean, 4) 
  }
  colnames(env_facts_matrix) <- c( Paras);
  
  envMeanPara <- env_mean_trait;
  envMeanPara$kPara <- kPara_values;
  
  png_file <- paste(exp_trait_dir, trait, 'MeanY_', nrow(env_mean_trait), kPara_Name, '_D', d1, '_', d2, '.png', sep = ''); 
  png(png_file, width= 4,height= 4,pointsize= 12, unit = "in", res = 600)
  par(mar = c(2.5, 2.0, 1, 0.5) , mgp = c(0.7, 0.01, 0), tck = -0.01, family = "mono");
  plot(envMeanPara$kPara, envMeanPara$meanY, xlab = kPara_Name, ylab = paste(trait, ' mean', sep = ''),  pch = 19, col = env_cols);
  abline(lm(meanY ~ kPara, data = envMeanPara), lty = 2);
  r1 <- round(cor(envMeanPara$meanY , envMeanPara$kPara), 3);
  legend("bottom", paste('r = ', r1, sep = ''), bty = "n")
  legend_p <- "topleft";  if (r1 < 0) { legend_p <- "topright"};
#  legend(legend_p, as.vector(env_mean_trait$env_code), pch = 19, col = env_cols, bty = "n", cex = .75 )
  dev.off()
  
  return(envMeanPara)
  
  colnames(envMeanPara)[3] <- kPara_Name;
  write.table(envMeanPara, file = envMeanPara_file, sep = "\t", row.names = F, quote = F);

}
##############
Slope_Intercept <- function(meanY_kPara, exp_trait, exp_trait_dir, kpara_append, save_tag) {
 line_codes <- unique(exp_trait$line_code); 
 lm_ab_matrix <- matrix(ncol = 8, nrow = length(line_codes));
 obs_prd_m <- matrix(0, ncol = 7,nrow = nrow(exp_trait));
 n <- 0; p <- 1
 for (l in 1: length(line_codes)) {
   l_trait <- subset(exp_trait, exp_trait$line == line_codes[l]);
#   if(nrow(l_trait) >= 15) {
     l_trait <- merge(l_trait, meanY_kPara, by = "env_code");
     lm_Mean <- lm(Yobs ~ meanY, data = l_trait);
     lm_Para <- lm(Yobs ~ kPara, data = l_trait); 
     a_Mean <- as.vector(round(predict(lm_Mean, data.frame(meanY = mean(meanY_kPara$meanY))), 4)); ## adjusted by the population mean
     b_Mean <- as.vector(round(lm_Mean$coefficient[2], 4));
     b_Para <- as.vector(round(lm_Para$coefficient[2],4));
     a_Para <- as.vector(round(predict(lm_Para, data.frame(kPara = mean(meanY_kPara$kPara))), 4)); ## adjusted by the population mean
     a_Para_ori <- as.vector(round(lm_Para$coefficient[1],4));
     R_Mean <- round(summary(lm_Mean)$r.squared, digit = 2)
     R_Para <- round(summary(lm_Para)$r.squared, digit = 2)
     lm_ab_matrix[l,] <- c(line_codes[l], a_Mean, b_Mean, a_Para, a_Para_ori, b_Para, R_Mean, R_Para);
     
     for (e in 1:nrow(l_trait)) {
       obs_trait <- l_trait$Yobs[e];
       if (!is.na(obs_trait)) {
         trn <- l_trait[-e,];
         l_mean <- round(mean(trn$Yobs, na.rm = T), digit  = 3);
         prd_trait_mean  <- round(predict( lm(Yobs ~ meanY, data = trn), l_trait[e,]), digit = 3);
         prd_trait_kpara <- round(predict( lm(Yobs ~ kPara, data = trn), l_trait[e,]), digit = 3);
         n <- n + 1;
         obs_prd_m[n,] <- c(l_trait$env_code[e], p, line_codes[l], prd_trait_mean, prd_trait_kpara, obs_trait, l_mean);
       }
     }
#   }
 }
 lm_ab_matrix <- lm_ab_matrix[!is.na(lm_ab_matrix[,2]),];
 colnames(lm_ab_matrix) <- c('line_code', 'Intcp_mean', 'Slope_mean', 'Intcp_para_adj', 'Intcp_para', 'Slope_para', 'R2_mean', 'R2_para');

 if (save_tag == 1) {
  out_file <- paste(exp_trait_dir, trait, '_', nrow(env_mean_trait), 'Env_IntcpSlope_', kpara_append, '.txt', sep = '');
  write.table(lm_ab_matrix, file = out_file, sep = "\t", quote = F, row.names = F)
  
  obs_prd_m <- obs_prd_m[1:n,]
  colnames(obs_prd_m) <- c('env_code', 'pop_code', 'line_code', 'Prd_trait_mean', 'Prd_trait_kPara', 'Obs_trait', 'Line_mean');
  obs_prd_file <- paste(exp_trait_dir, trait, '_', nrow(env_mean_trait), 'Env_1to2_', kpara_append, '.txt', sep = '');
  write.table(obs_prd_m, file = obs_prd_file, sep = "\t", quote = F, row.names = F); 
 }
 else { return (lm_ab_matrix) }
} 
##############
Enviromic_Prediction <- function(gFold, eIteration, envParas, envMeanYs, Paras, trait) {
 EnvVarM_file <- paste(exp_dir, 'Envirotype.txt', sep = '');
 n_K <- length(Paras)
 n_E <- nrow(env_meta_info_0);
 if (!file.exists(EnvVarM_file)) { 
  
  
  dap_win <- searching_days * searching_days  / 2;
  EnvVarM <- matrix(nrow = dap_win * n_K, ncol = n_E + 5);
 #  Paras <- c('DTR', 'GDD','dGDD', 'PR', 'PRDTR')##, 'PTD1', 'PTD2'); ## TS = Tmax^2-Tmin^2; TRR = 1-Tmin/Tmax
  #Paras <- ('PRDTR')
  n1 <- 1;
  dap_y <- 150 ##
  d1_start <- 1;
 for (d1 in d1_start:(dap_y - 6)) {
   for (d2 in (d1 + 6):dap_y) {
    days <- c(d1:d2); 
    env_facts_matrix <- matrix(nrow = n_E, ncol = n_K);
    for (e_i in 1:n_E) {
      e <- env_meta_info_0$env_code[e_i];
      env_facts_matrix[e_i,] <- round(colMeans(envParas[[match(e, names(envParas))]][d1:d2,], na.rm = T), digits = 3)
    }
    n2 <- n1 + n_K - 1
    EnvVarM[n1:n2, 6:(n_E + 5)] <- t(env_facts_matrix)
    EnvVarID <- paste(Paras, d1, d2, sep = '_'); 
    EnvVarM[n1:n2,1] <- EnvVarID
    EnvVarM[n1:n2,2] <- Paras
    EnvVarM[n1:n2,3] <- rep(d1, n_K)
    EnvVarM[n1:n2,4] <- rep(d2, n_K)
    EnvVarM[n1:n2,5] <- rep((d1 + d2) / 2, n_K)
    n1 <- n2 + 1;
    }
   } 
  EnvVarM <- as.data.frame(EnvVarM[1:n2,]);
  colnames(EnvVarM) <- c('eVar_ID', 'eVar', 'D1', 'D2', 'midXY', env_meta_info_0$env_code)
  write.table(EnvVarM, file = EnvVarM_file, quote = F, row.names = F, sep = "\t")
 }
 
 EnvVarM_0 <- read.table(EnvVarM_file, sep = "\t", head = T, check.names = F)
 EnvVarM <- subset(EnvVarM_0, (EnvVarM_0$D2 - EnvVarM_0$D1) >= 7)

 #EnvVarM <- subset(EnvVarM, EnvVarM$D2 < 60)
 X_matrix <- matrix()
 for (k in 1:n_K) {
  envPara <- Paras[k]
  envM <- (subset(EnvVarM, EnvVarM$eVar == envPara)[,-(1:5)])
  envM_2 <- scale(envM, center = T,scale = T ) ## scale & center each parameter by itself
  if (k == 1) { X_matrix <- envM_2} else { X_matrix <- rbind(X_matrix, envM_2)}
 }
 
 for (ti in 2:(ncol(envMeanYs) - 0)) {
#  trait <- colnames(envMeanYs)[ti]
  envMeanY <- envMeanYs[,c(1, ti)];
  
  envMeanY <- envMeanY[!(is.na(envMeanY[,2])),] 
  block_idx <- seq(1, (nrow(envMeanY) + 1), by = floor(nrow(envMeanY) / eFold)  )
  block_idx <- floor(seq(1, nrow(envMeanY) , length = eFold  ))
#  if (block_idx[length(block_idx)] < nrow(envMeanY)) {block_idx[length(block_idx)] <- nrow(envMeanY) + 1}
 # if (block_idx[length(block_idx)] <= nrow(envMeanY)) { block_idx[length(block_idx)] <- nrow(envMeanY) + 1}
#  colnames(envMeanY) <- c('env_code', 'Y')
  for (n in 1:eIteration) {
   env_idx <- sample(1:nrow(envMeanY), nrow(envMeanY));
   for (bi in 1:(length(block_idx) - 1)) {
    block_s <- block_idx[bi]; block_e <- block_idx[bi + 1] - 1
    if (bi == (length(block_idx) - 1)) {block_e <- block_idx[bi + 1] }
    env_prd_idx <- sort(env_idx[block_s:block_e] )
    prd_result <- Pred_rrBLUP(envMeanY, X_matrix, env_prd_idx, n)
    if (ti == 2 & n == 1 & bi == 1) { PrefPred_df <- prd_result} else { PrefPred_df <- rbind(PrefPred_df, prd_result)}
   }
  }
  PrefPred_df$trait_code <- rep(trait, nrow(PrefPred_df))
 }
 write.table(PrefPred_df, file = paste(exp_trait_dir, trait, '_', n_E,'ENV_EP_', eFold, 'fold_', eIteration, 'rep.txt', sep = ''), row.names = F, sep = "\t", quote = F)
}
##############
One_to_4_Prediction <- function(gFold, gIteration, SNPs, exp_trait, line_codes, meanY_kPara, kpara_append) {
 
 for (e_i in 1:nrow(meanY_kPara)) {
  meanY_kPara_Loo <- meanY_kPara[-e_i,];
  exp_trait_Loo <- subset(exp_trait, exp_trait$env_code != meanY_kPara$env_code[e_i]);
  lm_ab_matrix <- Slope_Intercept(meanY_kPara_Loo, exp_trait_Loo, exp_trait_dir, kpara_append, 0)
  lm_ab_matrix <- as.data.frame(lm_ab_matrix[, c(1, 5, 6)]) ## make sure the colname is 'line_code', 'Intcp_para', 'Slope_para'
  block_idx <- floor(seq(1, nrow(lm_ab_matrix) , length = gFold  ))
  if (block_idx[length(block_idx)] < nrow(lm_ab_matrix)) {block_idx[length(block_idx)] <- nrow(lm_ab_matrix)}
  for (n in 1:gIteration) {
   env_idx <- sample(1:nrow(lm_ab_matrix), nrow(lm_ab_matrix));
   for (bi in 1:(length(block_idx) - 1)) {
    block_s <- block_idx[bi]; block_e <- block_idx[bi + 1] - 1
    if (bi == (length(block_idx) - 1)) {block_e <- block_idx[bi + 1] }
    prd_idx <- sort(env_idx[block_s:block_e] )
    ab_prd <- Pred_rrBLUP(lm_ab_matrix, SNPs, prd_idx, n) ## predict a and b
    Y_prd <- round(ab_prd$Intcp_para_prd + ab_prd$Slope_para_prd * meanY_kPara$kPara[e_i], digits = 3); ## predict Y
    prd_result <- data.frame(line_code = ab_prd$ID_code, env_code = rep(meanY_kPara$env_code[e_i]), Yprd = Y_prd)
    prd_result <- merge(prd_result, exp_trait)
    prd_result$Rep <- rep(n, nrow(ab_prd))
    if (e_i == 1 & n == 1 & bi == 1) { PrefPred_df <- prd_result} else { PrefPred_df <- rbind(PrefPred_df, prd_result) }
   }
  }
 }
 out_1to4_file <- paste(exp_trait_dir, trait, '_', nrow(env_mean_trait), 'Env_1to4_', gFold, 'fold_', gIteration, 'rep_', kpara_append, '.txt', sep = '')
 write.table(PrefPred_df, file = out_1to4_file, row.names = F, quote = F, sep = "\t");
}
##############
One_to_3_Prediction <- function(gFold, gIteration, SNPs, exp_trait, line_codes, meanY_kPara, kpara_append) {
 total_s <- length(line_codes)
 block_idx <- floor(seq(1, total_s , length = gFold ))
 if (block_idx[length(block_idx)] < total_s) {block_idx[length(block_idx)] <- total_s}
 lm_ab_matrix <- Slope_Intercept(meanY_kPara, exp_trait, exp_trait_dir, kpara_append, 0)
 lm_ab_matrix <- as.data.frame(lm_ab_matrix[, c(1, 5, 6)])
 for (n in 1:gIteration) {
  env_idx <- sample(1:total_s, total_s);
  for (bi in 1:(length(block_idx) - 1)) {
   block_s <- block_idx[bi]; block_e <- block_idx[bi + 1] - 1
   if (bi == (length(block_idx) - 1)) {block_e <- block_idx[bi + 1] }
   prd_idx <- sort(env_idx[block_s:block_e] )
   ab_prd <- Pred_rrBLUP(lm_ab_matrix, SNPs, prd_idx, n) ## predict a and b
   for (e_i in 1:nrow(meanY_kPara)) {
    Y_prd <- round(ab_prd$Intcp_para_prd + ab_prd$Slope_para_prd * meanY_kPara$kPara[e_i], digits = 3);
    prd_result <- data.frame(line_code = ab_prd$ID_code, env_code = rep(meanY_kPara$env_code[e_i]), Yprd = Y_prd)
    prd_result <- merge(prd_result, exp_trait)
    prd_result$Rep <- rep(n, nrow(ab_prd))
    if (e_i == 1 & n == 1 & bi == 1) { PrefPred_df <- prd_result} else { PrefPred_df <- rbind(PrefPred_df, prd_result) }
   }
  }
 }
 out_1to3_file <- paste(exp_trait_dir, trait, '_', nrow(env_mean_trait), 'Env_1to3_', gFold, 'fold_', gIteration, 'rep_', kpara_append, '.txt', sep = '')
 write.table(PrefPred_df, file = out_1to3_file, row.names = F, quote = F, sep = "\t");
}

##############

Compile_Envirome_Matrix <- function(exp_dir, all_env_codes, envParas_file) {

 env_dir <- paste(exp_dir, 'dailyEnv/', sep = '');
 envParas <- list()
 for (e_i in 1:length(all_env_codes)) {
  env_daily_file <- paste(env_dir, all_env_codes[e_i], '_daily.txt', sep = '');
  env_daily <- read.table(env_daily_file, head = T, sep = "\t");
  envParas[[e_i]] <- env_daily[,-1]
 } 
 names(envParas) <- all_env_codes;
 save(envParas, file = envParas_file)
}
##############
Pred_rrBLUP <- function(Y_matrix, X_matrix, prd_idx, n ) {
# Y_matrix <- lm_ab_matrix; X_matrix <- SNPs;
 colnames(Y_matrix)[1] <- 'ID_code'
 y_trn <- Y_matrix[-(prd_idx),];
 y_prd <- Y_matrix[prd_idx,];
 A_trn <- X_matrix[, match(y_trn$ID_code, colnames(X_matrix)) - 0 ];
 A_prd <- X_matrix[, match(y_prd$ID_code, colnames(X_matrix)) - 0 ];
 prd_result_0 <- y_prd
 
# rep_code <- append(rep_code, rep(n, nrow(Y_prd)))
 for (t_i in 2:ncol(y_trn)) {
  colnames(prd_result_0)[t_i] <- paste(colnames(prd_result_0)[t_i], '_obs', sep = '')
  M1k <- mixed.solve(as.numeric(y_trn[,t_i]), Z = t(A_trn), K = NULL)
  U <- as.matrix(M1k$u); y_prd_0 <- t(A_prd) %*% U
  y_prd_i <- round(y_prd_0 + as.numeric(M1k$beta), digits = 3)
  df1 <- data.frame(ID_code = y_prd$ID_code, prd_i = y_prd_i);
  colnames(df1)[2] <- paste(colnames(y_trn)[t_i], '_prd', sep = '')
  prd_result_0 <- merge(prd_result_0, df1, by = "ID_code")
 }
 prd_result_0$Rep <- rep(n, nrow(y_prd))
 return(prd_result_0)
}
##########
Plot_crossvalidation_result <- function(gFold, gIteration, all_env_codes, kpara_append) {
 CV_files <- c('Env_1to2_', paste('Env_1to3_', gFold, 'fold_', gIteration, 'rep_', sep = ''), paste('Env_1to4_', gFold, 'fold_', gIteration, 'rep_', sep = ''))

 png_file <- paste(exp_trait_dir, trait, 'Pred_1to234_', length(all_env_codes), kpara_append, '.png', sep = ''); 
 png(png_file, width= 4 * 3 * 0.75,height= 4 * 0.75,pointsize= 12, unit = "in", res = 600)
 layout(matrix(1:3, nrow = 1))
  
 for (i in 1:3) {
  CV_file <- paste(exp_trait_dir, trait, '_', length(all_env_codes), CV_files[i], kpara_append, '.txt', sep = '');
  CVs <- read.table(CV_file, head = T, sep = "\t")
  
  if (i > 1) {CVs <- subset(CVs, CVs$Rep == 1)}
  if (i == 1) {colnames(CVs)[5:6] <- c('Yprd', 'Yobs')}
  xy_lim <- range(CVs$Yprd, CVs$Yobs, na.rm = T)
  par(mar = c(2.5, 2.0, 1, 0.5) , mgp = c(0.7, 0.01, 0), tck = -0.01, family = "mono");
  plot(CVs$Yprd, CVs$Yobs, col = env_cols[match(CVs$env_code, all_env_codes)], xlab = 'Predicted', ylab = 'Observed', pch = 19, xlim = xy_lim, ylim = xy_lim)
  abline(a = 0, b = 1, col = "grey", lty = 2)
  r1 <- sprintf("%.2f", cor(CVs$Yprd, CVs$Yobs, use = "complete.obs"));
  legend("bottom", legend= substitute(paste(italic('r'), " = ", R1), list(R1 = r1)), bty = "n")
  legend("topleft", paste('1 to ', i + 1, ' prediction', sep = ''), bty = "n")
 }
 dev.off()

}

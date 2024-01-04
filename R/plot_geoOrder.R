plot_geoOrder <- function(env_mean_trait, env_meta_info_0, line_by_env_df, trait) {
  # Order the environments by latitude, longitude, and planting date
  env_mean_trait <- env_mean_trait[env_mean_trait$env_code %in% colnames(line_by_env_df), ]
  env_geo_order_df <- merge(env_mean_trait, env_meta_info_0)
  env_geo_order_df <- env_geo_order_df[order(env_geo_order_df$lat, env_geo_order_df$lon, env_geo_order_df$PlantingDate), ]
  env_geo_order <- match(env_mean_trait$env_code, env_geo_order_df$env_code)
  
  op <- par(mar = c(5.0, 2.0, 1, 0.5) , mgp = c(1, 0.1, 0), tck = -0.01, cex.axis = 1, cex.lab = 1.2, family = "mono");
  plot(0, 0, col = "white", xlim = range(env_geo_order), ylim = range(exp_trait$Yobs, na.rm = T),  ylab = trait,  xlab = '', fg = "gray50", xaxt = "n");
  
  # Plot the data for each line
  for (i in 1:nrow(line_by_env_df)) {
    df4 <- data.frame(env_code = env_mean_trait$env_code, env_order = env_geo_order, Yobs = as.numeric(line_by_env_df[i, -1]))
    df4 <- df4[!is.na(df4$Yobs), ]
    df4 <- df4[order(df4$env_order), ]
    points(df4$env_order, df4$Yobs, col = gray_alpha, type = "l", pch = 19, lwd = 0.6)
    points(df4$env_order, df4$Yobs, col = gray_alpha,  pch = 19, cex = 0.3)
  }
  
  # Add the environmental means
  points(c(1:nrow(env_geo_order_df)), env_geo_order_df$meanY, col = "black", type = "l", lwd = 0.5)
  points(c(1:nrow(env_geo_order_df)), env_geo_order_df$meanY, col = env_cols[match(as.vector(env_geo_order_df$env_code), all_env_codes )], cex = 0.8, pch = 19)
  
  mtext(env_geo_order_df$env_code, side = 1, at = c(1:nrow(env_geo_order_df)), las = 2, line = 0.5, cex = 1)

  # par(op)
}

predRRBlup <- function(Y_matrix, X_matrix, prd_idx, n) {
  if (any(is.na(X_matrix))) {
    A_imp <- A.mat(X_matrix, return.imputed = TRUE)
    X_matrix <- A_imp$imputed
    rm(A_imp); gc()
  }
  
  colnames(Y_matrix)[1] <- 'ID_code'
  
  y_trn <- Y_matrix[-prd_idx, ]
  A_trn <- X_matrix[match(y_trn$ID_code, rownames(X_matrix), nomatch = 0), ]
  y_trn <- y_trn[match(y_trn$ID_code, rownames(A_trn), nomatch = 0), ]
  
  y_prd <- Y_matrix[prd_idx, ]
  A_prd <- X_matrix[match(y_prd$ID_code, rownames(X_matrix), nomatch = 0), ]
  
  prd_result_0 <- y_prd
  colnames(prd_result_0)[-1] <- paste0(colnames(prd_result_0)[-1], '_obs')
  
  for (t_i in 2:ncol(y_trn)) {
    M1k <- mixed.solve(y_trn[, t_i], Z = A_trn)
    U <- as.matrix(M1k$u)
    y_prd_0 <- A_prd %*% U
    y_prd_i <- y_prd_0 + as.numeric(M1k$beta)
    df1 <- data.frame(ID_code = rownames(A_prd), prd_i = y_prd_i)
    colnames(df1)[2] <- paste0(colnames(y_trn)[t_i], '_prd')
    prd_result_0 <- merge(prd_result_0, df1, by = "ID_code", all.x = TRUE)
  }
  
  prd_result_0$Rep <- rep(n, nrow(prd_result_0))
  return(prd_result_0)
}

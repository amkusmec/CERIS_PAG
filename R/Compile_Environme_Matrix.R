Compile_Envirome_Matrix <- function(exp_dir, all_env_codes) {
  env_dir <- paste(exp_dir, 'dailyEnv/', sep = '')
  envParas <- list()
  
  for (e_i in 1:length(all_env_codes)) {
    env_daily_file <- paste0(env_dir, all_env_codes[e_i], '_daily.txt')
    env_daily <- read.table(env_daily_file, head = T, sep = "\t", stringsAsFactors = F)
    params <- setdiff(names(env_daily), "env_code")
    env_daily$DAP <- 0:nrow(env_daily)
    envParas[[e_i]] <- env_daily[, c("env_code", "DAP", params)]
  }
  
  envParas <- do.call("rbind", envParas)
  return(envParas)
}

##########################                  Block 0                  ##########################

{
 ## install required packages
 if (!require(colorspace)) { install.packages("colorspace", repos = "https://cloud.r-project.org");}
 if (!require(rrBLUP)) { install.packages("rrBLUP", repos = "https://cloud.r-project.org");}
 ###
 cwd <- '/content/CERIS_PAG/'; #### Modify this for your own directory
 subfunction_file <- paste(cwd, 'Sub_functions_PAG.r', sep = '');
 source(subfunction_file);
}

###############################################################################################

##########################                  Block 1                  ##########################

{
 experiment <- '1Sorghum'; ## Options: 1Sorghum; 2Idaho;  
 exp_dir <- paste(cwd, experiment, '/', sep = '')
 env_meta_file <- paste(exp_dir, 'Env_meta_table.txt', sep = ''); ## make sure the PlantingData formated as 'YYYY-MM-DD'
 env_meta_info_0 <- read.table(env_meta_file, header = T, sep = "\t", stringsAsFactors = F);
 
 if (experiment == '1Sorghum') { searching_days <- 122; trait <- 'FTgdd'};
 if (experiment == '2Idaho') { searching_days <- 150; trait <- 'GY'};
 exp_traits_file <- paste(exp_dir, 'Traits_record.txt', sep = '');
 exp_traits <- read.table(exp_traits_file, sep = "\t", header = T, stringsAsFactors = F, na.string = 'NA');
 
 all_env_codes <- unique(exp_traits$env_code);
 env_cols <- rainbow_hcl(length(all_env_codes), c = 80, l = 60, start = 0, end = 300, fixup = TRUE, alpha = 0.75);
 
 envParas_file <- paste(exp_dir, length(all_env_codes), 'Envs_envParas_DAP', searching_days, '.rds', sep = ''); 
 if ( !file.exists(envParas_file) ) { Compile_Envirome_Matrix(exp_dir, all_env_codes, envParas_file) };
 load(envParas_file);
 
 Paras <- colnames(envParas[[1]]) 
}

###############################################################################################
##########################                  Block 2                  ##########################

{
 lInd <- which(colnames(exp_traits) == 'line_code'); eInd <- which(colnames(exp_traits) == 'env_code'); tInd <- which(colnames(exp_traits) == trait);
 exp_trait_dir <- paste(exp_dir, trait,  '/',  sep = ''); if (!dir.exists(exp_trait_dir))  { dir.create(exp_trait_dir, recursive= T)};
 exp_trait <- exp_traits[,c(lInd, eInd, tInd)]; 
 
 colnames(exp_trait)[3] <- 'Yobs';
 exp_trait <- aggregate(Yobs ~  line_code + env_code, data = exp_trait, mean) ## To make sure only one phenotype record per each line in each environment
 exp_trait <- exp_trait[!is.na(exp_trait$Yobs),];

 line_codes <- unique(exp_trait$line_code); 
 env_mean_trait_0 <- na.omit(aggregate(x = exp_trait$Yobs, by = list(env_code = exp_trait$env_code), mean, na.rm = T));
 colnames(env_mean_trait_0)[2] <- 'meanY';
 env_mean_trait <- merge(env_mean_trait_0, env_meta_info_0)
 env_mean_trait <- env_mean_trait[order(env_mean_trait$meanY),];
### two figures and the correspondent output files will be saved in the trait directory;
 FW_Model(exp_trait, exp_trait_dir, trait, all_env_codes, env_mean_trait, env_meta_info_0);
}

###############################################################################################
##########################                  Block 3                  ##########################

{
 CERIS(env_mean_trait, envParas, searching_days, exp_trait_dir, trait, Paras, pop_cor_file, pop_corP_max_file)#; searching_daps, searching_daps);
}

###############################################################################################

##########################                  Block 4                  ##########################
### For dataset with SNPs, such as 1Sorghum
### Change the following three parameters for the window and environmental parameter with the strongest correlation
 if (experiment == '1Sorghum') { kPara_Name <- 'PTT'; maxR_dap1 <- 18; maxR_dap2 <- 43;};
 if (experiment == '2Idaho') { kPara_Name <- 'GDD'; maxR_dap1 <- 33; maxR_dap2 <- 74;};

{
 kpara_append <- paste(kPara_Name, maxR_dap1, '_', maxR_dap2, sep = '')
 envMeanPara_file <- paste(exp_trait_dir, trait, '_', nrow(env_mean_trait), 'Env_', kpara_append, '.txt', sep = '');
 kPara_ind <-  match(kPara_Name, Paras); 
 meanY_kPara <- Plot_Trait_mean_kPara(env_mean_trait, envParas, maxR_dap1, maxR_dap2, trait, exp_trait_dir, env_cols, kPara_Name, kPara_ind, envMeanPara_file);  

 obs_prd_file <- paste(exp_trait_dir, trait, '_', nrow(env_mean_trait), 'Env_LOO_by_Lines_', kpara_append, '.txt', sep = '');
 LOO_png_file <- paste(exp_trait_dir, trait, '_', nrow(env_mean_trait), 'Env_LOO_by_Lines_', kpara_append, '.png', sep = '');
 Slope_Intercept(meanY_kPara, exp_trait, exp_trait_dir, kpara_append, 1);
# Plot_prediction_result(obs_prd_file, all_env_code, meanY_kPara, kPara_Name, LOO_png_file, env_cols, within_env_cor_file);
}
###############################################################################################

##########################                  Block 5                  ##########################
### This block works only when SNP matrix is avaiable, i.e., 1Sorghum in the demo

{
 gFold <- 5;
 gIteration <- 1;
 SNPs_file <- paste(exp_dir, 'Genotype.txt', sep = '');
 SNPs <- read.table(SNPs_file, head = T, sep = "\t")
 One_to_3_Prediction(gFold, gIteration, SNPs, exp_trait, line_codes, meanY_kPara, kpara_append)
 One_to_4_Prediction(gFold, gIteration, SNPs, exp_trait, line_codes, meanY_kPara, kpara_append)
 Plot_crossvalidation_result(gFold, gIteration, all_env_codes, kpara_append)

}

###############################################################################################


##########################                  Block 6                  ##########################
######### run this for 2Idaho only because the number of environments is large

{
 eFold <- 10;
 eIteration <- 2;
 Enviromic_Prediction(eFold, eIteration, envParas, env_mean_trait_0, Paras, trait)
}

###############################################################################################


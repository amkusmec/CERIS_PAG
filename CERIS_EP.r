### Block 1
# system("git clone https://github.com/amkusmec/CERIS_PAG/")


### Block 2
if (!require(colorspace)) install.packages("colorspace")
if (!require(rrBLUP)) install.packages("rrBLUP")

# Color scales for plotting
col_wdw <- 25;
col_palette <- diverge_hcl(col_wdw + 1, h = c(260, 0), c = 100, l = c(50, 90), power = 1)
gray_alpha <- rgb(128, 128, 128, alpha = 35, maxColorValue = 255)
poly_alpha <- rgb(238, 130, 238, alpha = 55.5, maxColorValue = 255)


### Don't copy this block
### Block 3
# cwd <- '/content/CERIS_PAG/' # This is the location of the files cloned into your Google Drive
cwd <- "~/CERIS_PAG/" # For local testing
r_files <- list.files(paste0(cwd, "R"), "*", full.names = TRUE)
for (f in r_files) source(f)


### Block 4
experiment <- "Maize"
trait <- "PH"


### Block 5
exp_dir <- paste(cwd, experiment, '/', sep = '')

# Load the environment metadata file
# Column `PlantingDate` should be formatted "YYYY-MM-DD" for proper parsing
env_meta_file <- paste(exp_dir, 'Env_meta_table.txt', sep = '')
env_meta_info_0 <- read.table(env_meta_file, header = T, sep = "\t", stringsAsFactors = F)

# Load the phenotypic data
exp_traits_file <- paste(exp_dir, 'Traits_record.txt', sep = '')
exp_traits <- read.table(exp_traits_file, sep = "\t", header = T, stringsAsFactors = F, na.string = 'NA')


### Block 6
# Load the daily environmental data
all_env_codes <- unique(exp_traits$env_code)
env_cols <- rainbow_hcl(length(all_env_codes), c = 80, l = 60, start = 0, end = 300, fixup = TRUE, alpha = 0.75)

envParas_file <- paste0(exp_dir, length(all_env_codes), 'Envs_envParas.txt')
if ( !file.exists(envParas_file) ) {
        envParas <- Compile_Envirome_Matrix(exp_dir, all_env_codes)
} else {
        envParas <- read.table(envParas_file, sep = "\t", header = T, stringsAsFactors = F, na.string = "NA")
        if (!("DAP" %in% names(envParas))) {
                params <- setdiff(names(envParas), c("env_code", "date"))
                envParas <- split(envParas, envParas$env_code)
                envParas <- lapply(envParas, function(df) {
                        df$DAP <- 1:nrow(df)
                        df[, c("env_code", "DAP", params)]
                })
                envParas <- do.call("rbind", envParas)
        }
}

# Names of the environmental parameters
Paras <- names(envParas)[-(1:2)]


### Block 7
# Standardize the format of the trait data
lInd <- which(colnames(exp_traits) == 'line_code') 
eInd <- which(colnames(exp_traits) == 'env_code') 
tInd <- which(colnames(exp_traits) == trait)

exp_trait <- exp_traits[, c(lInd, eInd, tInd)] 
colnames(exp_trait)[3] <- 'Yobs'

# Average across replicates within environments to reduce to one observation
# per line per environment
exp_trait <- aggregate(Yobs ~ line_code + env_code, exp_trait, mean, na.rm = TRUE)
exp_trait <- exp_trait[!is.na(exp_trait$Yobs), ]

line_codes <- unique(exp_trait$line_code)

# Calculate the environmental mean phenotype
env_mean_trait_0 <- aggregate(Yobs ~ env_code, exp_trait, mean, na.rm = TRUE)
colnames(env_mean_trait_0)[2] <- 'meanY'
env_mean_trait <- merge(env_mean_trait_0, env_meta_info_0)
env_mean_trait <- env_mean_trait[order(env_mean_trait$meanY), ]

# Reformat the phenotypic data
env_codes <- env_mean_trait$env_code
line_by_env_df <- data.frame(line_code = line_codes)
for (e_i in 1:nrow(env_mean_trait)) {
        e <- env_codes[e_i]
        e_trait <- subset(exp_trait, exp_trait$env_code == e)
        nonNAs <- length(which(!is.na(e_trait[, 3])))
        colnames(e_trait)[3] <- e
        line_by_env_df <- merge(line_by_env_df, e_trait[, c(1, 3)], all.x = T)
}


### Block 8
# Plot two different orderings of the environments
layout(matrix(1:2, ncol = 2))

# First, ordered by latitude, longitude, and planting date
plot_geoOrder(env_mean_trait, env_meta_info_0, line_by_env_df, trait)
mtext('A', side = 3, at = 1)

# Second, ordered by the environmental mean phenotype
plot_envMeans(env_mean_trait, line_by_env_df, trait)
mtext('B', side = 3, at = min(env_mean_trait$meanY)) 


### Block 9
# FW and plots
fw_res <- FW_Model(line_by_env_df, env_mean_trait)
plot_FWResults(env_mean_trait, line_by_env_df, fw_res, trait)





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


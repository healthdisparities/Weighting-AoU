# This code will run through each step for the uniwehgted and weighted GWAS conducted in our study. Note this demo will require downloading and loading the LDAK package to your workspace.

# IF YOU ARE GOING TO RUN THIS: Run in the background using Notebook 3a, this code takes a substantial amount of time to run

# This will also require prepartion of the workspace. In the Jupyter workspace create the following folder hierarchy. Arrows indicate a sub-folder wihtin the previous one.

# (Main folder) wGWAS

# -> (Folder) data

# -> (Files) Genomic data (found in bucket <bucket>/notebooks/data/ACAF_<ancestry group>)

# -> (Folder) output

# -> (File) covarsLDAK (found in bucket <bucket>/notebooks/data/covarsLDAK)
# -> (File) phenoLDAK (found in bucket <bucket>/notebooks/data/phenoLDAK)
# -> (File) weightsLDAK (found in bucket <bucket>/notebooks/data/weightsLDAK)

# -> (Folder) programs

# -> (File) LDAK linux program file

# Prep workspace
my_bucket <- Sys.getenv('WORKSPACE_BUCKET')
pwd <- system("pwd",intern=T) # Get current working directory
PFOLDER=paste0(pwd,"/wGWAS")
list.of.packages <- c("tidyverse",
"bigrquery",
"viridis",
"ggthemes",
"scales",
"skimr",
"lubridate",
"reshape2",
"glmnet",
"survey",
"svyweight",
"weights",
"fastDummies",
"Hmisc",
"ggplot2",
"patchwork",
"nnet",
"wCorr",
"rcompanion",
"data.table",
"readr",
"qqman",
"dplyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

# European Ancestry GWAS
# Run the Unweighted and Weighted GWAS
system(paste0("chmod a+x ",PFOLDER, "/programs/ldak5.2.linux")) # Give permissions to LDAK

chromosomes <- c(1:22) # For all 22 chromosomes

for(chr_num in chromosomes){
    
    #Save file name
    extract_file <- paste0("extractedChr",chr_num,".EuropeanCohort.PostQC") # Save the name of the files for x chromosome
    
    #Copy the file from the bucket to the workspace
    system(paste0("gsutil cp ", my_bucket, "/notebooks/data/ACAF_E/",extract_file,".bed ",PFOLDER,"/data"), intern=T) # Copy genetic data from bucket to workspace
    system(paste0("gsutil cp ", my_bucket, "/notebooks/data/ACAF_E/",extract_file,".bim ",PFOLDER,"/data"), intern=T) # Copy genetic data from bucket to workspace
    system(paste0("gsutil cp ", my_bucket, "/notebooks/data/ACAF_E/",extract_file,".fam ",PFOLDER,"/data"), intern=T) # Copy genetic data from bucket to workspace
    
    #Run LDAK
    
    # Unweighted GWAS
    # /home/jupyter/workspaces/allofusweightingstudydemo/wGWAS/programs/ldak5.2.linux ## Load LDAK
    # --linear /home/jupyter/workspaces/allofusweightingstudydemo/wGWAS/output/European/t2d_e_uw_X ## Run linear GWAS regression
    # --pheno /home/jupyter/workspaces/allofusweightingstudydemo/wGWAS/output/phenoLDAK ## Phenotype file (T2D cases)
    # --covar /home/jupyter/workspaces/allofusweightingstudydemo/wGWAS/output/covarsLDAK ## Covariate file (PC1-5, age, sex at birth)
    # --bfile /home/jupyter/workspaces/allofusweightingstudydemo/wGWAS/data/extractedChrX.EuropeanCohort.PostQC ## Genomic data file
    system(paste0(PFOLDER, "/programs/ldak5.2.linux --linear ",  PFOLDER, "/output/European/asthma_e_uw_",chr_num, " --pheno ", PFOLDER, "/output/phenoAsthmaLDAK --covar ",PFOLDER,"/output/covarsLDAK --bfile ",PFOLDER, "/data/",extract_file))
    
    # Weighted GWAS
    # /home/jupyter/workspaces/allofusweightingstudydemo/wGWAS/programs/ldak5.2.linux ## Load LDAK
    # --linear /home/jupyter/workspaces/allofusweightingstudydemo/wGWAS/output/European/t2d_e_w_X ## Run linear GWAS regression
    # --pheno /home/jupyter/workspaces/allofusweightingstudydemo/wGWAS/output/phenoLDAK ## Phenotype file (T2D cases)
    # --sandwich YES ## Use sandwich estimator for weights
    # --sample-weights /home/jupyter/workspaces/allofusweightingstudydemo/wGWAS/output/weightsLDAK ## Weight file
    # --covar /home/jupyter/workspaces/allofusweightingstudydemo/wGWAS/output/covarsLDAK ## Covariate file (PC1-5, age, sex at birth)
    # --bfile /home/jupyter/workspaces/allofusweightingstudydemo/wGWAS/data/extractedChrX.EuropeanCohort.PostQC ## Genomic data file
    system(paste0(PFOLDER, "/programs/ldak5.2.linux --linear ",  PFOLDER, "/output/European/asthma_e_w_",chr_num, " --pheno ", PFOLDER, "/output/phenoAsthmaLDAK --sandwich YES --sample-weights ", PFOLDER, "/output/weightsLDAK --covar ",PFOLDER,"/output/covarsLDAK --bfile ",PFOLDER, "/data/",extract_file))
    
    # Delete genomic data after run (it is still in bucket and can be redownloaded, but this is needed for space saving purposes)
    file.remove(paste0(PFOLDER,"/data/",extract_file,".bed")) 
    file.remove(paste0(PFOLDER,"/data/",extract_file,".bim"))
    file.remove(paste0(PFOLDER,"/data/",extract_file,".fam"))
    file.remove(paste0(PFOLDER,"/data/",extract_file,".log"))
    
    system(paste0("rm -rf /home/jupyter/.local/share/Trash/*")) # Clear trash
    
}

# Organize summary statistics from GWAS
gwa_e_uw_full <- data.frame()
gwa_e_w_full <- data.frame()

chromosomes <- c(1:22) # For all 22 chromosomes

for(chr_num in chromosomes){ # For each chromosome
    # European Unweighted
    effect_e_uw=fread(paste0(PFOLDER, "/output/European/asthma_e_uw_",chr_num,".assoc"), header=T) # Load association file generate from LDAK
    gwa_e_uw=data.frame(SNP=effect_e_uw$Predictor, CHR=effect_e_uw$Chromosome, POS=effect_e_uw$Basepair, BETA=effect_e_uw$Effect, SE=effect_e_uw$SD) # Save SNP, CHR, POS, BETA, SE
    pval_e_uw=fread(paste0(PFOLDER, "/output/European/asthma_e_uw_",chr_num,".pvalues"), header=T) # Load p-values generated
    gwa_e_uw$pvalue=pval_e_uw$P # Save p-values
    
    # European Weighted
    effect_e_w=fread(paste0(PFOLDER, "/output/European/asthma_e_w_",chr_num,".assoc"), header=T) # Load association file generate from LDAK
    gwa_e_w=data.frame(SNP=effect_e_w$Predictor, CHR=effect_e_w$Chromosome, POS=effect_e_w$Basepair, BETA=effect_e_w$Effect, SE=effect_e_w$SD) # Save SNP, CHR, POS, BETA, SE
    pval_e_w=fread(paste0(PFOLDER, "/output/European/asthma_e_w_",chr_num,".pvalues"), header=T) # Load p-values generated
    gwa_e_w$pvalue=pval_e_w$P # Save p-values
    
    gwa_e_uw_full <- rbind(gwa_e_uw_full,gwa_e_uw)
    gwa_e_w_full <- rbind(gwa_e_w_full,gwa_e_w)
}
# This snippet assumes that you run setup first

# This code saves your dataframe into a csv file in a "data" folder in Google Bucket

# Replace df with THE NAME OF YOUR DATAFRAME
my_dataframe <- gwa_e_uw_full

# Replace 'test.csv' with THE NAME of the file you're going to store in the bucket (don't delete the quotation marks)
destination_filename <- 'gwa_e_uw_asthma.csv'

########################################################################
##
################# DON'T CHANGE FROM HERE ###############################
##
########################################################################

# store the dataframe in current workspace
write_excel_csv(my_dataframe, destination_filename)

# Get the bucket name
my_bucket <- Sys.getenv('WORKSPACE_BUCKET')

# Copy the file from current workspace to the bucket
system(paste0("gsutil cp ./", destination_filename, " ", my_bucket, "/data/"), intern=T)

# Check if file is in the bucket
system(paste0("gsutil ls ", my_bucket, "/data/*.csv"), intern=T)
# This snippet assumes that you run setup first

# This code saves your dataframe into a csv file in a "data" folder in Google Bucket

# Replace df with THE NAME OF YOUR DATAFRAME
my_dataframe <- gwa_e_w_full

# Replace 'test.csv' with THE NAME of the file you're going to store in the bucket (don't delete the quotation marks)
destination_filename <- 'gwa_e_w_asthma.csv'

########################################################################
##
################# DON'T CHANGE FROM HERE ###############################
##
########################################################################

# store the dataframe in current workspace
write_excel_csv(my_dataframe, destination_filename)

# Get the bucket name
my_bucket <- Sys.getenv('WORKSPACE_BUCKET')

# Copy the file from current workspace to the bucket
system(paste0("gsutil cp ./", destination_filename, " ", my_bucket, "/data/"), intern=T)

# Check if file is in the bucket
system(paste0("gsutil ls ", my_bucket, "/data/*.csv"), intern=T)

# African Ancestry GWAS
# Run the Unweighted and Weighted GWAS
system(paste0("chmod a+x ",PFOLDER, "/programs/ldak5.2.linux")) # Give permissions to LDAK

chromosomes <- c(1:22)

for(chr_num in chromosomes){
    
    #Save file name
    extract_file <- paste0("extractedChr",chr_num,".AfricanCohort.PostQC")
    
    #Copy the file from the bucket to the workspace
    system(paste0("gsutil cp ", my_bucket, "/notebooks/data/ACAF_A/",extract_file,".bed ",PFOLDER,"/data"), intern=T) # Copy genetic data from bucket to workspace
    system(paste0("gsutil cp ", my_bucket, "/notebooks/data/ACAF_A/",extract_file,".bim ",PFOLDER,"/data"), intern=T) # Copy genetic data from bucket to workspace
    system(paste0("gsutil cp ", my_bucket, "/notebooks/data/ACAF_A/",extract_file,".fam ",PFOLDER,"/data"), intern=T) # Copy genetic data from bucket to workspace
    
    #Run LDAK
    
    # Unweighted GWAS
    # /home/jupyter/workspaces/allofusweightingstudydemo/wGWAS/programs/ldak5.2.linux ## Load LDAK
    # --linear /home/jupyter/workspaces/allofusweightingstudydemo/wGWAS/output/European/t2d_a_uw_X ## Run linear GWAS regression
    # --pheno /home/jupyter/workspaces/allofusweightingstudydemo/wGWAS/output/phenoLDAK ## Phenotype file (T2D cases)
    # --covar /home/jupyter/workspaces/allofusweightingstudydemo/wGWAS/output/covarsLDAK ## Covariate file (PC1-5, age, sex at birth)
    # --bfile /home/jupyter/workspaces/allofusweightingstudydemo/wGWAS/data/extractedChrX.AfricanCohort.PostQC ## Genomic data file
    system(paste0(PFOLDER, "/programs/ldak5.2.linux --linear ",  PFOLDER, "/output/African/asthma_a_uw_",chr_num, " --pheno ", PFOLDER, "/output/phenoAsthmaLDAK --covar ",PFOLDER,"/output/covarsLDAK --bfile ",PFOLDER, "/data/",extract_file))
    
    # Weighted GWAS
    # /home/jupyter/workspaces/allofusweightingstudydemo/wGWAS/programs/ldak5.2.linux ## Load LDAK
    # --linear /home/jupyter/workspaces/allofusweightingstudydemo/wGWAS/output/European/t2d_a_w_X ## Run linear GWAS regression
    # --pheno /home/jupyter/workspaces/allofusweightingstudydemo/wGWAS/output/phenoLDAK ## Phenotype file (T2D cases)
    # --sandwich YES ## Use sandwich estimator for weights
    # --sample-weights /home/jupyter/workspaces/allofusweightingstudydemo/wGWAS/output/weightsLDAK ## Weight file
    # --covar /home/jupyter/workspaces/allofusweightingstudydemo/wGWAS/output/covarsLDAK ## Covariate file (PC1-5, age, sex at birth)
    # --bfile /home/jupyter/workspaces/allofusweightingstudydemo/wGWAS/data/extractedChrX.AfricanCohort.PostQC ## Genomic data file
    system(paste0(PFOLDER, "/programs/ldak5.2.linux --linear ",  PFOLDER, "/output/African/asthma_a_w_",chr_num, " --pheno ", PFOLDER, "/output/phenoAsthmaLDAK --sandwich YES --sample-weights ", PFOLDER, "/output/weightsLDAK --covar ",PFOLDER,"/output/covarsLDAK --bfile ",PFOLDER, "/data/",extract_file))
    
    # Delete genomic data after run (it is still in bucket and can be redownloaded, but this is needed for space saving purposes)
    file.remove(paste0(PFOLDER,"/data/",extract_file,".bed"))
    file.remove(paste0(PFOLDER,"/data/",extract_file,".bim"))
    file.remove(paste0(PFOLDER,"/data/",extract_file,".fam"))
    file.remove(paste0(PFOLDER,"/data/",extract_file,".log"))
    
    system(paste0("rm -rf /home/jupyter/.local/share/Trash/*")) # Clear trash
    
}

# Organize summary statistics from GWAS
gwa_a_uw_full <- data.frame()
gwa_a_w_full <- data.frame()

chromosomes <- c(1:22)

for(chr_num in chromosomes){ # For each chromosome
    # African Unweighted
    effect_a_uw=fread(paste0(PFOLDER, "/output/African/asthma_a_uw_",chr_num,".assoc"), header=T) # Load association file generate from LDAK
    gwa_a_uw=data.frame(SNP=effect_a_uw$Predictor, CHR=effect_a_uw$Chromosome, POS=effect_a_uw$Basepair, BETA=effect_a_uw$Effect, SE=effect_a_uw$SD) # Save SNP, CHR, POS, BETA, SE
    pval_a_uw=fread(paste0(PFOLDER, "/output/African/asthma_a_uw_",chr_num,".pvalues"), header=T) # Load p-values generated
    gwa_a_uw$pvalue=pval_a_uw$P # Save p-values
    
    # African Weighted
    effect_a_w=fread(paste0(PFOLDER, "/output/African/asthma_a_w_",chr_num,".assoc"), header=T) # Load association file generate from LDAK
    gwa_a_w=data.frame(SNP=effect_a_w$Predictor, CHR=effect_a_w$Chromosome, POS=effect_a_w$Basepair, BETA=effect_a_w$Effect, SE=effect_a_w$SD) # Save SNP, CHR, POS, BETA, SE
    pval_a_w=fread(paste0(PFOLDER, "/output/African/asthma_a_w_",chr_num,".pvalues"), header=T) # Load p-values generated
    gwa_a_w$pvalue=pval_a_w$P # Save p-values
    
    gwa_a_uw_full <- rbind(gwa_a_uw_full,gwa_a_uw)
    gwa_a_w_full <- rbind(gwa_a_w_full,gwa_a_w)
}
# This snippet assumes that you run setup first

# This code saves your dataframe into a csv file in a "data" folder in Google Bucket

# Replace df with THE NAME OF YOUR DATAFRAME
my_dataframe <- gwa_a_uw_full

# Replace 'test.csv' with THE NAME of the file you're going to store in the bucket (don't delete the quotation marks)
destination_filename <- 'gwa_a_uw_asthma.csv'

########################################################################
##
################# DON'T CHANGE FROM HERE ###############################
##
########################################################################

# store the dataframe in current workspace
write_excel_csv(my_dataframe, destination_filename)

# Get the bucket name
my_bucket <- Sys.getenv('WORKSPACE_BUCKET')

# Copy the file from current workspace to the bucket
system(paste0("gsutil cp ./", destination_filename, " ", my_bucket, "/data/"), intern=T)

# Check if file is in the bucket
system(paste0("gsutil ls ", my_bucket, "/data/*.csv"), intern=T)
# This snippet assumes that you run setup first

# This code saves your dataframe into a csv file in a "data" folder in Google Bucket

# Replace df with THE NAME OF YOUR DATAFRAME
my_dataframe <- gwa_a_w_full

# Replace 'test.csv' with THE NAME of the file you're going to store in the bucket (don't delete the quotation marks)
destination_filename <- 'gwa_a_w_asthma.csv'

########################################################################
##
################# DON'T CHANGE FROM HERE ###############################
##
########################################################################

# store the dataframe in current workspace
write_excel_csv(my_dataframe, destination_filename)

# Get the bucket name
my_bucket <- Sys.getenv('WORKSPACE_BUCKET')

# Copy the file from current workspace to the bucket
system(paste0("gsutil cp ./", destination_filename, " ", my_bucket, "/data/"), intern=T)

# Check if file is in the bucket
system(paste0("gsutil ls ", my_bucket, "/data/*.csv"), intern=T)

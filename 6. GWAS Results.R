# Prep workspace
PFOLDER="/home/jupyter/workspaces/allofusweightingstudynew/wGWAS"
my_bucket <- Sys.getenv('WORKSPACE_BUCKET')
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
"hudson",
"dplyr",
"UpSetR",
"ComplexUpset")

# YOU MAY NEED TO DOWNLOAD THIS TO GENERATE MIAMI PLOTS
# devtools::install_github('anastasia-lucas/hudson')
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

# European Ancestry GWAS Analysis
# Load European Ancestry Results
# This snippet assumes that you run setup first

# This code copies a file from your Google Bucket into a dataframe

# replace 'test.csv' with the name of the file in your google bucket (don't delete the quotation marks)
name_of_file_in_bucket <- 'gwa_e_uw_full.csv'

########################################################################
##
################# DON'T CHANGE FROM HERE ###############################
##
########################################################################

# Get the bucket name
my_bucket <- Sys.getenv('WORKSPACE_BUCKET')

# Copy the file from current workspace to the bucket
system(paste0("gsutil cp ", my_bucket, "/notebooks/data/", name_of_file_in_bucket, " ."), intern=T)

# Load the file into a dataframe
gwa_e_uw_full  <- read_csv(name_of_file_in_bucket)
head(gwa_e_uw_full)
# This snippet assumes that you run setup first

# This code copies a file from your Google Bucket into a dataframe

# replace 'test.csv' with the name of the file in your google bucket (don't delete the quotation marks)
name_of_file_in_bucket <- 'gwa_e_w_full.csv'

########################################################################
##
################# DON'T CHANGE FROM HERE ###############################
##
########################################################################

# Get the bucket name
my_bucket <- Sys.getenv('WORKSPACE_BUCKET')

# Copy the file from current workspace to the bucket
system(paste0("gsutil cp ", my_bucket, "/data/", name_of_file_in_bucket, " ."), intern=T)

# Load the file into a dataframe
gwa_e_w_full  <- read_csv(name_of_file_in_bucket)
head(gwa_e_w_full)

# Miami Plot
# This code generates a Miami plot for both GWAS in the European ancestry groups. Takes time to run.

manH=gmirror(top=subset(gwa_e_uw_full, select=c(SNP, CHR, POS, pvalue)), 
             bottom=subset(gwa_e_w_full, select=c(SNP, CHR, POS, pvalue)), tline=5e-8, bline=5e-8, 
             toptitle="European Ancestry Unweighted GWAS", 
             bottomtitle = "European Ancestry Weighted GWAS", 
             highlight_p = c(5e-8,5e-8),
             highlighter="darkviolet",
            background = "white",
            file="/home/jupyter/workspaces/allofusweightingstudynew/figures/european_gwas",
            res=600)

# QQ Plots
colnames(gwa_e_uw_full)[4:6] <- c("B_E_UW","SE_E_UW","P_E_UW") # Rename columns to reflect what dataset it is a part of 
colnames(gwa_e_w_full)[4:6] <- c("B_E_W","SE_E_W","P_E_W") # Rename columns to reflect what dataset it is a part of
gwa_full <- full_join(gwa_e_uw_full,gwa_e_w_full,by=c("SNP","CHR","POS")) # Join unweighted and weighted GWAS results

head(gwa_full)
o = -log10(sort(gwa_e_uw_full$P_E_UW,decreasing=FALSE)) # Observed p-value points
e = -log10( ppoints(length(gwa_e_uw_full$P_E_UW) )) # Expected p-value points
qqeuw_df <- data.frame(o=o,e=e) # Save to dataframe
ps <- gwa_e_uw_full$P_E_UW # P-values
chisq <- qchisq(1 - ps, 1) # Chi-square test
lambda <- median(chisq) / qchisq(0.5, 1) # Calculate genomic infaltion factor (lambda)
paste0("Lambda for Unweighted European Ancestry GWAS: ",lambda)
qqeuw <- ggplot(qqeuw_df,aes(x=e,y=o)) + geom_point() + theme_classic(base_size=24) + geom_abline(intercept = 0, slope = 1, color='black') + xlab(bquote(Expected -log[10](italic(p))))+ ylab(bquote(Observed -log[10](italic(p)))) +
            ggtitle("Q-Q plot for Unweighted European Ancestry GWAS") +
            xlim(0,7) + ylim(0,7)
# qqeuw
o = -log10(sort(gwa_e_w_full$P_E_W,decreasing=FALSE)) # Observed p-value points
e = -log10( ppoints(length(gwa_e_w_full$P_E_W) )) # Expected p-value points
qqew_df <- data.frame(o=o,e=e) # Save to dataframe
ps <- gwa_e_w_full$P_E_W # P-values
chisq <- qchisq(1 - ps, 1) # Chi-square test
lambda <- median(chisq) / qchisq(0.5, 1) # Calculate genomic infaltion factor (lambda)
paste0("Lambda for Weighted European Ancestry GWAS: ",lambda)
qqew <- ggplot(qqew_df,aes(x=e,y=o)) + geom_point() + theme_classic(base_size=24) + geom_abline(intercept = 0, slope = 1, color='black') + xlab(bquote(Expected -log[10](italic(p)))) + ylab(bquote(Observed -log[10](italic(p)))) +
            ggtitle("Q-Q plot for Weighted European Ancestry GWAS") +
            xlim(0,7) + ylim(0,7)
# qqew
library(patchwork)
fig3_row2 <- qqeuw + qqew + plot_layout(nrow=2,ncol=1)
# ggsave("/home/jupyter/workspaces/allofusweightingstudynew/figures/f3_r2.png",fig3_row2,width=6.28,height=12.56,dpi=600,unit="in")

# Betas, p-values, SE
gwa_full$P10_E_UW <- -log10(gwa_full$P_E_UW) # -log10 the p-values
gwa_full$P10_E_W <- -log10(gwa_full$P_E_W) # -log10 the p-values
summary(lm(B_E_W ~ B_E_UW, data=gwa_full)) # Summarize regression of weighted and unweighted beta values
european_betas <- ggplot(gwa_full,aes(x=B_E_UW,y=B_E_W)) + 
                    geom_point(color="lightgray") + 
                    geom_smooth(method = "lm",se=TRUE,col="black") + 
                    geom_abline(slope=1, intercept=0, linetype='dashed', col = 'red',linewidth=1) + 
                    theme_classic(base_size=24) +
                    ggtitle("European GWAS Analyses") +
                    xlab("Unweighted Beta Value (|\u03b2|)") +
                    ylab("Weighted Beta Value (|\u03b2|)")

# european_betas
ggsave("/home/jupyter/workspaces/allofusweightingstudynew/figures/european_betas.png",european_betas,width=14,height=10,dpi=600,unit="in")
summary(lm(SE_E_W ~ SE_E_UW, data=gwa_full)) # Summarize regression of weighted and unweighted standard errors
european_ses <- ggplot(gwa_full,aes(x=SE_E_UW,y=SE_E_W)) + 
                    geom_point(color="lightgray") + 
                    geom_smooth(method = "lm",se=TRUE,col="black") + 
                    geom_abline(slope=1, intercept=0, linetype='dashed', col = 'red',linewidth=1) + 
                    theme_classic(base_size=24) +
                    ggtitle("Standard Error for European GWAS Analyses") +
                    xlab("Unweighted Standard Error") +
                    ylab("Weighted Standard Error")

# european_ses
ggsave("/home/jupyter/workspaces/allofusweightingstudynew/figures/european_ses.png",european_ses,width=14,height=10,dpi=600,unit="in")
summary(lm(P10_E_W ~ P10_E_UW, data=gwa_full)) # Summarize regression of weighted and unweighted -log10(p-values)
european_ps <- ggplot(gwa_full,aes(x=P10_E_UW,y=P10_E_W)) + 
                    geom_point(color="lightgray") + 
                    geom_smooth(method = "lm",se=TRUE,col="black") + 
                    geom_abline(slope=1, intercept=0, linetype='dashed', col = 'red',linewidth=1) + 
                    theme_classic(base_size=24) +
                    ggtitle("P-Values for European GWAS Analyses") +
                    xlab(bquote("Unweighted " ~ -log[10] ~ "(p-value)")) +
                    ylab(bquote("Weighted " ~ -log[10] ~ "(p-value)"))

# european_ps
ggsave("/home/jupyter/workspaces/allofusweightingstudynew/figures/european_pvals.png",european_ps,width=14,height=10,dpi=600,unit="in")

# Text summary
paste0("Significant variants in Unweighted Analysis: ",nrow(gwa_e_uw_full[gwa_e_uw_full$P_E_UW < 5e-8,]))
# paste0(gwa_e_uw_full[gwa_e_uw_full$P_E_UW < 5e-8,]$SNP)
paste0("====================================================")
paste0("Significant variants in Weighted Analysis: ",nrow(gwa_e_w_full[gwa_e_w_full$P_E_W < 5e-8,]))
# paste0(gwa_e_w_full[gwa_e_w_full$P_E_W < 5e-8,]$SNP)
sig_gwa_e_uw <- gwa_e_uw_full[gwa_e_uw_full$P_E_UW < 5e-8,][,1:3] # Significant variants from unweighted GWAS
sig_gwa_e_w <- gwa_e_w_full[gwa_e_w_full$P_E_W < 5e-8,][,1:3] # Significant variants from weighted GWAS
sig_snps_e <- unique(rbind(sig_gwa_e_uw,sig_gwa_e_w)) # Bind together the results
paste0("Significant variants in Both Analyses: ",length(intersect(sig_gwa_e_uw$SNP,sig_gwa_e_w$SNP)))

# African Ancestry GWAS Analysis
# Load African ancestry results
# This snippet assumes that you run setup first

# This code copies a file from your Google Bucket into a dataframe

# replace 'test.csv' with the name of the file in your google bucket (don't delete the quotation marks)
name_of_file_in_bucket <- 'gwa_a_uw_asthma.csv'

########################################################################
##
################# DON'T CHANGE FROM HERE ###############################
##
########################################################################

# Get the bucket name
my_bucket <- Sys.getenv('WORKSPACE_BUCKET')

# Copy the file from current workspace to the bucket
system(paste0("gsutil cp ", my_bucket, "/data/", name_of_file_in_bucket, " ."), intern=T)

# Load the file into a dataframe
gwa_a_uw_full  <- read_csv(name_of_file_in_bucket)
head(gwa_a_uw_full)
# This snippet assumes that you run setup first

# This code copies a file from your Google Bucket into a dataframe

# replace 'test.csv' with the name of the file in your google bucket (don't delete the quotation marks)
name_of_file_in_bucket <- 'gwa_a_w_asthma.csv'

########################################################################
##
################# DON'T CHANGE FROM HERE ###############################
##
########################################################################

# Get the bucket name
my_bucket <- Sys.getenv('WORKSPACE_BUCKET')

# Copy the file from current workspace to the bucket
system(paste0("gsutil cp ", my_bucket, "/data/", name_of_file_in_bucket, " ."), intern=T)

# Load the file into a dataframe
gwa_a_w_full  <- read_csv(name_of_file_in_bucket)
head(gwa_a_w_full)

# Miami Plot
# This code generates a Miami plot for both GWAS in the European ancestry groups. Takes time to run, and may require rebooting the worspace with more memory

manH=gmirror(top=subset(gwa_a_uw_full, select=c(SNP, CHR, POS, pvalue)), 
             bottom=subset(gwa_a_w_full, select=c(SNP, CHR, POS, pvalue)), tline=5e-8, bline=5e-8, 
             toptitle="Standard African Ancestry GWAS", 
             bottomtitle = "Weighted African Ancestry GWAS", 
             highlight_p = c(5e-8,5e-8),
             highlighter="darkviolet",
            background = "white",
            file="/home/jupyter/workspaces/allofusweightingstudynew/figures/african_gwas",
            res=600)

# QQ Plots
colnames(gwa_a_uw_full)[4:6] <- c("B_A_UW","SE_A_UW","P_A_UW") # Rename columns to reflect what dataset it is a part of 
colnames(gwa_a_w_full)[4:6] <- c("B_A_W","SE_A_W","P_A_W") # Rename columns to reflect what dataset it is a part of
gwa_full <- full_join(gwa_a_uw_full,gwa_a_w_full,by=c("SNP","CHR","POS")) # Join unweighted and weighted GWAS results

head(gwa_full)
o = -log10(sort(gwa_a_uw_full$P_A_UW,decreasing=FALSE)) # Observed p-value points
e = -log10( ppoints(length(gwa_a_uw_full$P_A_UW) )) # Expected p-value points
qqauw_df <- data.frame(o=o,e=e) # Combine in dataframe
ps <- gwa_a_uw_full$P_A_UW # p-values
chisq <- qchisq(1 - ps, 1) # Chi-square test
lambda <- median(chisq) / qchisq(0.5, 1) # Calculate genomic infaltion factor (lambda)
paste0("Lambda for Unweighted African Ancestry GWAS: ",lambda)
qqauw <- ggplot(qqauw_df,aes(x=e,y=o)) + geom_point() + theme_classic(base_size=24) + geom_abline(intercept = 0, slope = 1, color='black') + xlab(bquote(Expected -log[10](italic(p))))+ ylab(bquote(Observed -log[10](italic(p)))) +
            ggtitle("Q-Q plot for Unweighted African Ancestry GWAS") + xlim(0,7) + ylim(0,7)
# qqauw
o = -log10(sort(gwa_a_w_full$P_A_W,decreasing=FALSE)) # Observed p-value points
e = -log10( ppoints(length(gwa_a_w_full$P_A_W) )) # Expected p-value points
qqaw_df <- data.frame(o=o,e=e) # Combine in dataframe
ps <- gwa_a_w_full$P_A_W # p-values
chisq <- qchisq(1 - ps, 1) # Chi-square test
lambda <- median(chisq) / qchisq(0.5, 1) # Calculate genomic infaltion factor (lambda)
paste0("Lambda for Weighted African Ancestry GWAS: ",lambda)
qqaw <- ggplot(qqaw_df,aes(x=e,y=o)) + geom_point() + theme_classic(base_size=24) + geom_abline(intercept = 0, slope = 1, color='black') + xlab(bquote(Expected -log[10](italic(p)))) + ylab(bquote(Observed -log[10](italic(p)))) +
            ggtitle("Q-Q plot for Weighted African Ancestry GWAS") + xlim(0,7) + ylim(0,7)
# qqaw
library(patchwork)
fig3_row4 <- qqauw + qqaw + plot_layout(ncol=1,nrow=2)
# ggsave("/home/jupyter/workspaces/allofusweightingstudynew/figures/f3_r4.png",fig3_row4,width=6.28,height=12.56,dpi=600,unit="in")

# Betas, p-values, SE
gwa_full$P10_A_UW <- -log10(gwa_full$P_A_UW) # -log10 the p-values
gwa_full$P10_A_W <- -log10(gwa_full$P_A_W) # -log10 the p-values
summary(lm(B_A_W ~ B_A_UW, data=gwa_full)) # Summarize regression of weighted and unweighted beta values
african_betas <- ggplot(gwa_full,aes(x=B_A_UW,y=B_A_W)) + 
                    geom_point(color="lightgray") + 
                    geom_smooth(method = "lm",se=TRUE,col="black") + 
                    geom_abline(slope=1, intercept=0, linetype='dashed', col = 'red',linewidth=1) + 
                    theme_classic(base_size=24) +
                    ggtitle("Beta Values for African GWAS Analyses") +
                    xlab("Unweighted Beta Value (\u03b2)") +
                    ylab("Weighted Beta Value (\u03b2)")

# african_betas
ggsave("/home/jupyter/workspaces/allofusweightingstudynew/figures/african_betas.png",african_betas,width=14,height=10,dpi=600,unit="in")
summary(lm(SE_A_W ~ SE_A_UW, data=gwa_full)) # Summarize regression of weighted and unweighted standard errors
african_ses <- ggplot(gwa_full,aes(x=SE_A_UW,y=SE_A_W)) + 
                    geom_point(color="lightgray") + 
                    geom_smooth(method = "lm",se=TRUE,col="black") + 
                    geom_abline(slope=1, intercept=0, linetype='dashed', col = 'red',linewidth=1) + 
                    theme_classic(base_size=24) +
                    ggtitle("Standard Error for African GWAS Analyses") +
                    xlab("Unweighted Standard Error") +
                    ylab("Weighted Standard Error")

# african_ses
ggsave("/home/jupyter/workspaces/allofusweightingstudynew/figures/african_ses.png",african_ses,width=14,height=10,dpi=600,unit="in")
summary(lm(P10_A_W ~ P10_A_UW, data=gwa_full)) # Summarize regression of weighted and unweighted p-values
african_ps <- ggplot(gwa_full,aes(x=P10_A_UW,y=P10_A_W)) + 
                    geom_point(color="lightgray") + 
                    geom_smooth(method = "lm",se=TRUE,col="black") + 
                    geom_abline(slope=1, intercept=0, linetype='dashed', col = 'red',linewidth=1) + 
                    theme_classic(base_size=24) +
                    ggtitle("P-Values for African GWAS Analyses") +
                    xlab(bquote("Unweighted " ~ -log[10] ~ "(p-value)")) +
                    ylab(bquote("Weighted " ~ -log[10] ~ "(p-value)"))

# african_ps

ggsave("/home/jupyter/workspaces/allofusweightingstudynew/figures/african_pvals.png",african_ps,width=14,height=10,dpi=600,unit="in")

# Text Summary
paste0("Significant variants in Unweighted Analysis: ",nrow(gwa_a_uw_full[gwa_a_uw_full$P_A_UW < 5e-8,]))
# paste0(gwa_a_uw_full[gwa_a_uw_full$P_A_UW < 5e-8,]$SNP)
paste0("====================================================")
paste0("Significant variants in Weighted Analysis: ",nrow(gwa_a_w_full[gwa_a_w_full$P_A_W < 5e-8,]))
paste0(gwa_a_w_full[gwa_a_w_full$P_A_W < 5e-8,]$SNP)
sig_gwa_a_uw <- gwa_a_uw_full[gwa_a_uw_full$P_A_UW < 5e-8,][,1:3]
sig_gwa_a_w <- gwa_a_w_full[gwa_a_w_full$P_A_W < 5e-8,][,1:3]
sig_snps_a <- unique(rbind(sig_gwa_a_uw,sig_gwa_a_w))

paste0("Significant SNPs in Both Analyses: ",length(intersect(sig_gwa_a_uw$SNP,sig_gwa_a_w$SNP)))

# Compare all GWAS
paste0("Significant in European UW: ",length(sig_gwa_e_uw$SNP))
paste0("Significant in European W: ",length(sig_gwa_e_w$SNP))
paste0("Significant in African UW: ",length(sig_gwa_a_uw$SNP))
paste0("Significant in African W: ",length(sig_gwa_a_w$SNP))

paste0("Significant in African UW and African W: ",length(intersect(sig_gwa_a_uw$SNP,sig_gwa_a_w$SNP)))
# intersect(sig_gwa_a_uw$SNP,sig_gwa_a_w$SNP)
paste0("Significant in European UW and European W: ",length(intersect(sig_gwa_e_uw$SNP,sig_gwa_e_w$SNP)))
paste0("Significant in African UW and European UW: ",length(intersect(sig_gwa_a_uw$SNP,sig_gwa_e_uw$SNP)))
# intersect(sig_gwa_a_uw$SNP,sig_gwa_e_uw$SNP)
paste0("Significant in African W and European W: ",length(intersect(sig_gwa_a_w$SNP,sig_gwa_e_w$SNP)))
# intersect(sig_gwa_a_w$SNP,sig_gwa_e_w$SNP)

paste0("Significant in EUW/AUW/AW GWAS: ",length(Reduce(intersect, list(sig_gwa_e_uw$SNP,sig_gwa_a_uw$SNP,sig_gwa_a_w$SNP))))
paste0("Significant in AUW/AW/EW GWAS: ",length(Reduce(intersect, list(sig_gwa_a_uw$SNP,sig_gwa_a_w$SNP,sig_gwa_e_w$SNP))))
paste0("Significant in AW/EW/EUW GWAS: ",length(Reduce(intersect, list(sig_gwa_a_w$SNP,sig_gwa_e_w$SNP,sig_gwa_e_uw$SNP))))
paste0("Significant in EW/EUW/AUW GWAS: ",length(Reduce(intersect, list(sig_gwa_e_w$SNP,sig_gwa_e_uw$SNP,sig_gwa_a_uw$SNP))))

paste0("Significant in all 4 GWAS: ",length(Reduce(intersect, list(sig_gwa_e_uw$SNP,sig_gwa_e_w$SNP,sig_gwa_a_uw$SNP,sig_gwa_a_w$SNP))))
Reduce(intersect, list(sig_gwa_e_uw$SNP,sig_gwa_e_w$SNP,sig_gwa_a_uw$SNP,sig_gwa_a_w$SNP))

# UpSet Plot
gwa_e_uw <- gwa_e_uw_full[gwa_e_uw_full$P_E_UW < 5e-8,] # Save significant variants from European ancestry unweighted GWAS
gwa_e_w <- gwa_e_w_full[gwa_e_w_full$P_E_W < 5e-8,] # Save significant variants from European ancestry weighted GWAS
gwa_a_uw <- gwa_a_uw_full[gwa_a_uw_full$P_A_UW < 5e-8,] # Save significant variants from African ancestry unweighted GWAS
gwa_a_w <- gwa_a_w_full[gwa_a_w_full$P_A_W < 5e-8,] # Save significant variants from African ancestry weighted GWAS
eur_uw_set <- gwa_e_uw$SNP # Save vector of significant variants
eur_w_set <- gwa_e_w$SNP # Save vector of significant variants
afr_uw_set <- gwa_a_uw$SNP # Save vector of significant variants
afr_w_set <- gwa_a_w$SNP # Save vector of significant variants
# Save list of all vectors
snp_list <- list(`European Unweighted`=eur_uw_set,`European Weighted`=eur_w_set,`African Unweighted`=afr_uw_set,`African Weighted`=afr_w_set)
library(grid)
library(UpSetR)
# detach("package:ComplexUpset")
library(ComplexUpset)
# Create a binary matrix to show intersections
binary_matrix <- fromList(snp_list)
binary_df <- as.data.frame(binary_matrix)
upset_save <- upset(
  binary_df,
  set_sizes=FALSE,
  intersect = c('European Unweighted', 'European Weighted', 'African Unweighted', 'African Weighted'),  # Specify the sets of interest
  name = '',
  base_annotations=list('Intersection size'=intersection_size(counts=TRUE,text = list(size=8))),
    themes=upset_default_themes(text=element_text()
)) + theme_bw(base_size=24) + theme(axis.title.x = element_blank(),  # Remove x-axis title (group label)
  axis.title.y = element_blank())

upset_save

ggsave("/home/jupyter/workspaces/allofusweightingstudynew/figures/upset_plot.png",upset_save,width=16,height=10,unit="in")
upset_plot <- upset(binary_matrix, 
      nsets = length(snp_list),
      order.by = "freq",
      keep.order = TRUE,
      mainbar.y.label = "Number of Variant Intersections",
      sets.x.label = "Number of Significant Variants",
      main.bar.color="darkblue",
      sets.bar.color="darkblue")

upset_plot + theme_classic(base_size=24)

ggsave("/home/jupyter/workspaces/allofusweightingstudy/figures/upset_plot.png",upset_plot,width=16,height=10,unit="in")

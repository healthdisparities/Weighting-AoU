#Initial Data Comparison
#Prepare Data
#Merge All of Us and NHANES data
We will start by merging NHANES data and All of Us data into one dataframe. The NHANES cohort has its own weights that must be applied, and we added those in 2.2 to NHANES data. For All of Us we assign a weight of 1 to all participants to take them as is.

complete_aou_df$weight <- 1 #Assign a weight of 1 to All of Us participants
head(complete_aou_df)
combined_df <- rbind(complete_aou_df,complete_nhanes_df) # Combine NHANES and All of Us data
Create Dummy Variables
LASSO requires categorical variables be broken up into dummy variables. The fastDummies package allows us to do this.

lasso_df <- dummy_cols(combined_df,
                        select_columns=c("race","gender","highest_grade","marital_status","health_insurance",
                                         "birthplace","alcohol_final","smoking_final","overall_health"),
                        ignore_na=TRUE,
                        remove_first_dummy = TRUE,
                        remove_selected_columns = TRUE) # Convert categorical variables to dummy variables

lasso_df <- lasso_df %>%
                relocate(weight, .after=last_col()) %>%
                relocate(data_label, .after=last_col()) %>%
                relocate(participation, .after=last_col()) # Reorder columns
lasso_df <- lasso_df[complete.cases(lasso_df),] # Ensure there are only complete cases
head(lasso_df)
Run Univariate LASSO regression
Here we run the univariate LASSO regression. This will help us see how variables predict participation in All of Us

x <- as.matrix(lasso_df[2:(ncol(lasso_df)-3)]) # Matrix of predictors
y <- as.matrix(lasso_df[[ncol(lasso_df)]]) # Matrix of participation variables
w <- as.matrix(lasso_df[[(ncol(lasso_df)-2)]]) # Matrix of weights
univariate_model <- cv.glmnet(x,y,weights=w,family="binomial") # Develop model to predict participation probability
beta <- coef(univariate_model,s="lambda.min") # Extract coefficients from LASSO regression for lambda min
model_coef_uw <- as.data.frame(as.matrix(beta)) # Convert to dataframe
model_coef_uw$names <- rownames(model_coef_uw) # Save row names
model_coef_uw <- model_coef_uw[-1,] # Remove intercept value (not important for prediction)
head(model_coef_uw)
Analyze effect sizes
We standardize effect sizes for all variables in order to compare them. Without standradization the effect sizes will be on different scales and can not be easily compared.

head(lasso_df)
dim(lasso_df)
wt <- lasso_df$weight # Extract weights
sx <- sapply(lasso_df[,2:(ncol(lasso_df)-3)],function(x) sqrt(wtd.var(x,wt))) #Get standard deviation for each predictor
sy <- sqrt(wtd.var(lasso_df$participation,wt)) # Save standard deviation for participation
# sx <- sapply(lasso_df[,2:(ncol(lasso_df)-3)],function(x) sd(x)) #Get standard deviation for each predictor
# sy <- sd(lasso_df$participation) # Save standard deviation for participation
model_coef_uw$s_s1 <- ((model_coef_uw$s1)*sx)/sy # Standardize beta coefficients
model_coef_uw$label <- "Full" # Add data label
lasso_df_f <- lasso_df[lasso_df$gender_2 == 1,] # Save data for females only
lasso_df_m <- lasso_df[lasso_df$gender_2 == 0,] # Save data for males only
lasso_df_f <- lasso_df_f %>%
                    subset(select=-c(gender_2)) # Remove gender (not important since we subset ny gender)
lasso_df_m <- lasso_df_m %>%
                    subset(select=-c(gender_2)) # Remove gender (not important since we subset ny gender)
x_f <- as.matrix(lasso_df_f[2:(ncol(lasso_df_f)-3)]) # Matrix of predictors
y_f <- as.matrix(lasso_df_f[[ncol(lasso_df_f)]]) # Matrix of participation variables
w_f <- as.matrix(lasso_df_f[[(ncol(lasso_df_f)-2)]]) # Matrix of weights

x_m <- as.matrix(lasso_df_m[2:(ncol(lasso_df_m)-3)]) # Matrix of predictors
y_m <- as.matrix(lasso_df_m[[ncol(lasso_df_m)]]) # Matrix of participation variables
w_m <- as.matrix(lasso_df_m[[(ncol(lasso_df_m)-2)]]) # Matrix of weights
model_uni_og_uw_f <- cv.glmnet(x_f,y_f,weights=w_f,family="binomial") # Develop model to predict participation probability
model_uni_og_uw_m <- cv.glmnet(x_m,y_m,weights=w_m,family="binomial") # Develop model to predict participation probability
wt_f <- lasso_df_f$weight # Extract weights
sx_f <- sapply(lasso_df_f[,2:(ncol(lasso_df_f)-3)],function(x) sqrt(wtd.var(x,wt_f))) #Get standard deviation for each predictor
sy_f <- sqrt(wtd.var(lasso_df_f$participation,wt_f)) # Save standard deviation for participation

wt_m <- lasso_df_m$weight # Extract weights
sx_m <- sapply(lasso_df_m[,2:(ncol(lasso_df_m)-3)],function(x) sqrt(wtd.var(x,wt_m))) #Get standard deviation for each predictor
sy_m <- sqrt(wtd.var(lasso_df_m$participation,wt_m)) # Save standard deviation for participation
# sx_f <- sapply(lasso_df_f[,2:(ncol(lasso_df_f)-3)],function(x) sd(x)) #Get standard deviation for each predictor
# sy_f <- sd(lasso_df_f$participation) # Save standard deviation for participation

# sx_m <- sapply(lasso_df_m[,2:(ncol(lasso_df_m)-3)],function(x) sd(x)) #Get standard deviation for each predictor
# sy_m <- sd(lasso_df_m$participation) # Save standard deviation for participation
beta_f <- coef(model_uni_og_uw_f,s="lambda.min") # Extract coefficients from LASSO regression for lambda min
model_coef_uw_f <- as.data.frame(as.matrix(beta_f)) # Convert to dataframe
model_coef_uw_f$names <- rownames(model_coef_uw_f) # Save row names
model_coef_uw_f <- model_coef_uw_f[-1,]

beta_m <- coef(model_uni_og_uw_m,s="lambda.min") # Extract coefficients from LASSO regression for lambda min
model_coef_uw_m <- as.data.frame(as.matrix(beta_m)) # Convert to dataframe
model_coef_uw_m$names <- rownames(model_coef_uw_m) # Save row names
model_coef_uw_m <- model_coef_uw_m[-1,] # Remove intercept value (not important for prediction)

model_coef_uw_f$s_s1 <- ((model_coef_uw_f$s1)*sx_f)/sy_f # Standardize beta coefficients
model_coef_uw_m$s_s1 <- ((model_coef_uw_m$s1)*sx_m)/sy_m # Standardize beta coefficients

model_coef_uw_f$label <- "Female" # Add data label
model_coef_uw_m$label <- "Male" # Add data label

model_coef_uw_full <- rbind(model_coef_uw_f,model_coef_uw_m,model_coef_uw) # Combine all data
model_coef_plot_uw <- ggplot(data=model_coef_uw_full,aes(x=names,y=s_s1,color=label)) + 
                    geom_point(size=4,position=position_jitter(0.1)) +
                    scale_x_discrete(labels=c("Age","Alcohol status (Less than yearly)","Alcohol status (Monthly or less)","Alcohol status (Two to four times a week)","Alcohol status (Twice or more a week)","Birthplace (Other)","Sex at Birth (Female)","Health Insurance (Yes)","Highest grade (9-11th grade)","Highest grade (High school graduate/GED or equivalent)","Highest grade (Some college or AA degree)","Highest grade (College graduate or above)","Marital status (Widowed/Divorced/Separated)","Marital status (Never married)","Overall Health (Fair)","Overall Health (Good)","Overall Health (Very Good)","Overall Health (Excellent)","Race and Ethnicity (Black or African American)","Race and Ethnicity (Hispanic or Latino)","Race and Ethnicity (Asian)","Race and Ethnicity (Other/Multiracial)","Smoking status (Past smoker)","Smoking status (Sometimes smokes)","Smoking status (Daily smoker)")) +
                    coord_flip() +
                    geom_hline(yintercept=0, linetype='dotted', col = 'red') +
                    xlab("") + ylab("Standardized Beta Value (\u03b2)") + 
                    scale_y_continuous(limits=c(-12.5,30)) +
#                     theme_classic(base_size=24) +
                    guides(color="none") +
                    #scale_color_discrete(breaks=c("Full","Male","Female")) +
                    scale_color_manual(values=c("darkgreen","steelblue3","mediumpurple2"),breaks=c("Full","Male","Female")) +
                    theme(legend.position = "top") + labs(color="") + theme(legend.position = "none")
#                     options(repr.plot.width=4)

model_coef_plot_uw
Develop Weights
Prepare Data for LASSO
Merge All of Us and NHANES data
We will start by merging NHANES data and All of Us data into one dataframe. The NHANES cohort has its own weights that must be applied, and we added those in 2.2 to NHANES data. For All of Us we assign a weight of 1 to all participants to take them as is.

complete_aou_df$weight <- 1 #Assign a weight of 1 to All of Us participants
head(complete_aou_df)
combined_df <- rbind(complete_aou_df,complete_nhanes_df) # Combine NHANES and All of Us data
Create Dummy Variables
LASSO requires categorical variables be broken up into dummy variables. The fastDummies package allows us to do this.

lasso_df <- dummy_cols(combined_df,
                        select_columns=c("race","gender","highest_grade","marital_status","health_insurance",
                                         "birthplace","alcohol_final","smoking_final","overall_health"),
                        ignore_na=TRUE,
                        remove_first_dummy = TRUE,
                        remove_selected_columns = TRUE) # Convert categorical variables to dummy variables

lasso_df <- lasso_df %>%
                relocate(weight, .after=last_col()) %>%
                relocate(data_label, .after=last_col()) %>%
                relocate(participation, .after=last_col()) # Reorder columns
lasso_df <- lasso_df[complete.cases(lasso_df),] # Ensure there are only complete cases
head(lasso_df)
Develop LASSO IP weight model
To develop weights we break up the data into matrices. To capture the full breadth of the correlation structure in the data, we include all possible 2-way interactions

f <- as.formula(~ .*.) # Formula to ensure all 2 way interactions are added
x <- model.matrix(f,lasso_df[2:(ncol(lasso_df)-3)])[,-1] # Matrix of predictors
y <- as.matrix(lasso_df[[ncol(lasso_df)]]) # Matrix of participation status
w <- as.matrix(lasso_df[[(ncol(lasso_df)-2)]]) # Matrix of weights
model_int_og <- cv.glmnet(x,y,weights=w,family="binomial") # Build model to calculate weights
plot(model_int_og)
plot(model_int_og$glmnet.fit,"lambda",labels=TRUE)
Predict participation probability
Using the model above, we predict participation probability for each person.

aou_dummy_df <- dummy_cols(complete_aou_df,
                                select_columns=c("race","gender","highest_grade","marital_status","health_insurance",
                                                       "birthplace","alcohol_final","smoking_final","overall_health"),
                                ignore_na=TRUE,
                                remove_first_dummy = TRUE,
                                remove_selected_columns = TRUE,
                                omit_colname_prefix = TRUE) # For only the All of Us dataset, convert to dummy variables

aou_dummy_df <- aou_dummy_df %>%
                relocate(weight, .after=last_col()) %>%
                relocate(data_label, .after=last_col()) %>%
                relocate(participation, .after=last_col()) # Reorder variables
head(aou_dummy_df)
f <- as.formula(~ .*.) # Formula to create all 2 way interactions
predict_x <- model.matrix(f,aou_dummy_df[2:(ncol(aou_dummy_df)-3)])[,-1] # Matrix of predictors
Calculate Weights
Finally we calculate IP weights using the formula (1-Pi)/Pi where Pi is participation probability.

Note: LASSO does not return the exact same coefficients every time (but they are still very close), thus when calculating weights your range may be slightly different than the range listed in the paper. The weights are still effective in reducing participation bias, and their effectiveness is further tested in "5 Validate Weights"

predict_aou <- aou_dummy_df # Temporary dataframe to calculate IP weights
head(predict_x)
predict_aou$predict_prob <- predict(model_int_og,newx=predict_x,s="lambda.min",type="response") # Get participation probability (Pi)
predict_aou$ip <- 1 - predict_aou$predict_prob # Top of formula (1 - Pi)
predict_aou$w_ip <- predict_aou$ip/predict_aou$predict_prob # Calculate IP weight (1 - Pi)/Pi
predict_aou$w_ip <- as.double(predict_aou$w_ip) # Convert to number
dim(predict_aou)
predict_probs <- predict_aou %>% subset(select=c(id,predict_prob))
predict_probs$predict_prob <- as.double(predict_probs$predict_prob)
# This snippet assumes that you run setup first

# This code saves your dataframe into a csv file in a "data" folder in Google Bucket

# Replace df with THE NAME OF YOUR DATAFRAME
my_dataframe <- predict_probs # Save participation probability

# Replace 'test.csv' with THE NAME of the file you're going to store in the bucket (don't delete the quotation marks)
destination_filename <- 'predict_probs.csv'

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
aou_weights <- predict_aou %>%
                subset(select=c(id,w_ip)) # Save only ID and IP weights
aou_weights$weight <- (aou_weights$w_ip)/mean(aou_weights$w_ip) # Normalize weights
aou_weights <- aou_weights %>%
                subset(select=c(id,weight)) # Save only ID and normalized IP weights
# This snippet assumes that you run setup first

# This code saves your dataframe into a csv file in a "data" folder in Google Bucket

# Replace df with THE NAME OF YOUR DATAFRAME
my_dataframe <- aou_weights

# Replace 'test.csv' with THE NAME of the file you're going to store in the bucket (don't delete the quotation marks)
destination_filename <- 'aou_weights_calculated_2.csv'

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
complete_aou_df$weight <- aou_weights$weight
head(complete_aou_df)
See weight distribution
# This snippet assumes that you run setup first

# This code copies a file from your Google Bucket into a dataframe

# replace 'test.csv' with the name of the file in your google bucket (don't delete the quotation marks)
name_of_file_in_bucket <- 'aou_weights_calculated_oh.csv'

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
aou_weights  <- read_csv(name_of_file_in_bucket)
head(aou_weights)
complete_aou_df$weight <- aou_weights$weight
density_plot <- ggplot(aou_weights,aes(x=weight,y=after_stat(scaled))) +
    geom_density(fill="aliceblue",linewidth=1) +
    theme_classic(base_size=24) +
    scale_x_continuous(limits=c(0,10)) +
    scale_y_continuous(breaks=c(0,0.3,0.6,0.9)) + 
    xlab("Normalized IP Weights") +
    ylab("Density")

density_plot
Validate Weights
Merge All of Us and NHANES data
We will start by merging NHANES data and All of Us data into one dataframe. The NHANES cohort has its own weights that must be applied, and we added those in 2.2 to NHANES data. For All of Us we assign a weight of 1 to all participants to take them as is.

combined_df <- rbind(complete_aou_df,complete_nhanes_df) # Combine NHANES and All of Us data
Create Dummy Variables
LASSO requires categorical variables be broken up into dummy variables. The fastDummies package allows us to do this.

lasso_df <- dummy_cols(combined_df,
                        select_columns=c("race","gender","highest_grade","marital_status","health_insurance",
                                         "birthplace","alcohol_final","smoking_final","overall_health"),
                        ignore_na=TRUE,
                        remove_first_dummy = TRUE,
                        remove_selected_columns = TRUE) # Convert categorical variables to dummy variables

lasso_df <- lasso_df %>%
                relocate(weight, .after=last_col()) %>%
                relocate(data_label, .after=last_col()) %>%
                relocate(participation, .after=last_col()) # Reorder columns
lasso_df <- lasso_df[complete.cases(lasso_df),] # Ensure there are only complete cases
head(lasso_df)
Run Univariate LASSO regression
Here we run the univariate LASSO regression. This will help us see how variables predict participation in All of Us

x <- as.matrix(lasso_df[2:(ncol(lasso_df)-3)]) # Matrix of predictors
y <- as.matrix(lasso_df[[ncol(lasso_df)]]) # Matrix of participation variables
w <- as.matrix(lasso_df[[(ncol(lasso_df)-2)]]) # Matrix of weights
univariate_model <- cv.glmnet(x,y,weights=w,family="binomial") # Develop model to predict participation probability
beta <- coef(univariate_model,s="lambda.min") # Extract coefficients from LASSO regression for lambda min
model_coef_w <- as.data.frame(as.matrix(beta)) # Convert to dataframe
model_coef_w$names <- rownames(model_coef_w) # Save row names
model_coef_w <- model_coef_w[-1,] # Remove intercept value (not important for prediction)
head(model_coef_w)
Analyze effect sizes
We standardize effect sizes for all variables in order to compare them. Without standradization the effect sizes will be on different scales and can not be easily compared.

wt <- lasso_df$weight # Extract weights
sx <- sapply(lasso_df[,2:(ncol(lasso_df)-3)],function(x) sqrt(wtd.var(x,wt))) #Get standard deviation for each predictor
sy <- sqrt(wtd.var(lasso_df$participation,wt)) # Save standard deviation for participation
# sx <- sapply(lasso_df[,2:(ncol(lasso_df)-3)],function(x) sd(x)) #Get standard deviation for each predictor
# sy <- sd(lasso_df$participation) # Save standard deviation for participation
model_coef_w$s_s1 <- ((model_coef_w$s1)*sx)/sy # Standardize beta coefficients
model_coef_w$label <- "Full" # Add data label
lasso_df_f <- lasso_df[lasso_df$gender_2 == 1,] # Save data for females only
lasso_df_m <- lasso_df[lasso_df$gender_2 == 0,] # Save data for males only
lasso_df_f <- lasso_df_f %>%
                    subset(select=-c(gender_2)) # Remove gender (not important since we subset ny gender)
lasso_df_m <- lasso_df_m %>%
                    subset(select=-c(gender_2)) # Remove gender (not important since we subset ny gender)
x_f <- as.matrix(lasso_df_f[2:(ncol(lasso_df_f)-3)]) # Matrix of predictors
y_f <- as.matrix(lasso_df_f[[ncol(lasso_df_f)]]) # Matrix of participation variables
w_f <- as.matrix(lasso_df_f[[(ncol(lasso_df_f)-2)]]) # Matrix of weights

x_m <- as.matrix(lasso_df_m[2:(ncol(lasso_df_m)-3)]) # Matrix of predictors
y_m <- as.matrix(lasso_df_m[[ncol(lasso_df_m)]]) # Matrix of participation variables
w_m <- as.matrix(lasso_df_m[[(ncol(lasso_df_m)-2)]]) # Matrix of weights
model_uni_og_w_f <- cv.glmnet(x_f,y_f,weights=w_f,family="binomial") # Develop model to predict participation probability
model_uni_og_w_m <- cv.glmnet(x_m,y_m,weights=w_m,family="binomial") # Develop model to predict participation probability
wt_f <- lasso_df_f$weight # Extract weights
sx_f <- sapply(lasso_df_f[,2:(ncol(lasso_df_f)-3)],function(x) sqrt(wtd.var(x,wt_f))) #Get standard deviation for each predictor
sy_f <- sqrt(wtd.var(lasso_df_f$participation,wt_f)) # Save standard deviation for participation

wt_m <- lasso_df_m$weight # Extract weights
sx_m <- sapply(lasso_df_m[,2:(ncol(lasso_df_m)-3)],function(x) sqrt(wtd.var(x,wt_m))) #Get standard deviation for each predictor
sy_m <- sqrt(wtd.var(lasso_df_m$participation,wt_m)) # Save standard deviation for participation
sx_f <- sapply(lasso_df_f[,2:(ncol(lasso_df_f)-3)],function(x) sd(x)) #Get standard deviation for each predictor
sy_f <- sd(lasso_df_f$participation) # Save standard deviation for participation

sx_m <- sapply(lasso_df_m[,2:(ncol(lasso_df_m)-3)],function(x) sd(x)) #Get standard deviation for each predictor
sy_m <- sd(lasso_df_m$participation) # Save standard deviation for participation
beta_f <- coef(model_uni_og_w_f,s="lambda.min") # Extract coefficients from LASSO regression for lambda min
model_coef_w_f <- as.data.frame(as.matrix(beta_f)) # Convert to dataframe
model_coef_w_f$names <- rownames(model_coef_w_f) # Save row names
model_coef_w_f <- model_coef_w_f[-1,]

beta_m <- coef(model_uni_og_w_m,s="lambda.min") # Extract coefficients from LASSO regression for lambda min
model_coef_w_m <- as.data.frame(as.matrix(beta_m)) # Convert to dataframe
model_coef_w_m$names <- rownames(model_coef_w_m) # Save row names
model_coef_w_m <- model_coef_w_m[-1,] # Remove intercept value (not important for prediction)

model_coef_w_f$s_s1 <- ((model_coef_w_f$s1)*sx_f)/sy_f # Standardize beta coefficients
model_coef_w_m$s_s1 <- ((model_coef_w_m$s1)*sx_m)/sy_m # Standardize beta coefficients

model_coef_w_f$label <- "Female" # Add data label
model_coef_w_m$label <- "Male" # Add data label

model_coef_w_full <- rbind(model_coef_w_f,model_coef_w_m,model_coef_w) # Combine all data
model_coef_w_plot <- ggplot(data=model_coef_w_full,aes(x=names,y=s_s1,color=label)) + 
                    geom_point(size=4,position=position_jitter(0.1)) +
                    scale_x_discrete(labels=c("Age","Alcohol status (Less than yearly)","Alcohol status (Monthly or less)","Alcohol status (Two to four times a week)","Alcohol status (Twice or more a week)","Birthplace (Other)","Sex at Birth (Female)","Health Insurance (Yes)","Highest grade (9-11th grade)","Highest grade (High school graduate/GED or equivalent)","Highest grade (Some college or AA degree)","Highest grade (College graduate or above)","Marital status (Widowed/Divorced/Separated)","Marital status (Never married)","Overall Health (Fair)","Overall Health (Good)","Overall Health (Very Good)","Overall Health (Excellent)","Race and Ethnicity (Black or African American)","Race and Ethnicity (Hispanic or Latino)","Race and Ethnicity (Asian)","Race and Ethnicity (Other/Multiracial)","Smoking status (Past smoker)","Smoking status (Sometimes smokes)","Smoking status (Daily smoker)")) +
                    coord_flip() +
                    geom_hline(yintercept=0, linetype='dotted', col = 'red') +
                    xlab("") + ylab("Standardized Beta Value (\u03b2)") + 
                    scale_y_continuous(limits=c(-10,10)) +
#                     theme_classic(base_size=24) +
#                     guides(color="none") +
                    #scale_color_discrete(breaks=c("Full","Male","Female")) +
                    scale_color_manual(values=c("darkgreen","steelblue3","mediumpurple2"),breaks=c("Full","Male","Female")) +
                    theme(legend.position = "top") + labs(color="") + theme(axis.text.y=element_blank()) + guides(color="none")
#                     options(repr.plot.width=4)
                    
model_coef_w_plot

# This snippet assumes that you run setup first

# This code saves your dataframe into a csv file in a "data" folder in Google Bucket

# Replace df with THE NAME OF YOUR DATAFRAME
my_dataframe <- aou_weights

# Replace 'test.csv' with THE NAME of the file you're going to store in the bucket (don't delete the quotation marks)
destination_filename <- 'aou_weights_calculated_oh.csv'

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
system(paste0("gsutil cp ./", destination_filename, " ", my_bucket, "/notebooks/data/"), intern=T)

# Check if file is in the bucket
system(paste0("gsutil ls ", my_bucket, "/notebooks/data/*.csv"), intern=T)

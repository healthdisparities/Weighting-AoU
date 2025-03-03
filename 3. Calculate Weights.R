#Prepare Workspace
#Load packages needed for weight development

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
"rcompanion")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, require, character.only = TRUE)

# Import prepared data

# Univariate LASSO model pre-weighting
## Prepare Data
### Merge All of Us and NHANES data
# We will start by merging NHANES data and All of Us data into one dataframe. The NHANES cohort has its own weights that must be applied, and we added those in 2.2 to NHANES data. For All of Us we assign a weight of 1 to all participants to take them as is.

complete_aou_df$weight <- 1 #Assign a weight of 1 to All of Us participants

combined_df <- rbind(complete_aou_df,complete_nhanes_df) # Combine NHANES and All of Us data

### Create dummy variables
lasso_df <- dummy_cols(combined_df,
                        select_columns=c("race","sex_at_birth","nativity","marital_status","education",
                                         "health_insurance","overall_health","drinking_habits","smoking_habits"),
                        ignore_na=TRUE,
                        remove_first_dummy = TRUE,
                        remove_selected_columns = TRUE) # Convert categorical variables to dummy variables

lasso_df <- lasso_df %>%
                relocate(weight, .after=last_col()) %>%
                relocate(data_label, .after=last_col()) %>%
                relocate(participation, .after=last_col()) # Reorder columns
lasso_df <- lasso_df[complete.cases(lasso_df),] # Ensure there are only complete cases

### Run Univariate LASSO regression
# Here we run the univariate LASSO regression. This will help us see how variables predict participation in All of Us

x <- as.matrix(lasso_df[2:(ncol(lasso_df)-3)]) # Matrix of predictors
y <- as.matrix(lasso_df[[ncol(lasso_df)]]) # Matrix of participation variables
w <- as.matrix(lasso_df[[(ncol(lasso_df)-2)]]) # Matrix of weights

univariate_model <- cv.glmnet(x,y,weights=w,family="binomial") # Develop model to predict participation probability

beta <- coef(univariate_model,s="lambda.min") # Extract coefficients from LASSO regression for lambda min
model_coef_uw <- as.data.frame(as.matrix(beta)) # Convert to dataframe
model_coef_uw$names <- rownames(model_coef_uw) # Save row names
model_coef_uw <- model_coef_uw[-1,] # Remove intercept value (not important for prediction)

### Analyze effect sizes
wt <- lasso_df$weight # Extract weights
sx <- sapply(lasso_df[,2:(ncol(lasso_df)-3)],function(x) sqrt(wtd.var(x,wt))) #Get standard deviation for each predictor
sy <- sqrt(wtd.var(lasso_df$participation,wt)) # Save standard deviation for participation

model_coef_uw$s_s1 <- ((model_coef_uw$s1)*sx)/sy # Standardize beta coefficients
model_coef_uw$label <- "Full" # Add data label
             
lasso_df_f <- lasso_df[lasso_df$sex_at_birth_1 == 1,] # Save data for females only
lasso_df_m <- lasso_df[lasso_df$sex_at_birth_1 == 0,] # Save data for males only
lasso_df_f <- lasso_df_f %>%
                    subset(select=-c(sex_at_birth_1)) # Remove gender (not important since we subset ny gender)
             
lasso_df_m <- lasso_df_m %>%
                    subset(select=-c(sex_at_birth_1)) # Remove gender (not important since we subset ny gender)
             
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

model_coef_uw_full <- model_coef_uw_full %>%
                        mutate(names=recode(names,"age"="Age",
                                  "race_2"="Race (Black or African American)",
                                  "race_3"="Race (Hispanic or Latino)",
                                  "race_4"="Race (Asian)",
                                  "race_5"="Race (Other/Multiracial)",
                                  "sex_at_birth_1"="Sex at Birth (Female)",
                                   "education_2"="Education (9-11th grade)",
                                   "education_3"="Education (High school graduate/GED)",
                                   "education_4"="Education (Some college/AA)",
                                   "education_5"="Education (College graduate or above)",
                                   "marital_status_2"="Marital status (Widowed/Divorced/Separated)",
                                   "marital_status_3"="Marital status (Never married)",
                                   "health_insurance_1"="Health insurance (Yes)",
                                   "nativity_2"="Nativity (Other)",
                                   "drinking_habits_2"="Drinking habits (Less than yearly)",
                                   "drinking_habits_3"="Drinking habits (Monthly or less)",
                                   "drinking_habits_4"="Drinking habits (Two to four times a month)",
                                   "drinking_habits_5"="Drinking habits (Twice or more a week)",
                                   "smoking_habits_2"="Smoking habits (Former smoker)",
                                   "smoking_habits_3"="Smoking habits (Sometimes smokes)",
                                   "smoking_habits_4"="Smoking habits (Daily smoker)",
                                   "overall_health_2"="Overall Health (Fair)",
                                   "overall_health_3"="Overall Health (Good)",
                                   "overall_health_4"="Overall Health (Very Good)",
                                   "overall_health_5"="Overall Health (Excellent)",
                             ))

model_coef_plot_uw <- ggplot(data=model_coef_uw_full,aes(x=names,y=s_s1,color=label)) + 
                    geom_point(size=4,position=position_jitter(0.1)) +
#                     scale_x_discrete(labels=c("Age","Alcohol status (Less than yearly)","Alcohol status (Monthly or less)","Alcohol status (Two to four times a week)","Alcohol status (Twice or more a week)","Birthplace (Other)","Sex at Birth (Female)","Health Insurance (Yes)","Highest grade (9-11th grade)","Highest grade (High school graduate/GED or equivalent)","Highest grade (Some college or AA degree)","Highest grade (College graduate or above)","Marital status (Widowed/Divorced/Separated)","Marital status (Never married)","Overall Health (Fair)","Overall Health (Good)","Overall Health (Very Good)","Overall Health (Excellent)","Race and Ethnicity (Black or African American)","Race and Ethnicity (Hispanic or Latino)","Race and Ethnicity (Asian)","Race and Ethnicity (Other/Multiracial)","Smoking status (Past smoker)","Smoking status (Sometimes smokes)","Smoking status (Daily smoker)")) +
                    coord_flip() +
                    geom_hline(yintercept=0, linetype='dotted', col = 'red') +
                    xlab("") + ylab("Standardized Beta Value (\u03b2)") + 
                    scale_y_continuous(limits=c(-8,31)) +
                    theme_classic(base_size=24) +
                    guides(color="none") +
                    #scale_color_discrete(breaks=c("Full","Male","Female")) +
                    scale_color_manual(values=c("darkgreen","steelblue3","mediumpurple2"),breaks=c("Full","Male","Female")) +
                    theme(legend.position = "top") + labs(color="") + theme(legend.position = "none")
#                     options(repr.plot.width=4)

model_coef_plot_uw

# Develop weights
## Prepare Data for LASSO
### Merge All of Us and NHANES data
# We will start by merging NHANES data and All of Us data into one dataframe. The NHANES cohort has its own weights that must be applied, and we added those in 2.2 to NHANES data. For All of Us we assign a weight of 1 to all participants to take them as is.

complete_aou_df$weight <- 1 #Assign a weight of 1 to All of Us participants

combined_df <- rbind(complete_aou_df,complete_nhanes_df) # Combine NHANES and All of Us data

## Create Dummy Variables
# LASSO requires categorical variables be broken up into dummy variables. The fastDummies package allows us to do this.
lasso_df <- dummy_cols(combined_df,
                        select_columns=c("race","sex_at_birth","nativity","marital_status","education",
                                         "health_insurance","overall_health","drinking_habits","smoking_habits"),
                        ignore_na=TRUE,
                        remove_first_dummy = TRUE,
                        remove_selected_columns = TRUE) # Convert categorical variables to dummy variables

lasso_df <- lasso_df %>%
                relocate(weight, .after=last_col()) %>%
                relocate(data_label, .after=last_col()) %>%
                relocate(participation, .after=last_col()) # Reorder columns
               
lasso_df <- lasso_df[complete.cases(lasso_df),] # Ensure there are only complete cases

## Develop LASSO IP weight model
# To develop weights we break up the data into matrices. To capture the full breadth of the correlation structure in the data, we include all possible 2-way interactions

f <- as.formula(~ .*.) # Formula to ensure all 2 way interactions are added
x <- model.matrix(f,lasso_df[2:(ncol(lasso_df)-3)])[,-1] # Matrix of predictors
y <- as.matrix(lasso_df[[ncol(lasso_df)]]) # Matrix of participation status
w <- as.matrix(lasso_df[[(ncol(lasso_df)-2)]]) # Matrix of weights
               
model_int_og <- cv.glmnet(x,y,weights=w,family="binomial") # Build model to calculate weights
               
plot(model_int_og)

plot(model_int_og$glmnet.fit,"lambda",labels=TRUE)

## Predict participation probability
# Using the model above, we predict participation probability for each person.

aou_dummy_df <- dummy_cols(complete_aou_df,
                                select_columns=c("race","sex_at_birth","nativity","marital_status","education",
                                         "health_insurance","overall_health","drinking_habits","smoking_habits"),
                                ignore_na=TRUE,
                                remove_first_dummy = TRUE,
                                remove_selected_columns = TRUE,
                                omit_colname_prefix = TRUE) # For only the All of Us dataset, convert to dummy variables

aou_dummy_df <- aou_dummy_df %>%
                relocate(weight, .after=last_col()) %>%
                relocate(data_label, .after=last_col()) %>%
                relocate(participation, .after=last_col()) # Reorder variables

f <- as.formula(~ .*.) # Formula to create all 2 way interactions
predict_x <- model.matrix(f,aou_dummy_df[2:(ncol(aou_dummy_df)-3)])[,-1] # Matrix of predictors

## Calculate Weights
### Calculate IP
# Finally we calculate IP weights using the formula (1-Pi)/Pi where Pi is participation probability.
# Note: LASSO does not return the exact same coefficients every time (but they are still very close), thus when calculating weights your range may be slightly different than the range listed in the paper. The weights are still effective in reducing participation bias, and their effectiveness is further tested in "5 Validate Weights"

predict_aou <- aou_dummy_df # Temporary dataframe to calculate IP weights
predict_aou$predict_prob <- predict(model_int_og,newx=predict_x,s="lambda.min",type="response") # Get participation probability (Pi)
predict_aou$ip <- 1 - predict_aou$predict_prob # Top of formula (1 - Pi)
predict_aou$w_ip <- predict_aou$ip/predict_aou$predict_prob # Calculate IP weight (1 - Pi)/Pi
predict_aou$w_ip <- as.double(predict_aou$w_ip) # Convert to number

aou_weights <- predict_aou %>%
                subset(select=c(id,w_ip)) # Save only ID and IP weights
colnames(aou_weights)[2] <- "weight"
               
aou_weights <- aou_weights %>%
                subset(select=c(id,weight)) # Save only ID and normalized IP weights

## Post stratify
# We further post stratify erights using ACS data to better match age race and sex of the US population
complete_aou_df <- complete_aou_df %>% subset(select=-weight)
complete_aou_df <- full_join(complete_aou_df,aou_weights,by="id")

# Categorize age
complete_aou_df <- complete_aou_df %>%
  mutate(age_group = case_when(
    age >= 18 & age <= 39 ~ "18-39",
    age >= 40 & age <= 49 ~ "40-49",
    age >= 50 & age <= 59 ~ "50-59",
    age >= 60 & age <= 79 ~ "60-79"
  ))

pop_totals <- read_csv("totals_for_poststratifying.csv")
colnames(pop_totals)[3] <- "sex_at_birth" 

complete_aou_df <- left_join(complete_aou_df,pop_totals,by=c("age_group","race","sex_at_birth"))

design <- svydesign(ids = ~id, weights = ~weight, data = complete_aou_df)
colnames(pop_totals)[4] <- "Freq"
post_design <- postStratify(design, ~age_group + race + sex_at_birth, pop_totals)
final_weights <- weights(post_design)

complete_aou_df$post_weight <- final_weights

aou_weights <- complete_aou_df %>%
                    subset(select=c(id,weight,post_weight))

# Validate Weights
complete_aou_df <- full_join(complete_aou_df,aou_weights,by="id") # Merge dataset with weights
complete_aou_df <- complete_aou_df[complete.cases(complete_aou_df),] # Save only complete cases (should already be complete cases)
complete_aou_df <- complete_aou_df %>% subset(select=-c(weight)) %>% rename(weight = post_weight)
head(complete_aou_df)

complete_nhanes_df <- full_join(complete_nhanes_df,nhanes_weights,by="id") # Merge dataset with weights
complete_nhanes_df <- complete_nhanes_df[complete.cases(complete_nhanes_df),] # Save complete cases
head(complete_nhanes_df)

## Merge All of Us and NHANES data
# We will start by merging NHANES data and All of Us data into one dataframe. The NHANES cohort has its own weights that must be applied, and we added those in 2.2 to NHANES data. For All of Us we assign a weight of 1 to all participants to take them as is.
combined_df <- rbind(complete_aou_df,complete_nhanes_df) # Combine NHANES and All of Us data

## Create Dummy Variables
# LASSO requires categorical variables be broken up into dummy variables. The fastDummies package allows us to do this.
lasso_df <- dummy_cols(combined_df,
                        select_columns=c("race","sex_at_birth","nativity","marital_status","education",
                                         "health_insurance","overall_health","drinking_habits","smoking_habits"),
                        ignore_na=TRUE,
                        remove_first_dummy = TRUE,
                        remove_selected_columns = TRUE) # Convert categorical variables to dummy variables

lasso_df <- lasso_df %>%
                relocate(weight, .after=last_col()) %>%
                relocate(data_label, .after=last_col()) %>%
                relocate(participation, .after=last_col()) # Reorder columns
               
lasso_df <- lasso_df[complete.cases(lasso_df),] # Ensure there are only complete cases

## Run Univariate LASSO regression
# Here we run the univariate LASSO regression. This will help us see how variables predict participation in All of Us

x <- as.matrix(lasso_df[2:(ncol(lasso_df)-3)]) # Matrix of predictors
y <- as.matrix(lasso_df[[ncol(lasso_df)]]) # Matrix of participation variables
w <- as.matrix(lasso_df[[(ncol(lasso_df)-2)]]) # Matrix of weights
               
univariate_model <- cv.glmnet(x,y,weights=w,family="binomial") # Develop model to predict participation probability
               
beta <- coef(univariate_model,s="lambda.min") # Extract coefficients from LASSO regression for lambda min
model_coef_w <- as.data.frame(as.matrix(beta)) # Convert to dataframe
model_coef_w$names <- rownames(model_coef_w) # Save row names
model_coef_w <- model_coef_w[-1,] # Remove intercept value (not important for prediction)

## Analyze effect sizes
# We standardize effect sizes for all variables in order to compare them. Without standradization the effect sizes will be on different scales and can not be easily compared.

wt <- lasso_df$weight # Extract weights
sx <- sapply(lasso_df[,2:(ncol(lasso_df)-3)],function(x) sqrt(wtd.var(x,wt))) #Get standard deviation for each predictor
sy <- sqrt(wtd.var(lasso_df$participation,wt)) # Save standard deviation for participation

model_coef_w$s_s1 <- ((model_coef_w$s1)*sx)/sy # Standardize beta coefficients
model_coef_w$label <- "Full" # Add data label
             
lasso_df_f <- lasso_df[lasso_df$sex_at_birth_1 == 1,] # Save data for females only
lasso_df_m <- lasso_df[lasso_df$sex_at_birth_1 == 0,] # Save data for males only
lasso_df_f <- lasso_df_f %>%
                    subset(select=-c(sex_at_birth_1)) # Remove gender (not important since we subset ny gender)
lasso_df_m <- lasso_df_m %>%
                    subset(select=-c(sex_at_birth_1)) # Remove gender (not important since we subset ny gender)
             
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
               
model_coef_w_full <- model_coef_w_full %>%
                        mutate(names=recode(names,"age"="Age",
                                  "race_2"="Race (Black or African American)",
                                  "race_3"="Race (Hispanic or Latino)",
                                  "race_4"="Race (Asian)",
                                  "race_5"="Race (Other/Multiracial)",
                                  "sex_at_birth_1"="Sex at Birth (Female)",
                                   "education_2"="Education (9-11th grade)",
                                   "education_3"="Education (High school graduate/GED)",
                                   "education_4"="Education (Some college/AA)",
                                   "education_5"="Education (College graduate or above)",
                                   "marital_status_2"="Marital status (Widowed/Divorced/Separated)",
                                   "marital_status_3"="Marital status (Never married)",
                                   "health_insurance_1"="Health insurance (Yes)",
                                   "nativity_2"="Nativity (Other)",
                                   "drinking_habits_2"="Drinking habits (Less than yearly)",
                                   "drinking_habits_3"="Drinking habits (Monthly or less)",
                                   "drinking_habits_4"="Drinking habits (Two to four times a month)",
                                   "drinking_habits_5"="Drinking habits (Twice or more a week)",
                                   "smoking_habits_2"="Smoking habits (Former smoker)",
                                   "smoking_habits_3"="Smoking habits (Sometimes smokes)",
                                   "smoking_habits_4"="Smoking habits (Daily smoker)",
                                   "overall_health_2"="Overall Health (Fair)",
                                   "overall_health_3"="Overall Health (Good)",
                                   "overall_health_4"="Overall Health (Very Good)",
                                   "overall_health_5"="Overall Health (Excellent)",
                             )) 

model_coef_plot_w <- ggplot(data=model_coef_w_full,aes(x=names,y=s_s1,color=label)) + 
                    geom_point(size=4,position=position_jitter(0.1)) +
#                     scale_x_discrete(labels=c("Age","Alcohol status (Less than yearly)","Alcohol status (Monthly or less)","Alcohol status (Two to four times a week)","Alcohol status (Twice or more a week)","Birthplace (Other)","Sex at Birth (Female)","Health Insurance (Yes)","Highest grade (9-11th grade)","Highest grade (High school graduate/GED or equivalent)","Highest grade (Some college or AA degree)","Highest grade (College graduate or above)","Marital status (Widowed/Divorced/Separated)","Marital status (Never married)","Overall Health (Fair)","Overall Health (Good)","Overall Health (Very Good)","Overall Health (Excellent)","Race and Ethnicity (Black or African American)","Race and Ethnicity (Hispanic or Latino)","Race and Ethnicity (Asian)","Race and Ethnicity (Other/Multiracial)","Smoking status (Past smoker)","Smoking status (Sometimes smokes)","Smoking status (Daily smoker)")) +
                    coord_flip() +
                    geom_hline(yintercept=0, linetype='dotted', col = 'red') +
                    xlab("") + ylab("Standardized Beta Value (\u03b2)") + 
                    scale_y_continuous(limits=c(-8,31)) +
                    theme_classic() +
                    guides(color="none") +
                    #scale_color_discrete(breaks=c("Full","Male","Female")) +
                    scale_color_manual(values=c("darkgreen","steelblue3","mediumpurple2"),breaks=c("Full","Male","Female")) +
                    theme(legend.position = "top") + labs(color="") + theme(legend.position = "none")
#                     options(repr.plot.width=4)

model_coef_plot_w

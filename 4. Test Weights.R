# Prep workspace
# Load packages needed for weight testing

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
# Import and merge All of Us data
# This snippet assumes that you run setup first

# This code copies a file from your Google Bucket into a dataframe

# replace 'test.csv' with the name of the file in your google bucket (don't delete the quotation marks)
name_of_file_in_bucket <- 'final_aou_data_complete_oh.csv'

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
complete_aou_df  <- read_csv(name_of_file_in_bucket)

# As mentioned in notebook 1, the weights vary slightly with each iteration due to the nature of LASSO regression. Here we load the weights developed and used in our paper for the sake of replicability.

# If you would like to use weights you developed in Notebook 1, please change to "aou_weights_calculated.csv"

# As you compare the two you will notice despite slight variations, the weights produce approximately the same results.

# This snippet assumes that you run setup first

# This code copies a file from your Google Bucket into a dataframe

# replace 'test.csv' with the name of the file in your google bucket (don't delete the quotation marks)
name_of_file_in_bucket <- 'aou_weights_calculated_2.csv'

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
aou_weights  <- read_csv(name_of_file_in_bucket)
complete_aou_df <- full_join(complete_aou_df,aou_weights,by="id") # Merge dataset with weights
complete_aou_df <- complete_aou_df[complete.cases(complete_aou_df),] # Save only complete cases (should already be complete cases)
head(complete_aou_df)

# Import and merge NHANES data
# This snippet assumes that you run setup first

# This code copies a file from your Google Bucket into a dataframe

# replace 'test.csv' with the name of the file in your google bucket (don't delete the quotation marks)
name_of_file_in_bucket <- 'final_nhanes_data_complete_oh.csv'

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
complete_nhanes_df  <- read_csv(name_of_file_in_bucket)
# This snippet assumes that you run setup first

# This code copies a file from your Google Bucket into a dataframe

# replace 'test.csv' with the name of the file in your google bucket (don't delete the quotation marks)
name_of_file_in_bucket <- 'updated_nhanes_weights.csv'

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
nhanes_weights  <- read_csv(name_of_file_in_bucket)
complete_nhanes_df <- full_join(complete_nhanes_df,nhanes_weights,by="id") # Merge dataset with weights
complete_nhanes_df <- complete_nhanes_df[complete.cases(complete_nhanes_df),] # Save complete cases
head(complete_nhanes_df)

# Recover population means and proportions
# The code here will print the means and prevalences given one of the variables in NHANES and All of Us. These numbers were later used to generate graphs showing mean and proportion recovery.

vector_nhanes_weights <- complete_nhanes_df$weight #Save a vector of NHANES weights we can call
vector_aou_weights <- complete_aou_df$weight #Save a vector of All of Us weights we can call

# Age
nhanes_age <- complete_nhanes_df$age # Save a vector of ages of NHANES we can work with
aou_age <- complete_aou_df$age # Save a vector of ages for All of Us we can work with
# Save dataframe of mean age for each data source
age_df <- data.frame(Source = c("NHANES","All of Us (unweighted)","All of Us (weighted)"),
                     Age = c(wtd.mean(nhanes_age,vector_nhanes_weights),mean(aou_age),wtd.mean(aou_age,vector_aou_weights)))
head(age_df)
age_plot <- ggplot(data=age_df,aes(x=factor(Source, level=c("NHANES","All of Us (unweighted)","All of Us (weighted)")),y=Age,fill=factor(Source, level=c("NHANES","All of Us (unweighted)","All of Us (weighted)")))) + 
            geom_bar(stat="identity",width=0.35) + 
            theme_classic() + 
            theme(axis.text.x=element_text(angle=25,vjust=1,hjust=1)) + 
            scale_x_discrete(name ="") + 
            ylab("Mean Age") + 
            guides(fill="none") + 
            scale_fill_manual(values=c("darkgreen","darkblue","darkorange")) + 
            ggtitle("Age") +
            xlab("")

age_plot

# Race and Ethnicity
The process used in this section is common for the rest of the categorical variables included in the model.

nhanes_race <- complete_nhanes_df$race # Save a vector of race from NHANES
aou_race <- complete_aou_df$race # Save a vector of race from All of Us
race_df <- data.frame(Race = c("White","Black or African American","Hispanic or Latino","Asian","Other/Multiracial"),
                      NHANES = as.vector(wpct(nhanes_race,vector_nhanes_weights)), # Save weighted proportion of each racial and ethnic group from NHANES
                      `All of Us (Unweighted)` = as.vector(prop.table(table(aou_race))), # Save unweighted proprotion of each racial and ethnic group from All of Us
                      `All of Us (Weighted)` = as.vector(wpct(aou_race,vector_aou_weights))) # Save weighted proprotion of each racial and ethnic group from All of Us
colnames(race_df)[c(3,4)] <- c("All of Us (unweighted)", "All of Us (weighted)")
head(race_df)
# This code pivots the data to long format in order to easily plot the data
race_df <- race_df %>%
            pivot_longer(cols=c("NHANES","All of Us (unweighted)","All of Us (weighted)"),
                         names_to = "Source",
                         values_to = "Proportion")
race_plot <- ggplot(data=race_df,aes(x=factor(Race, level=c("White","Black or African American","Hispanic or Latino","Asian","Other/Multiracial")),y=Proportion,fill=factor(Source, level=c("NHANES","All of Us (unweighted)","All of Us (weighted)")))) + 
             geom_bar(stat="identity", position = position_dodge(width = 0.7), width = 0.6) + 
             theme_classic() + 
             theme(axis.text.x=element_text(angle=25,vjust=1,hjust=1)) + 
             guides(fill="none") + 
             xlab("") + 
             ylab("Proportion") + 
             scale_fill_manual(values=c("darkgreen","darkblue","darkorange")) + 
             ggtitle("Race and Ethnicity") +
             scale_x_discrete(limits = levels(race_df$Race))

race_plot

# Education
nhanes_grade <- complete_nhanes_df$highest_grade
aou_grade <- complete_aou_df$highest_grade
grade_df <- data.frame(Grade = c("Less than 9th grade","9-11th grade","High school graduate/GED or equivalent","Some college or AA degree","College graduate or above"),
                      NHANES = as.vector(wpct(nhanes_grade,vector_nhanes_weights)),
                      `All of Us (Unweighted)` = as.vector(prop.table(table(aou_grade))),
                      `All of Us (Weighted)` = as.vector(wpct(aou_grade,vector_aou_weights)))
colnames(grade_df)[c(3,4)] <- c("All of Us (unweighted)", "All of Us (weighted)")
head(grade_df)
grade_df <- grade_df %>%
            pivot_longer(cols=c("NHANES","All of Us (unweighted)","All of Us (weighted)"),
                         names_to = "Source",
                         values_to = "Proportion")
head(grade_df)
grade_plot <- ggplot(data=grade_df,aes(x=factor(Grade, level=c("Less than 9th grade","9-11th grade","High school graduate/GED or equivalent","Some college or AA degree","College graduate or above")),y=Proportion,fill=factor(Source, level=c("NHANES","All of Us (unweighted)","All of Us (weighted)")))) + 
              geom_bar(stat="identity", position = position_dodge(width = 0.7), width = 0.6) + 
              theme_classic() + 
              theme(axis.text.x=element_text(angle=25,vjust=1,hjust=1)) + 
              guides(fill="none") + 
              xlab("") +
              scale_fill_manual(values=c("darkgreen","darkblue","darkorange")) + 
              ggtitle("Education") +
              scale_x_discrete(limits = levels(grade_df$Grade))

grade_plot

# Sex at Birth
nhanes_sex <- complete_nhanes_df$gender
aou_sex <- complete_aou_df$gender
sex_df <- data.frame(Sex = c("Male","Female"),
                      NHANES = as.vector(wpct(nhanes_sex,vector_nhanes_weights)),
                      `All of Us (Unweighted)` = as.vector(prop.table(table(aou_sex))),
                      `All of Us (Weighted)` = as.vector(wpct(aou_sex,vector_aou_weights)))
colnames(sex_df)[c(3,4)] <- c("All of Us (unweighted)", "All of Us (weighted)")
head(sex_df)
sex_df <- sex_df %>%
            pivot_longer(cols=c("NHANES","All of Us (unweighted)","All of Us (weighted)"),
                         names_to = "Source",
                         values_to = "Proportion")
head(sex_df)
sex_plot <- ggplot(data=sex_df,aes(x=factor(Sex, level=c("Male","Female")),y=Proportion,fill=factor(Source, level=c("NHANES","All of Us (unweighted)","All of Us (weighted)")))) + 
              geom_bar(stat="identity", position = position_dodge(width = 0.7), width = 0.6) + 
              theme_classic() + 
              theme(axis.text.x=element_text(angle=25,vjust=1,hjust=1)) + 
              guides(fill="none") + 
              xlab("") +
              scale_fill_manual(values=c("darkgreen","darkblue","darkorange")) + 
              ggtitle("Sex at Birth") +
              scale_x_discrete(limits = levels(sex_df$Sex))

sex_plot

# Birthplace
nhanes_birthplace <- complete_nhanes_df$birthplace
aou_birthplace <- complete_aou_df$birthplace
birthplace_df <- data.frame(Birthplace = c("Born in US", "Born Outside US"),
                            NHANES = as.vector(wpct(nhanes_birthplace,vector_nhanes_weights)),
                            `All of Us (Unweighted)` = as.vector(prop.table(table(aou_birthplace))),
                            `All of Us (Weighted)` = as.vector(wpct(aou_birthplace,vector_aou_weights)))
colnames(birthplace_df)[c(3,4)] <- c("All of Us (unweighted)", "All of Us (weighted)")
head(birthplace_df)
birthplace_df <- birthplace_df %>%
            pivot_longer(cols=c("NHANES","All of Us (unweighted)","All of Us (weighted)"),
                         names_to = "Source",
                         values_to = "Proportion")
head(birthplace_df)
birthplace_plot <- ggplot(data=birthplace_df,aes(x=factor(Birthplace, level=c("Born in US", "Born Outside US")),y=Proportion,fill=factor(Source, level=c("NHANES","All of Us (unweighted)","All of Us (weighted)")))) + 
              geom_bar(stat="identity", position = position_dodge(width = 0.7), width = 0.6) + 
              theme_classic() + 
              theme(axis.text.x=element_text(angle=25,vjust=1,hjust=1)) + 
              guides(fill="none") + 
              xlab("") +
              scale_fill_manual(values=c("darkgreen","darkblue","darkorange")) + 
              ggtitle("Birthplace") +
              scale_x_discrete(limits = levels(birthplace_df$Birthplace))

birthplace_plot

# Health Insurance
nhanes_health_insurance <- complete_nhanes_df$health_insurance
aou_health_insurance <- complete_aou_df$health_insurance
health_insurance_df <- data.frame(health_insurance = c("Not Covered by Health Insurance", "Covered by Health Insurance"),
                            NHANES = as.vector(wpct(nhanes_health_insurance,vector_nhanes_weights)),
                            `All of Us (Unweighted)` = as.vector(prop.table(table(aou_health_insurance))),
                            `All of Us (Weighted)` = as.vector(wpct(aou_health_insurance,vector_aou_weights)))
colnames(health_insurance_df)[c(1,3,4)] <- c("Health Insurance","All of Us (unweighted)", "All of Us (weighted)")
head(health_insurance_df)
health_insurance_df <- health_insurance_df %>%
            pivot_longer(cols=c("NHANES","All of Us (unweighted)","All of Us (weighted)"),
                         names_to = "Source",
                         values_to = "Proportion")
head(health_insurance_df)
health_insurance_plot <- ggplot(data=health_insurance_df,aes(x=factor(`Health Insurance`, level=c("Not Covered by Health Insurance", "Covered by Health Insurance")),y=Proportion,fill=factor(Source, level=c("NHANES","All of Us (unweighted)","All of Us (weighted)")))) + 
              geom_bar(stat="identity", position = position_dodge(width = 0.7), width = 0.6) + 
              theme_classic() + 
              theme(axis.text.x=element_text(angle=25,vjust=1,hjust=1)) + 
              guides(fill="none") + 
              xlab("") +
              scale_fill_manual(values=c("darkgreen","darkblue","darkorange")) + 
              ggtitle("Health Insurance") +
              scale_x_discrete(limits = levels(health_insurance_df$`Health Insurance`))

health_insurance_plot

# Marital Status
nhanes_marital_status <- complete_nhanes_df$marital_status
aou_marital_status <- complete_aou_df$marital_status
marital_status_df <- data.frame(marital_status = c("Married/Living with Partner","Widowed/Divorced/Separated","Never Married"),
                            NHANES = as.vector(wpct(nhanes_marital_status,vector_nhanes_weights)),
                            `All of Us (Unweighted)` = as.vector(prop.table(table(aou_marital_status))),
                            `All of Us (Weighted)` = as.vector(wpct(aou_marital_status,vector_aou_weights)))
colnames(marital_status_df)[c(1,3,4)] <- c("Marital Status","All of Us (unweighted)", "All of Us (weighted)")
head(marital_status_df)
marital_status_df <- marital_status_df %>%
            pivot_longer(cols=c("NHANES","All of Us (unweighted)","All of Us (weighted)"),
                         names_to = "Source",
                         values_to = "Proportion")
head(marital_status_df)
marital_status_plot <- ggplot(data=marital_status_df,aes(x=factor(`Marital Status`, level=c("Married/Living with Partner","Widowed/Divorced/Separated","Never Married")),y=Proportion,fill=factor(Source, level=c("NHANES","All of Us (unweighted)","All of Us (weighted)")))) + 
              geom_bar(stat="identity", position = position_dodge(width = 0.7), width = 0.6) + 
              theme_classic() + 
              theme(axis.text.x=element_text(angle=25,vjust=1,hjust=1)) + 
              guides(fill="none") + 
              xlab("") +
              scale_fill_manual(values=c("darkgreen","darkblue","darkorange")) + 
              ggtitle("Marital Status") +
              scale_x_discrete(limits = levels(marital_status_df$`Marital Status`))

marital_status_plot

# Drinking Habits
nhanes_alcohol <- complete_nhanes_df$alcohol_final
aou_alcohol <- complete_aou_df$alcohol_final
alcohol_df <- data.frame(alcohol = c("Never drank","Less than yearly","Monthly or less","Two to four times a month","Twice or more a week"),
                      NHANES = as.vector(wpct(nhanes_alcohol,vector_nhanes_weights)),
                      `All of Us (Unweighted)` = as.vector(prop.table(table(aou_alcohol))),
                      `All of Us (Weighted)` = as.vector(wpct(aou_alcohol,vector_aou_weights)))
colnames(alcohol_df)[c(1,3,4)] <- c("Drinking Habits","All of Us (unweighted)", "All of Us (weighted)")
head(alcohol_df)
alcohol_df <- alcohol_df %>%
            pivot_longer(cols=c("NHANES","All of Us (unweighted)","All of Us (weighted)"),
                         names_to = "Source",
                         values_to = "Proportion")
head(alcohol_df)
alcohol_plot <- ggplot(data=alcohol_df,aes(x=factor(`Drinking Habits`, level=c("Never drank","Less than yearly","Monthly or less","Two to four times a month","Twice or more a week")),y=Proportion,fill=factor(Source, level=c("NHANES","All of Us (unweighted)","All of Us (weighted)")))) + 
              geom_bar(stat="identity", position = position_dodge(width = 0.7), width = 0.6) + 
              theme_classic() + 
              theme(axis.text.x=element_text(angle=25,vjust=1,hjust=1)) + 
              guides(fill="none") + 
              xlab("") +
              scale_fill_manual(values=c("darkgreen","darkblue","darkorange")) + 
              ggtitle("Drinking Habits") +
              scale_x_discrete(limits = levels(alcohol_df$`Drinking Habits`))

alcohol_plot

# Smoking Habits
nhanes_smoking <- complete_nhanes_df$smoking_final
aou_smoking <- complete_aou_df$smoking_final
smoking_df <- data.frame(smoking = c("Never smoked","Past smoker","Sometimes smokes","Daily smoker"),
                      NHANES = as.vector(wpct(nhanes_smoking,vector_nhanes_weights)),
                      `All of Us (Unweighted)` = as.vector(prop.table(table(aou_smoking))),
                      `All of Us (Weighted)` = as.vector(wpct(aou_smoking,vector_aou_weights)))
colnames(smoking_df)[c(1,3,4)] <- c("Smoking Habits","All of Us (unweighted)", "All of Us (weighted)")
head(smoking_df)
smoking_df <- smoking_df %>%
            pivot_longer(cols=c("NHANES","All of Us (unweighted)","All of Us (weighted)"),
                         names_to = "Source",
                         values_to = "Proportion")
head(smoking_df)
smoking_plot <- ggplot(data=smoking_df,aes(x=factor(`Smoking Habits`, level=c("Never smoked","Past smoker","Sometimes smokes","Daily smoker")),y=Proportion,fill=factor(Source, level=c("NHANES","All of Us (unweighted)","All of Us (weighted)")))) + 
              geom_bar(stat="identity", position = position_dodge(width = 0.7), width = 0.6) + 
              theme_classic() + 
              theme(axis.text.x=element_text(angle=25,vjust=1,hjust=1)) + 
              guides(fill="none") + 
              xlab("") +
              scale_fill_manual(values=c("darkgreen","darkblue","darkorange")) + 
              ggtitle("Smoking Habits") +
              scale_x_discrete(limits = levels(smoking_df$`Smoking Habits`))

smoking_plot

# Overall Health
nhanes_oh <- complete_nhanes_df$overall_health
aou_oh <- complete_aou_df$overall_health
oh_df <- data.frame(oh = c("Poor","Fair","Good","Very Good","Excellent"),
                      NHANES = as.vector(wpct(nhanes_oh,vector_nhanes_weights)),
                      `All of Us (Unweighted)` = as.vector(prop.table(table(aou_oh))),
                      `All of Us (Weighted)` = as.vector(wpct(aou_oh,vector_aou_weights)))
colnames(oh_df)[c(1,3,4)] <- c("Overall Health","All of Us (unweighted)", "All of Us (weighted)")
head(oh_df)
oh_df <- oh_df %>%
            pivot_longer(cols=c("NHANES","All of Us (unweighted)","All of Us (weighted)"),
                         names_to = "Source",
                         values_to = "Proportion")
head(oh_df)
oh_plot <- ggplot(data=oh_df,aes(x=factor(`Overall Health`, level=c("Poor","Fair","Good","Very Good","Excellent")),y=Proportion,fill=factor(Source, level=c("NHANES","All of Us (unweighted)","All of Us (weighted)")))) + 
              geom_bar(stat="identity", position = position_dodge(width = 0.7), width = 0.6) + 
              theme_classic() + 
              theme(axis.text.x=element_text(angle=25,vjust=1,hjust=1)) + 
              guides(fill="none") + 
              xlab("") +
              scale_fill_manual(values=c("darkgreen","darkblue","darkorange")) + 
              ggtitle("Overall Health") +
              scale_x_discrete(limits = levels(oh_df$`Overall Health`))

oh_plot

# Compare correlation coefficients
# Here we focus on comparing the correlation coefficients in each dataset. This allows us to see if the IP weights accomodated the correlation structure of the data.

# We calculate correlation coefficients using point-biserial correlation and Cramer's V. More details on this can be seen in our paper.

# This function prepares the dataframe for caclulation of correlation coefficients
multinomialPrep <- function(df){
    
    df <- df[complete.cases(df),]
    
    df$race <- as.factor(df$race)
#     df$race <- relevel(df$race, ref="1")
    
    df$highest_grade <- as.factor(df$highest_grade)
#     df$highest_grade <- relevel(df$highest_grade, ref="1")
    
    df$marital_status <- as.factor(df$marital_status)
#     df$marital_status <- relevel(df$marital_status, ref="1")
    
    df$alcohol_final <- as.factor(df$alcohol_final)
#     df$alcohol_final <- relevel(df$alcohol_final, ref="1")
    
    df$smoking_final <- as.factor(df$smoking_final)
#     df$smoking_final <- relevel(df$smoking_final, ref="1")
    
    df$overall_health <- as.factor(df$overall_health)
    
    df$gender[df$gender == 1] <- 0
    df$gender[df$gender == 2] <- 1
    df$gender <- as.factor(df$gender)
    
    df$health_insurance[df$health_insurance == 1] <- 0
    df$health_insurance[df$health_insurance == 2] <- 1
    df$health_insurance <- as.factor(df$health_insurance)
    
    df$birthplace[df$birthplace == 1] <- 0
    df$birthplace[df$birthplace == 2] <- 1
    df$birthplace <- as.factor(df$birthplace)
    
    
#     df <- df %>% 
#             relocate(race, highest_grade, marital_status, alcohol_final, smoking_final) %>%
#             relocate(gender, health_insurance, birthplace) %>%
#             relocate(age,weight_kg,height_cm) %>%
#             relocate(id)
    
    return(df)
    
}
combined_df <- rbind(complete_nhanes_df,complete_aou_df) # Combine NHANES and All of Us datasets
aou_w <- combined_df[combined_df$data_label == "All of Us",] # Save dataframe with All of Us participants
nhanes_w <- combined_df[combined_df$data_label == "NHANES",] # Save dataframe with NHANES participants
aou_w <- multinomialPrep(aou_w) # Prep data for correlation coefficeint calculations
nhanes_w <- multinomialPrep(nhanes_w) # Prep data for correlation coefficeint calculations
compare_vars <- colnames(aou_w)[2:(ncol(aou_w)-3)] # List of variables to compare
corr_df <- data.frame() # Data frame for correlation coefficients
for(i in 1:(length(compare_vars)-1)){ # For all variables except the last one
    for(j in (i+1):length(compare_vars)){ # For all variables not including the current iteration
        
        y_char <- compare_vars[i] #Get y
        x_char <- compare_vars[j] #Get x
        formula_paste <- paste(y_char," ~ ",x_char) #Formula to paste

        
        if(y_char == "age"){ # If age
            y_type <- "number" # Variable type is numerical
        }else if(y_char == "gender" | y_char == "health_insurance" | y_char == "birthplace"){ # If any of these
            y_type <- "binary" # Variable type is binary
        }else if(y_char == "race" | y_char == "highest_grade" | y_char == "marital_status" | y_char == "smoking_final"| y_char == "alcohol_final" | y_char == "overall_health"){ # If any of these
            y_type <- "multinomial" # Variable type is multinomial (more than 2 categories)
        }
            
        if(x_char == "age"){ # If age
            x_type <- "number" # Variable type is numerical
        }else if(x_char == "gender" | x_char == "health_insurance" | x_char == "birthplace"){ # If any of these
            x_type <- "binary" # Variable type is binary
        }else if(x_char == "race" | x_char == "highest_grade" | x_char == "marital_status" | x_char == "smoking_final"| x_char == "alcohol_final" | x_char == "overall_health"){ # If any of these
            x_type <- "multinomial" # Variable type is multinomial (more than 2 categories)
        }
            
        
            
        if(x_type == "number" & y_type == "multinomial"){ # If you are comparing a numerical and multinomial variable, use point-biserial correlation
            temp_nhanes_w <- cbind(nhanes_w[x_char],nhanes_w[y_char],nhanes_w["weight"]) # Temp NHANES df of x var and y var
            temp_aou_w <- cbind(aou_w[x_char],aou_w[y_char],aou_w["weight"]) # Temp All of Us df of x var and y var
            temp_nhanes_w_dummy <- dummy_cols(temp_nhanes_w,
                                              select_columns=y_char,
                                              ignore_na=TRUE,
                                              remove_first_dummy = FALSE,
                                              remove_selected_columns = TRUE) # Convert categorical variable to dummy variable
            temp_nhanes_w_dummy <- relocate(temp_nhanes_w_dummy, weight, .after=last_col()) # Move weights to last column
            temp_aou_w_dummy <- dummy_cols(temp_aou_w,
                                              select_columns=y_char,
                                              ignore_na=TRUE,
                                              remove_first_dummy = FALSE,
                                              remove_selected_columns = TRUE) # Convert categorical variable to dummy variable
            temp_aou_w_dummy <- relocate(temp_aou_w_dummy, weight, .after=last_col()) # Move weights to last column
            ######Data prep done
            
            for(k in 2:(ncol(temp_nhanes_w_dummy)-1)){ # For each dummy variable
                corr_df[nrow(corr_df)+1,1] <- paste(colnames(temp_nhanes_w_dummy)[1]," ~ ",colnames(temp_nhanes_w_dummy)[k]) # Save the formula
                corr_df[nrow(corr_df),2] <- wtd.cor(temp_nhanes_w_dummy[[1]],temp_nhanes_w_dummy[[k]],weight=temp_nhanes_w_dummy$weight)[[1]] # Calculate weighted correlation in NHANES
                corr_df[nrow(corr_df),3] <- wtd.cor(temp_aou_w_dummy[[1]],temp_aou_w_dummy[[k]])[[1]] # Calculate unweighted correlation in All of Us
                corr_df[nrow(corr_df),4] <- wtd.cor(temp_aou_w_dummy[[1]],temp_aou_w_dummy[[k]],weight=temp_aou_w_dummy$weight)[[1]] # Calculate weighted correlation in All of Us
            }
            
        }else if(y_type == "number" & x_type == "multinomial"){ # If you are comparing a numerical and multinomial variable, use point-biserial correlation
            temp_nhanes_w <- cbind(nhanes_w[y_char],nhanes_w[x_char],nhanes_w["weight"]) # Temp NHANES df of x var and y var
            temp_aou_w <- cbind(aou_w[y_char],aou_w[x_char],aou_w["weight"]) # Temp All of Us df of x var and y var
            temp_nhanes_w_dummy <- dummy_cols(temp_nhanes_w,
                                              select_columns=x_char,
                                              ignore_na=TRUE,
                                              remove_first_dummy = FALSE,
                                              remove_selected_columns = TRUE) # Convert categorical variable to dummy variable
            temp_nhanes_w_dummy <- relocate(temp_nhanes_w_dummy, weight, .after=last_col()) # Move weights to last column
            temp_aou_w_dummy <- dummy_cols(temp_aou_w,
                                              select_columns=x_char,
                                              ignore_na=TRUE,
                                              remove_first_dummy = FALSE,
                                              remove_selected_columns = TRUE) # Convert categorical variable to dummy variable
            temp_aou_w_dummy <- relocate(temp_aou_w_dummy, weight, .after=last_col()) # Move weights to last column
            
            for(k in 2:(ncol(temp_nhanes_w_dummy)-1)){ # For each dummy variable
                corr_df[nrow(corr_df)+1,1] <- paste(colnames(temp_nhanes_w_dummy)[1]," ~ ",colnames(temp_nhanes_w_dummy)[k]) # Save the formula
                corr_df[nrow(corr_df),2] <- wtd.cor(temp_nhanes_w_dummy[[1]],temp_nhanes_w_dummy[[k]],weight=temp_nhanes_w_dummy$weight)[[1]] # Calculate weighted correlation in NHANES
                corr_df[nrow(corr_df),3] <- wtd.cor(temp_aou_w_dummy[[1]],temp_aou_w_dummy[[k]])[[1]] # Calculate unweighted correlation in All of Us
                corr_df[nrow(corr_df),4] <- wtd.cor(temp_aou_w_dummy[[1]],temp_aou_w_dummy[[k]],weight=temp_aou_w_dummy$weight)[[1]] # Calculate weighted correlation in All of Us
            }
            
        }else if(x_type == "number" & y_type == "binary"){ # If x is numerical and y is binary
            temp_nhanes_w <- cbind(nhanes_w[x_char],nhanes_w[y_char],nhanes_w["weight"]) # Temp NHANES df of x var and y var
            temp_aou_w <- cbind(aou_w[x_char],aou_w[y_char],aou_w["weight"]) # Temp All of Us df of x var and y var
            corr_df[nrow(corr_df)+1,1] <- paste(colnames(temp_nhanes_w)[1]," ~ ",colnames(temp_nhanes_w)[2]) # Save formula
            corr_df[nrow(corr_df),2] <- wtd.cor(temp_nhanes_w[[1]],as.numeric(temp_nhanes_w[[2]]),weight=temp_nhanes_w$weight)[[1]] # Calculate weighted correlation in NHANES
            corr_df[nrow(corr_df),3] <- wtd.cor(temp_aou_w[[1]],as.numeric(temp_aou_w[[2]]))[[1]] # Calculate unweighted correlation in All of Us
            corr_df[nrow(corr_df),4] <- wtd.cor(temp_aou_w[[1]],as.numeric(temp_aou_w[[2]]),weight=temp_aou_w$weight)[[1]] # Calculate weighted correlation in All of uS
        }else if(y_type == "number" & x_type == "binary"){ # If x is binary and y is numerical
            temp_nhanes_w <- cbind(nhanes_w[y_char],nhanes_w[x_char],nhanes_w["weight"]) # Temp NHANES df of x var and y var
            temp_aou_w <- cbind(aou_w[y_char],aou_w[x_char],aou_w["weight"]) # Temp All of Us df of x var and y var
            corr_df[nrow(corr_df)+1,1] <- paste(colnames(temp_nhanes_w)[1]," ~ ",colnames(temp_nhanes_w)[2]) # Save formula
            corr_df[nrow(corr_df),2] <- wtd.cor(temp_nhanes_w[[1]],as.numeric(temp_nhanes_w[[2]]),weight=temp_nhanes_w$weight)[[1]] # Calculate weighted correlation in NHANES
            corr_df[nrow(corr_df),3] <- wtd.cor(temp_aou_w[[1]],as.numeric(temp_aou_w[[2]]))[[1]] # Calculate unweighted correlation in All of Us
            corr_df[nrow(corr_df),4] <- wtd.cor(temp_aou_w[[1]],as.numeric(temp_aou_w[[2]]),weight=temp_aou_w$weight)[[1]] # Calculate weighted correlation in All of uS
        }else if( (y_type == "binary" | y_type == "multinomial") & (x_type == "binary" | x_type =="multinomial") ){ # If both variables are categorical
            corr_df[nrow(corr_df)+1,1] <- formula_paste #Save formula
            chi_nhanes_w <- wtd.chi.sq(nhanes_w[[y_char]],nhanes_w[[x_char]],weight=nhanes_w$weight)[[1]] # Weighted chi square test for NHANES
            chi_aou_uw <- wtd.chi.sq(aou_w[[y_char]],aou_w[[x_char]])[[1]] # Unweighted chi square test for All of Us
            chi_aou_w <- wtd.chi.sq(aou_w[[y_char]],aou_w[[x_char]],weight=aou_w$weight)[[1]] # Weighted chi square test for All of Us
            corr_df[nrow(corr_df),2] <- sqrt( (chi_nhanes_w/nrow(nhanes_w)) / (min((length(levels(nhanes_w[[x_char]]))-1),(length(levels(nhanes_w[[y_char]]))-1))) ) # Calculate Cramer's V for NHANES
            corr_df[nrow(corr_df),3] <- sqrt( (chi_aou_uw/nrow(aou_w)) / (min((length(levels(aou_w[[x_char]]))-1),(length(levels(aou_w[[y_char]]))-1))) ) # Calculate Cramer's V for unweighted All of Us
            corr_df[nrow(corr_df),4] <- sqrt( (chi_aou_w/nrow(aou_w)) / (min((length(levels(aou_w[[x_char]]))-1),(length(levels(aou_w[[y_char]]))-1))) ) # Calculate Cramer's V for weighted All of Us
        } 
            
    }
}
        
colnames(corr_df)[1:4] <- c("formula","nhanes_r","aou_uw_r","aou_w_r")
        
for(i in 1:nrow(corr_df)){
    corr_df[i,5] <- corr_df$nhanes_r[i] - corr_df$aou_uw_r[i] # Measure difference in NHANES and unweighted ALl of Us
    corr_df[i,6] <- corr_df$nhanes_r[i] - corr_df$aou_w_r[i] # Measure difference in NHANES and weighted All of Us
    if(abs(corr_df[i,5]) > 0.05){ # If the difference between NHANES and unweighted All of Us is greater than 0.05
        corr_df[i,7] <- "rdiff > 0.05" # Save label
    }else{ # Else
        corr_df[i,7] <- "rdiff < 0.05" # Save label
    }
}

colnames(corr_df)[5:7] <- c("nhanes_aou_uw","nhanes_aou_w","diff_lab")
corr_df_2 <- melt(subset(corr_df,select=-c(nhanes_aou_uw,nhanes_aou_w)), id=c("formula","diff_lab")) # Convert to long form for plotting

corr_df_2$variable <- as.character(corr_df_2$variable) # Save variable as a character

corr_df_2$variable[corr_df_2$variable == "nhanes_r"] <- "NHANES" # Change label
corr_df_2$variable[corr_df_2$variable == "aou_uw_r"] <- "All of Us (unweighted)" # Change label
corr_df_2$variable[corr_df_2$variable == "aou_w_r"] <- "All of Us (weighted)" # Change label

corr_df_2$variable <- factor(corr_df_2$variable,levels=c("NHANES","All of Us (unweighted)","All of Us (weighted)")) # Make it a factor
corr_plot <- ggplot(corr_df_2, aes(x=formula,y=value,shape=variable)) +
                geom_point(aes(color=diff_lab,shape=variable),size=2) + 
                theme_classic(base_size=6) +
                theme(legend.position = "top",axis.text.x = element_text(angle=55,hjust=1),legend.title=element_blank()) +
                scale_color_manual(values = c("black","purple"),labels=c(bquote(r[diff] < 0.05),bquote(r[diff] > 0.05))) +
                scale_shape_manual(values= c(0,1,2)) +
                guides(color="none",shape="none") +
                xlab("") + ylab("Correlation coefficient (r)") +
                scale_x_discrete(labels=c("Age ~ Alcohol Status (Never drank)",
                                          "Age ~ Alcohol Status (Less than yearly)",
                                          "Age ~ Alcohol Status (Monthly or less)",
                                          "Age ~ Alcohol Status (Two to four times a week)",
                                          "Age ~ Alcohol Status (Twice or more a week)",
                                          "Age ~ Birthplace",
                                          "Age ~ Sex at Birth",
                                          "Age ~ Health Insurance",
                                          "Age ~ Highest Grade (Less than 9th grade)",
                                          "Age ~ Highest Grade (9-11th grade)",
                                          "Age ~ Highest Grade (High school graduate/GED or equivalent)",
                                          "Age ~ Highest Grade (Some college or AA degree)",
                                          "Age ~ Highest Grade (College graduate or above)",
                                          "Age ~ Marital Status (Married/Living with partner)",
                                          "Age ~ Marital Status (Widowed/Divorced/Separated)",
                                          "Age ~ Marital Status (Never married)",
                                          "Age ~ Overall Health (Poor)",
                                          "Age ~ Overall Health (Fair)",
                                          "Age ~ Overall Health (Good)",
                                          "Age ~ Overall Health (Very Good)",
                                          "Age ~ Overall Health (Excellent)",
                                          "Age ~ Race/Ethnicity (White)",
                                          "Age ~ Race/Ethnicity (Black or African American)",
                                          "Age ~ Race/Ethnicity (Hispanic or Latino)",
                                          "Age ~ Race/Ethnicity (Asian)",
                                          "Age ~ Race/Ethnicity (Other/Multiracial)",
                                          "Age ~ Smoking Status (Never smoked)",
                                          "Age ~ Smoking Status (Former smoker)",
                                          "Age ~ Smoking Status (Sometimes smokes)",
                                          "Age ~ Smoking Status (Daily smoker)",
                                          "Alcohol status ~ Overall Health",
                                          "Alcohol status ~ Smoking Status",
                                          "Birthplace ~ Alcohol Status",
                                          "Birthplace ~ Overall Health",
                                          "Birthplace ~ Smoking Status",
                                          "Sex at Birth ~ Alcohol Status",
                                          "Sex at Birth ~ Birthplace",
                                          "Sex at Birth ~ Health Insurance",
                                          "Sex at Birth ~ Highest Grade",
                                          "Sex at Birth ~ Marital Status",
                                          "Sex at Birth ~ Overall Health",
                                          "Sex at Birth ~ Smoking Status",
                                          "Health Insurance ~ Alcohol Status",
                                          "Health Insurance ~ Birthplace",
                                          "Health Insurance ~ Overall Health",
                                          "Health Insurance ~ Smoking Status",
                                          "Highest Grade ~ Alcohol Status",
                                          "Highest Grade ~ Birthplace",
                                          "Highest Grade ~ Health Insurance",
                                          "Highest Grade ~ Marital Status",
                                          "Highest Grade ~ Overall Health",
                                          "Highest Grade ~ Smoking Status",
                                          "Marital Status ~ Alcohol Status",
                                          "Marital Status ~ Birthplace",
                                          "Marital Status ~ Health Insurance",
                                          "Marital Status ~ Overall Health",
                                          "Marital Status ~ Smoking Status",
                                          "Race/Ethnicity ~ Alcohol Status",
                                          "Race/Ethnicity ~ Birthplace",
                                          "Race/Ethnicity ~ Sex at Birth",
                                          "Race/Ethnicity ~ Health Insurance",
                                          "Race/Ethnicity ~ Highest Grade",
                                          "Race/Ethnicity ~ Marital Status",
                                          "Race/Ethnicity ~ Overall Health",
                                          "Race/Ethnicity ~ Smoking Status",
                                          "Smoking Status ~ Overall Health")) +
                theme(legend.position.inside = c(0.5,0.5),plot.margin = margin(10, 10, 10, 100))
                

corr_plot

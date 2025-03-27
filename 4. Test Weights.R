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
"rcompanion",
"sjPlot")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, require, character.only = TRUE)

# Import prepared data
complete_aou_df <- full_join(complete_aou_df,aou_weights,by="id") # Merge dataset with weights
complete_aou_df <- complete_aou_df[complete.cases(complete_aou_df),] # Save only complete cases (should already be complete cases)
complete_aou_df <- complete_aou_df %>% subset(select=-c(weight)) %>% rename(weight = post_weight)
head(complete_aou_df)

complete_nhanes_df <- full_join(complete_nhanes_df,nhanes_weights,by="id") # Merge dataset with weights
complete_nhanes_df <- complete_nhanes_df[complete.cases(complete_nhanes_df),] # Save complete cases

pop_totals <- read_csv("totals_for_poststratifying.csv")

#Recover population means and proportions
#The code here will print the means and prevalences given one of the variables in NHANES and All of Us. These numbers were later used to generate graphs showing mean and proportion recovery.

vector_nhanes_weights <- complete_nhanes_df$weight #Save a vector of NHANES weights we can call
vector_aou_weights <- complete_aou_df$weight #Save a vector of All of Us weights we can call

#Age
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

#Age Groups
#The process used in this section is common for the rest of the categorical variables included in the model.

complete_aou_df <- complete_aou_df %>%
  mutate(age_group = case_when(
    age >= 18 & age <= 39 ~ "18-39",
    age >= 40 & age <= 49 ~ "40-49",
    age >= 50 & age <= 59 ~ "50-59",
    age >= 60 & age <= 79 ~ "60-79"
  ))

complete_nhanes_df <- complete_nhanes_df %>%
  mutate(age_group = case_when(
    age >= 18 & age <= 39 ~ "18-39",
    age >= 40 & age <= 49 ~ "40-49",
    age >= 50 & age <= 59 ~ "50-59",
    age >= 60 & age <= 79 ~ "60-79"
  ))

nhanes_race <- complete_nhanes_df$age_group # Save a vector of race from NHANES
aou_race <- complete_aou_df$age_group # Save a vector of race from All of Us

age_group_df <- data.frame(`Age Group` = c("18-39","40-49","50-59","60-79"),
                      NHANES = as.vector(wpct(nhanes_race,vector_nhanes_weights)), # Save weighted proportion of each racial and ethnic group from NHANES
                      `All of Us (Unweighted)` = as.vector(prop.table(table(aou_race))), # Save unweighted proprotion of each racial and ethnic group from All of Us
                      `All of Us (Weighted)` = as.vector(wpct(aou_race,vector_aou_weights))) # Save weighted proprotion of each racial and ethnic group from All of Us
colnames(age_group_df)[c(3,4)] <- c("All of Us (unweighted)", "All of Us (weighted)")

age_group_df$`ACS` <- c(sum(pop_totals[pop_totals$age_group == "18-39",]$pop_total)/sum(pop_totals$pop_total),
                         sum(pop_totals[pop_totals$age_group == "40-49",]$pop_total)/sum(pop_totals$pop_total),
                         sum(pop_totals[pop_totals$age_group == "50-59",]$pop_total)/sum(pop_totals$pop_total),
                         sum(pop_totals[pop_totals$age_group == "60-79",]$pop_total)/sum(pop_totals$pop_total))

colnames(age_group_df)[1] <- "Age Group"
head(age_group_df)
# This code pivots the data to long format in order to easily plot the data
age_group_df <- age_group_df %>%
            pivot_longer(cols=c("NHANES","All of Us (unweighted)","All of Us (weighted)","ACS"),
                         names_to = "Source",
                         values_to = "Proportion")

head(age_group_df)
age_group_plot <- ggplot(data=age_group_df,aes(x=factor(`Age Group`, level=c("18-39","40-49","50-59","60-79")),y=Proportion,fill=factor(Source, level=c("ACS","NHANES","All of Us (unweighted)","All of Us (weighted)")))) + 
             geom_bar(stat="identity", position = position_dodge(width = 0.7), width = 0.6) + 
             theme_classic() + 
             theme(axis.text.x=element_text(angle=25,vjust=1,hjust=1)) + 
             guides(fill="none") + 
             xlab("") + 
             ylab("Proportion") + 
             scale_fill_manual(values=c("#696969","darkgreen","darkblue","darkorange")) + 
             ggtitle("Age") +
             scale_x_discrete(limits = levels(age_group_df$`Age Group`))

age_group_plot

# Race and Ethnicity
# The process used in this section is common for the rest of the categorical variables included in the model.

nhanes_race <- complete_nhanes_df$race # Save a vector of race from NHANES
aou_race <- complete_aou_df$race # Save a vector of race from All of Us

race_df <- data.frame(Race = c("White","Black or African American","Hispanic or Latino","Asian","Other/Multiracial"),
                      NHANES = as.vector(wpct(nhanes_race,vector_nhanes_weights)), # Save weighted proportion of each racial and ethnic group from NHANES
                      `All of Us (Unweighted)` = as.vector(prop.table(table(aou_race))), # Save unweighted proprotion of each racial and ethnic group from All of Us
                      `All of Us (Weighted)` = as.vector(wpct(aou_race,vector_aou_weights))) # Save weighted proprotion of each racial and ethnic group from All of Us

colnames(race_df)[c(3,4)] <- c("All of Us (unweighted)", "All of Us (weighted)")

race_df$`ACS` <- c(sum(pop_totals[pop_totals$race == 1,]$pop_total)/sum(pop_totals$pop_total),
                         sum(pop_totals[pop_totals$race == 2,]$pop_total)/sum(pop_totals$pop_total),
                         sum(pop_totals[pop_totals$race == 3,]$pop_total)/sum(pop_totals$pop_total),
                         sum(pop_totals[pop_totals$race == 4,]$pop_total)/sum(pop_totals$pop_total),
                         sum(pop_totals[pop_totals$race == 5,]$pop_total)/sum(pop_totals$pop_total))
head(race_df)

# This code pivots the data to long format in order to easily plot the data

race_df <- race_df %>%
            pivot_longer(cols=c("NHANES","All of Us (unweighted)","All of Us (weighted)","ACS"),
                         names_to = "Source",
                         values_to = "Proportion")

race_plot <- ggplot(data=race_df,aes(x=factor(Race, level=c("White","Black or African American","Hispanic or Latino","Asian","Other/Multiracial")),y=Proportion,fill=factor(Source, level=c("ACS","NHANES","All of Us (unweighted)","All of Us (weighted)")))) + 
             geom_bar(stat="identity", position = position_dodge(width = 0.7), width = 0.6) + 
             theme_classic() + 
             theme(axis.text.x=element_text(angle=25,vjust=1,hjust=1)) + 
             guides(fill="none") + 
             xlab("") + 
             ylab("Proportion") + 
             scale_fill_manual(values=c("#696969","darkgreen","darkblue","darkorange")) + 
             ggtitle("Race and Ethnicity") +
             scale_x_discrete(limits = levels(race_df$Race))

race_plot

# Education
nhanes_grade <- complete_nhanes_df$education
aou_grade <- complete_aou_df$education

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
nhanes_sex <- complete_nhanes_df$sex_at_birth
aou_sex <- complete_aou_df$sex_at_birth

sex_df <- data.frame(Sex = c("Male","Female"),
                      NHANES = as.vector(wpct(nhanes_sex,vector_nhanes_weights)),
                      `All of Us (Unweighted)` = as.vector(prop.table(table(aou_sex))),
                      `All of Us (Weighted)` = as.vector(wpct(aou_sex,vector_aou_weights)))

colnames(sex_df)[c(3,4)] <- c("All of Us (unweighted)", "All of Us (weighted)")

sex_df$`ACS` <- c(sum(pop_totals[pop_totals$sex == 0,]$pop_total)/sum(pop_totals$pop_total),
                         sum(pop_totals[pop_totals$sex == 1,]$pop_total)/sum(pop_totals$pop_total))
head(sex_df)

sex_df <- sex_df %>%
            pivot_longer(cols=c("NHANES","All of Us (unweighted)","All of Us (weighted)","ACS"),
                         names_to = "Source",
                         values_to = "Proportion")
(sex_df)

sex_plot <- ggplot(data=sex_df,aes(x=factor(Sex, level=c("Male","Female")),y=Proportion,fill=factor(Source, level=c("ACS","NHANES","All of Us (unweighted)","All of Us (weighted)")))) + 
              geom_bar(stat="identity", position = position_dodge(width = 0.7), width = 0.6) + 
              theme_classic() + 
              theme(axis.text.x=element_text(angle=25,vjust=1,hjust=1),legend.position="bottom",legend.title=element_blank()) + 
#               guides(fill="none") + 
              xlab("") +
              scale_fill_manual(values=c("#696969","darkgreen","darkblue","darkorange")) + 
              ggtitle("Sex at Birth") +
              scale_x_discrete(limits = levels(sex_df$Sex))

sex_plot

# Nativity
nhanes_nativity <- complete_nhanes_df$nativity
aou_nativity <- complete_aou_df$nativity

nativity_df <- data.frame(Nativity = c("Born in US", "Born Outside US"),
                            NHANES = as.vector(wpct(nhanes_nativity,vector_nhanes_weights)),
                            `All of Us (Unweighted)` = as.vector(prop.table(table(aou_nativity))),
                            `All of Us (Weighted)` = as.vector(wpct(aou_nativity,vector_aou_weights)))

colnames(nativity_df)[c(3,4)] <- c("All of Us (unweighted)", "All of Us (weighted)")

head(nativity_df)

nativity_df <- nativity_df %>%
            pivot_longer(cols=c("NHANES","All of Us (unweighted)","All of Us (weighted)"),
                         names_to = "Source",
                         values_to = "Proportion")

head(nativity_df)

nativity_plot <- ggplot(data=nativity_df,aes(x=factor(Nativity, level=c("Born in US", "Born Outside US")),y=Proportion,fill=factor(Source, level=c("NHANES","All of Us (unweighted)","All of Us (weighted)")))) + 
              geom_bar(stat="identity", position = position_dodge(width = 0.7), width = 0.6) + 
              theme_classic() + 
              theme(axis.text.x=element_text(angle=25,vjust=1,hjust=1)) + 
              guides(fill="none") + 
              xlab("") +
              scale_fill_manual(values=c("darkgreen","darkblue","darkorange")) + 
              ggtitle("Nativity") +
              scale_x_discrete(limits = levels(nativity_df$Nativity))

nativity_plot

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
nhanes_alcohol <- complete_nhanes_df$drinking_habits
aou_alcohol <- complete_aou_df$drinking_habits

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
nhanes_smoking <- complete_nhanes_df$smoking_habits
aou_smoking <- complete_aou_df$smoking_habits

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
    
    df$education <- as.factor(df$education)
#     df$education <- relevel(df$education, ref="1")
    
    df$marital_status <- as.factor(df$marital_status)
#     df$marital_status <- relevel(df$marital_status, ref="1")
    
    df$drinking_habits <- as.factor(df$drinking_habits)
#     df$drinking_habits <- relevel(df$drinking_habits, ref="1")
    
    df$smoking_habits <- as.factor(df$smoking_habits)
#     df$smoking_habits <- relevel(df$smoking_habits, ref="1")
    
    df$overall_health <- as.factor(df$overall_health)
    
#     df$sex_at_birth[df$sex_at_birth == 1] <- 0
#     df$sex_at_birth[df$sex_at_birth == 2] <- 1
    df$sex_at_birth <- as.factor(df$sex_at_birth)
    
#     df$health_insurance[df$health_insurance == 1] <- 0
#     df$health_insurance[df$health_insurance == 2] <- 1
    df$health_insurance <- as.factor(df$health_insurance)
    
    df$nativity[df$nativity == 1] <- 0
    df$nativity[df$nativity == 2] <- 1
    df$nativity <- as.factor(df$nativity)
    
    
#     df <- df %>% 
#             relocate(race, education, marital_status, drinking_habits, smoking_habits) %>%
#             relocate(sex_at_birth, health_insurance, nativity) %>%
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
        }else if(y_char == "sex_at_birth" | y_char == "health_insurance" | y_char == "nativity"){ # If any of these
            y_type <- "binary" # Variable type is binary
        }else if(y_char == "race" | y_char == "education" | y_char == "marital_status" | y_char == "smoking_habits"| y_char == "drinking_habits" | y_char == "overall_health"){ # If any of these
            y_type <- "multinomial" # Variable type is multinomial (more than 2 categories)
        }
            
        if(x_char == "age"){ # If age
            x_type <- "number" # Variable type is numerical
        }else if(x_char == "sex_at_birth" | x_char == "health_insurance" | x_char == "nativity"){ # If any of these
            x_type <- "binary" # Variable type is binary
        }else if(x_char == "race" | x_char == "education" | x_char == "marital_status" | x_char == "smoking_habits"| x_char == "drinking_habits" | x_char == "overall_health"){ # If any of these
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
      
# Define the mapping as a named vector
name_map <- c(
  "age" = "Age",
  "race" = "Race and Ethnicity",
  "race_1" = "Race and Ethnicity (White)", 
  "race_2" = "Race and Ethnicity (Black or African American)",
  "race_3" = "Race and Ethnicity (Hispanic or Latino)",
  "race_4" = "Race and Ethnicity (Asian)",
  "race_5" = "Race and Ethnicity (Other/Multiracial)",
  "sex_at_birth" = "Sex",
  "education" = "Education",  
  "education_1" = "Education (Less than 9th grade)",
  "education_2" = "Education (9-11th grade)",
  "education_3" = "Education (High school graduate/GED)",
  "education_4" = "Education (Some college/AA)",
  "education_5" = "Education (College graduate or above)",
  "marital_status" = "Marital status",  
  "marital_status_1" = "Marital status (Married/Living with Partner)",
  "marital_status_2" = "Marital status (Widowed/Divorced/Separated)",
  "marital_status_3" = "Marital status (Never married)",
  "health_insurance" = "Health insurance",
  "nativity" = "Nativity",
  "drinking_habits" = "Drinking habits",
  "drinking_habits_1" = "Drinking habits (Never drank)",
  "drinking_habits_2" = "Drinking habits (Less than yearly)",
  "drinking_habits_3" = "Drinking habits (Monthly or less)",
  "drinking_habits_4" = "Drinking habits (Two to four times a month)",
  "drinking_habits_5" = "Drinking habits (Twice or more a week)",
  "smoking_habits" = "Smoking habits",
  "smoking_habits_1" = "Smoking habits (Never smoked)",
  "smoking_habits_2" = "Smoking habits (Former smoker)",
  "smoking_habits_3" = "Smoking habits (Sometimes smokes)",
  "smoking_habits_4" = "Smoking habits (Daily smoker)",
  "overall_health" = "Overall Health",  
  "overall_health_1" = "Overall Health (Poor)",
  "overall_health_2" = "Overall Health (Fair)",
  "overall_health_3" = "Overall Health (Good)",
  "overall_health_4" = "Overall Health (Very Good)",
  "overall_health_5" = "Overall Health (Excellent)"
)

# Function to replace terms in the "formula" column of corr_df_2
replace_terms <- function(text_column, replacements) {
  for (key in names(replacements)) {
    text_column <- gsub(paste0("\\b", key, "\\b"), replacements[key], text_column)
  }
  return(text_column)
}

# Apply the function to the "formula" column in corr_df_2
corr_df_2 <- corr_df_2 %>%
  mutate(formula = replace_terms(formula, name_map))

corr_df <- corr_df %>%
  mutate(formula = replace_terms(formula, name_map))

corr_plot <- ggplot(corr_df_2, aes(x=formula,y=value,shape=variable)) +
                geom_point(aes(color=diff_lab,shape=variable),size=2) + 
                theme_classic(base_size=6) +
                theme(legend.position = "top",axis.text.x = element_text(angle=55,hjust=1),legend.title=element_blank()) +
                scale_color_manual(values = c("black","purple"),labels=c(bquote(r[diff] < 0.05),bquote(r[diff] > 0.05))) +
                scale_shape_manual(values= c(0,1,2)) +
                guides(color="none",shape="none") +
                xlab("") + ylab("Correlation coefficient (r)") +
                theme(legend.position.inside = c(0.5,0.5),plot.margin = margin(10, 10, 10, 100))
                

corr_plot

# Compare beta coefficients
complete_nhanes_df$nativity[complete_nhanes_df$nativity == 1] <- 0
complete_nhanes_df$nativity[complete_nhanes_df$nativity == 2] <- 1

complete_aou_df$nativity[complete_aou_df$nativity == 1] <- 0
complete_aou_df$nativity[complete_aou_df$nativity == 2] <- 1

demo1720 <- read_xpt("P_DEMO.xpt")
demo2123 <- read_xpt("DEMO_L.xpt")

demo1720 <- demo1720 %>%
                select(id=SEQN,
                      psu=SDMVPSU,
                      stra=SDMVSTRA)

demo2123 <- demo2123 %>%
                select(id=SEQN,
                      psu=SDMVPSU,
                      stra=SDMVSTRA)

demo <- rbind(demo1720,demo2123)

complete_nhanes_df <- left_join(complete_nhanes_df,demo,by="id")

aou_weights <- complete_aou_df$weight

nhanes_psu <- complete_nhanes_df$psu
nhanes_stra <- complete_nhanes_df$stra
nhanes_weights <- complete_nhanes_df$weight

aou_regression <- complete_aou_df %>%
                    subset(select=-c(id,data_label,participation,weight)) %>%
                    select(everything(),age)

nhanes_regression <- complete_nhanes_df %>%
                    subset(select=-c(id,data_label,participation,weight,psu,stra)) %>%
                    select(everything(),age)

aou_regression <- dummy_cols(aou_regression,
                        select_columns=c("race","marital_status","education",
                                         "overall_health","drinking_habits","smoking_habits"),
                        ignore_na=TRUE,
                        remove_first_dummy = FALSE,
                        remove_selected_columns = TRUE) # Convert categorical variables to dummy variables

aou_regression <- aou_regression %>% relocate(age,.after=last_col())

nhanes_regression <- dummy_cols(nhanes_regression,
                        select_columns=c("race","marital_status","education",
                                         "overall_health","drinking_habits","smoking_habits"),
                        ignore_na=TRUE,
                        remove_first_dummy = FALSE,
                        remove_selected_columns = TRUE) # Convert categorical variables to dummy variables

nhanes_regression <- nhanes_regression %>% relocate(age,.after=last_col())

# Define groups to avoid intra-category comparisons
categories <- list(
  race = c("race_1", "race_2", "race_3", "race_4", "race_5"),
  marital_status = c("marital_status_1", "marital_status_2", "marital_status_3"),
  education = c("education_1", "education_2", "education_3", "education_4", "education_5"),
  overall_health = c("overall_health_1", "overall_health_2", "overall_health_3", "overall_health_4", "overall_health_5"),
  drinking_habits = c("drinking_habits_1", "drinking_habits_2", "drinking_habits_3", "drinking_habits_4", "drinking_habits_5"),
  smoking_habits = c("smoking_habits_1", "smoking_habits_2", "smoking_habits_3", "smoking_habits_4")
)

# Function to check if two variables belong to the same category
same_category <- function(var1, var2, categories) {
  for (group in categories) {
    if (var1 %in% group & var2 %in% group) {
      return(TRUE)  # Skip this pair
    }
  }
  return(FALSE)  # Proceed with regression
}

# Initialize dataframe
beta_df <- data.frame(formula = character(),
                      beta_nhanes = numeric(),
                      beta_aou = numeric(),
                      beta_waou = numeric(),
                      stringsAsFactors = FALSE)

beta_graphing <- data.frame(formula = character(),
                           beta = numeric(),
                           lower = numeric(),
                           upper = numeric(),
                           dataset = character())

# Loop through all column pairs
for (y in 1:(ncol(aou_regression)-1)) {
  for (x in (y+1):ncol(aou_regression)) {
    
    var_y <- colnames(aou_regression)[y]
    var_x <- colnames(aou_regression)[x]

    # Skip comparisons within the same category
    if (same_category(var_y, var_x, categories)) next  

    formula_to_paste <- paste0(var_y, " ~ ", var_x)
    
    # NHANES Regression
    temp_nhanes <- nhanes_regression[, c(y, x)]
    colnames(temp_nhanes) <- c("y", "x")
    temp_nhanes$y <- as.integer(temp_nhanes$y)
    temp_nhanes$psu <- nhanes_psu
    temp_nhanes$stra <- nhanes_stra
    temp_nhanes$weight <- nhanes_weights  
    
    NHANES_all <- svydesign(data=temp_nhanes, id=~psu, strata=~stra, weights=~weight, nest=TRUE)
    model_nhanes <- svyglm(y ~ x, design=NHANES_all, family="binomial")
    nhanes_beta <- summary(model_nhanes)$coefficients["x", 1]
      
    nhanes_interval <- confint(model_nhanes,verbose=F)
    nhanes_lower <- nhanes_interval["x",1]
    nhanes_upper <- nhanes_interval["x",2]
    
    # AOU Regression
    temp_aou <- aou_regression[, c(y, x)]
    colnames(temp_aou) <- c("y", "x")
    temp_aou$y <- as.integer(temp_aou$y)
    
    model_aou <- glm(y ~ x, data = temp_aou, family = "binomial")
    aou_beta <- summary(model_aou)$coefficients["x", 1]
      
    aou_interval <- confint(model_aou,verbose=F)
    aou_lower <- aou_interval["x",1]
    aou_upper <- aou_interval["x",2]  

    # WAOU Regression
    temp_waou <- aou_regression[, c(y, x)]
    colnames(temp_waou) <- c("y", "x")
    temp_waou$y <- as.integer(temp_waou$y)
    temp_waou$weight <- aou_weights  
    
    waou_survey <- svydesign(ids=~0,weights=~weight,data=temp_waou)
    model_waou <- svyglm(y ~ x,design=waou_survey,family="binomial")
    waou_beta <- summary(model_waou)$coefficients["x",1]
      
    waou_interval <- confint(model_waou,verbose=F)
    waou_lower <- waou_interval["x",1]
    waou_upper <- waou_interval["x",2]  
    

    # Append results
    beta_df <- rbind(beta_df, data.frame(formula = formula_to_paste, 
                                         beta_nhanes = nhanes_beta, 
                                         beta_aou = aou_beta, 
                                         beta_waou = waou_beta))
      
    beta_graphing <- rbind(beta_graphing, data.frame(formula = c(formula_to_paste,formula_to_paste,formula_to_paste),
                                                    beta = c(nhanes_beta,aou_beta,waou_beta),
                                                    lower = c(nhanes_lower,aou_lower,waou_lower),
                                                    upper = c(nhanes_upper,aou_upper,waou_upper),
                                                    dataset = c("NHANES","All of Us (unweighted)","All of Us (weighted)") 
                                                    ))  
  }
}

# Define the mapping as a named vector
name_map <- c(
  "age" = "Age",
  "race" = "Race and Ethnicity",
  "race_1" = "Race and Ethnicity (White)", 
  "race_2" = "Race and Ethnicity (Black or African American)",
  "race_3" = "Race and Ethnicity (Hispanic or Latino)",
  "race_4" = "Race and Ethnicity (Asian)",
  "race_5" = "Race and Ethnicity (Other/Multiracial)",
  "sex_at_birth" = "Sex",
  "education" = "Education",  
  "education_1" = "Education (Less than 9th grade)",
  "education_2" = "Education (9-11th grade)",
  "education_3" = "Education (High school graduate/GED)",
  "education_4" = "Education (Some college/AA)",
  "education_5" = "Education (College graduate or above)",
  "marital_status" = "Marital status",  
  "marital_status_1" = "Marital status (Married/Living with Partner)",
  "marital_status_2" = "Marital status (Widowed/Divorced/Separated)",
  "marital_status_3" = "Marital status (Never married)",
  "health_insurance" = "Health insurance",
  "nativity" = "Nativity",
  "drinking_habits" = "Drinking habits",
  "drinking_habits_1" = "Drinking habits (Never drank)",
  "drinking_habits_2" = "Drinking habits (Less than yearly)",
  "drinking_habits_3" = "Drinking habits (Monthly or less)",
  "drinking_habits_4" = "Drinking habits (Two to four times a month)",
  "drinking_habits_5" = "Drinking habits (Twice or more a week)",
  "smoking_habits" = "Smoking habits",
  "smoking_habits_1" = "Smoking habits (Never smoked)",
  "smoking_habits_2" = "Smoking habits (Former smoker)",
  "smoking_habits_3" = "Smoking habits (Sometimes smokes)",
  "smoking_habits_4" = "Smoking habits (Daily smoker)",
  "overall_health" = "Overall Health",  
  "overall_health_1" = "Overall Health (Poor)",
  "overall_health_2" = "Overall Health (Fair)",
  "overall_health_3" = "Overall Health (Good)",
  "overall_health_4" = "Overall Health (Very Good)",
  "overall_health_5" = "Overall Health (Excellent)"
)

# Function to replace terms in the "formula" column of corr_df_2
replace_terms <- function(text_column, replacements) {
  for (key in names(replacements)) {
    text_column <- gsub(paste0("\\b", key, "\\b"), replacements[key], text_column)
  }
  return(text_column)
}

beta_df <- beta_df %>%
  mutate(formula = replace_terms(formula, name_map))

beta_graphing <- beta_graphing %>%
  mutate(formula = replace_terms(formula, name_map))

uw_model <- lm(beta_aou ~ beta_nhanes, data=beta_df)
summary(uw_model)

w_model <- lm(beta_waou ~ beta_nhanes, data=beta_df)
summary(w_model)

beta_g <- ggplot(beta_df,aes(x=beta_nhanes,y=beta_aou)) + 
                    geom_point(color="gray69",size=3) + 
                    geom_smooth(method = "lm",se=TRUE,col="black") + 
                    geom_abline(slope=1, intercept=0, linetype='dashed', col = 'red',linewidth=1) + 
                    theme_classic() +
                    ggtitle("Effect Size Comparison") +
                    xlab("NHANES Effect Sizes") +
                    ylab("All of Us (unweighted) Effect Sizes")

beta_g <- ggplot(beta_df,aes(x=beta_nhanes,y=beta_waou)) + 
                    geom_point(color="gray69",size=3) + 
                    geom_smooth(method = "lm",se=TRUE,col="black") + 
                    geom_abline(slope=1, intercept=0, linetype='dashed', col = 'red',linewidth=1) + 
                    theme_classic() +
                    ggtitle("Effect Size Comparison") +
                    xlab("NHANES Effect Sizes") +
                    ylab("All of Us (weighted) Effect Sizes")

## Graphing
beta_df$diff <- abs(beta_df$beta_aou - beta_df$beta_nhanes)
beta_df <- beta_df %>%
                arrange(desc(diff))

graphing_df <- data.frame(formula = as.factor(beta_df$formula))
graphing_df <- left_join(graphing_df,beta_graphing,by="formula")

graphing_df2 <- graphing_df %>% filter(formula %in% formulas[1:30])
graphing_df2$formula <- factor(graphing_df2$formula,levels=rev(unique(graphing_df2$formula)))

beta_plot <- ggplot(graphing_df2, aes(x = beta, y = formula, color = factor(dataset,c("NHANES","All of Us (unweighted)","All of Us (weighted)")))) +
  geom_stripes(odd = "#33333333", even = "white") +
  geom_point(position = position_dodge(width = 0.5), size = 5) +  # Points for beta
  geom_errorbar(aes(xmin = lower, xmax = upper), 
                position = position_dodge(width = 0.5), width = 0.2) +  # Error bars
  theme_classic() +
  labs(x = "Effect Size Estimates",y="") +
  theme(legend.position = "bottom",legend.title = element_blank()) +  # Move legend below
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  scale_color_manual(values=c("darkgreen","darkblue","darkorange"))
#   coord_flip()

beta_plot

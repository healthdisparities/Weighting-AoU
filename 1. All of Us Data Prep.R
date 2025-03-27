# Demographics
demographics_df <- demographics_df %>%
  mutate(sex_at_birth = case_when(
    sex_at_birth == "Male" ~ 0,      # Assign 0 to Male
    sex_at_birth == "Female" ~ 1,    # Assign 1 to Female
    TRUE ~ NA_real_         # Everything else to NA (ensures numeric output)
  ))

demographics_df <- demographics_df %>%
  mutate(
    race_ethnicity = case_when(
      race == "White" & ethnicity == "Not Hispanic or Latino" ~ 1,  # Non-Hispanic White
      race == "Black or African American" & ethnicity == "Not Hispanic or Latino" ~ 2,  # Non-Hispanic Black
      ethnicity == "Hispanic or Latino" ~ 3,  # Hispanic or Latino
      race == "Asian" & ethnicity == "Not Hispanic or Latino" ~ 4,  # Non-Hispanic Asian
      race %in% c("More than one population", 
                  "Native Hawaiian or Other Pacific Islander", 
                  "Middle Eastern or North African") ~ 5,  # Other/Multiracial
      TRUE ~ NA_real_  # Everything else to NA
    )
  )

colnames(demographics_df)[1] <- "id"
demographics_df <- demographics_df %>%
                    subset(select=c(id,sex_at_birth,age,race_ethnicity))
colnames(demographics_df)[4] <- "race"

# Basics
basics_df 

## Education
education_df <- basics_df[basics_df$question == "Education Level: Highest Grade",]

education_df <- education_df %>%
  mutate(
    education = case_when(
      answer %in% c("Highest Grade: Never Attended",
                    "Highest Grade: One Through Four",
                    "Highest Grade: Five Through Eight") ~ 1, #Less than 9th grade
      answer == "Highest Grade: Nine Through Eleven" ~ 2, #9-11th
      answer == "Highest Grade: Twelve Or GED" ~ 3, #Twelve or GED
      answer == "Highest Grade: College One to Three" ~ 4, #Some college or AA
      answer == "Highest Grade: College Graduate" ~ 5, #College Graduate
      answer == "Highest Grade: Advanced Degree" ~ 5, #Advanced degree  
      TRUE ~ NA_real_  # Everything else to NA
    )
  )

education_df <- education_df %>%
                    subset(select=c(person_id,education))

## Health insurance
health_insurance_df <- basics_df[basics_df$question == "Insurance: Health Insurance",]

health_insurance_df <- health_insurance_df %>%
  mutate(
    health_insurance = case_when(
      answer == "Health Insurance: No" ~ 0,  #No
      answer == "Health Insurance: Yes" ~ 1, #Yes
      TRUE ~ NA_real_  # Everything else to NA
    )
  )

health_insurance_df <- health_insurance_df %>%
                    subset(select=c(person_id,health_insurance))

## Marital status
marital_status_df <- basics_df[basics_df$question == "Marital Status: Current Marital Status",]

marital_status_df <- marital_status_df %>%
  mutate(
    marital_status = case_when(
      answer == "Current Marital Status: Married" ~ 1,  # Married/Living with Partner
      answer == "Current Marital Status: Living With Partner" ~ 1,  # Married/Living with Partner
      answer == "Current Marital Status: Widowed" ~ 2, #Widowed
      answer == "Current Marital Status: Divorced" ~ 2, #Divorced
      answer == "Current Marital Status: Separated" ~ 2, #Separated
      answer == "Current Marital Status: Never Married" ~ 3, #Never married
      TRUE ~ NA_real_  # Everything else to NA
    )
  )

marital_status_df <- marital_status_df %>%
                    subset(select=c(person_id,marital_status))

## Nativity
nativity_df <- basics_df[basics_df$question == "The Basics: Birthplace",]

nativity_df <- nativity_df %>%
  mutate(
    nativity = case_when(
      answer == "Birthplace: USA" ~ 1,  # Born in US
      answer == "PMI: Other" ~ 2, #Other
      TRUE ~ NA_real_  # Everything else to NA
    )
  )

nativity_df <- nativity_df %>%
                    subset(select=c(person_id,nativity))

## Merge basics
df_list <- list(education_df,health_insurance_df,marital_status_df,nativity_df)

basics_df_final <- reduce(df_list, full_join, by = "person_id")  # Replace with actual column name
colnames(basics_df_final)[1] <- "id"

# General health
general_health_df <- general_health_df %>%
  mutate(
    overall_health = case_when(
      answer == "General Health: Poor" ~ 1,  # Poor
      answer == "General Health: Fair" ~ 2, #Fair
      answer == "General Health: Good" ~ 3, #Good
      answer == "General Health: Very Good" ~ 4, #Very good
      answer == "General Health: Excellent" ~ 5, #Excellent
      TRUE ~ NA_real_  # Everything else to NA
    )
  )

general_health_df <- general_health_df %>%
                        subset(select=c(person_id,overall_health))
colnames(general_health_df)[1] <- "id"

# Smoking
smoking_df 

ever_smoked_df <- smoking_df[smoking_df$question == "Smoking: 100 Cigs Lifetime",]
current_smoke_df <- smoking_df[smoking_df$question == "Smoking: Smoke Frequency",]

colnames(ever_smoked_df)[c(1,4)] <- c("id","ever_smoke")
ever_smoked_df <- ever_smoked_df[,c(1,4)]
colnames(current_smoke_df)[c(1,4)] <- c("id","current_smoke")
current_smoke_df <- current_smoke_df[,c(1,4)]
smoking_df <- full_join(ever_smoked_df,current_smoke_df,by="id")

smoking_df <- smoking_df %>%
  mutate(
    smoking_habits = case_when(
      ever_smoke == "100 Cigs Lifetime: No" ~ 1,  # Never smoked
      ever_smoke == "100 Cigs Lifetime: Yes" & current_smoke == "Smoke Frequency: Not At All" ~ 2,  #Former smoker
      current_smoke == "Smoke Frequency: Some Days" ~ 3,  # Sometimes smokes
      current_smoke == "Smoke Frequency: Every Day" ~ 4,  # Daily smoker
      TRUE ~ NA_real_  # Everything else to NA
    )
  )

smoking_df <- smoking_df %>%
                subset(select=c(id,smoking_habits))

# Alcohol
alcohol_df 

ever_drank_df <- alcohol_df[alcohol_df$question == "Alcohol: Alcohol Participant",]
current_drink_df <- alcohol_df[alcohol_df$question == "Alcohol: Drink Frequency Past Year",]

colnames(ever_drank_df)[c(1,4)] <- c("id","ever_drank")
ever_drank_df <- ever_drank_df[,c(1,4)]
colnames(current_drink_df)[c(1,4)] <- c("id","current_drink")
current_drink_df <- current_drink_df[,c(1,4)]
alcohol_df <- full_join(ever_drank_df,current_drink_df,by="id")

alcohol_df <- alcohol_df %>%
  mutate(
    drinking_habits = case_when(
      ever_drank == "Alcohol Participant: No" ~ 1,  # Never drank
      current_drink == "Drink Frequency Past Year: Never" ~ 2,  #Less than yearly
      current_drink == "Drink Frequency Past Year: Monthly Or Less" ~ 3,  # Monthly or less
      current_drink == "Drink Frequency Past Year: 2 to 4 Per Month" ~ 4,  # Two to four times a month
      current_drink %in% c("Drink Frequency Past Year: 2 to 3 Per Week",
                           "Drink Frequency Past Year: 4 or More Per Week")~ 5, # Twice or more a week
      TRUE ~ NA_real_  # Everything else to NA
    )
  )

alcohol_df <- alcohol_df %>%
                subset(select=c(id,drinking_habits))

# Final merge
df_list <- list(demographics_df,basics_df_final,general_health_df,smoking_df,alcohol_df)

aou_df_final <- reduce(df_list, full_join, by = "id")

aou_df_final <- aou_df_final %>%
                    subset(select=c(id,
                                   age,
                                   race,
                                   sex_at_birth,
                                   nativity,
                                   marital_status,
                                   education,
                                   health_insurance,
                                   overall_health,
                                   drinking_habits,
                                   smoking_habits))

aou_df_final <- (aou_df_final[complete.cases(aou_df_final),])
aou_df_final <- aou_df_final[aou_df_final$age >= 18 & aou_df_final$age < 80,]

aou_df_final$data_label <- "All of Us"
aou_df_final$participation <- 1

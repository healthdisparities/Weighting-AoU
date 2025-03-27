library(dplyr)
library(haven)
library(tidyverse)

# 2017-2020

## Import data
demographics_1720 <- read_xpt("P_DEMO.xpt")
alcohol_1720 <- read_xpt("P_ALQ.xpt")
insurance_1720 <- read_xpt("P_HIQ.xpt")
smoking_1720 <- read_xpt("P_SMQ.xpt")
oh_1720 <- read_xpt("P_HUQ.xpt")

## Demographics
demographics_1720 <- demographics_1720 %>% 
                        rename(
                            id = SEQN,
                            age = RIDAGEYR,
                            race = RIDRETH3,
                            birthplace = DMDBORN4,
                            marital_status = DMDMARTZ,
                            highest_grade = DMDEDUC2,
                            gender = RIAGENDR,
                            weight = WTMECPRP
                        ) %>%
                      select(id, age, race, birthplace, marital_status, highest_grade, gender, weight)

demographics_1720 <- demographics_1720 %>%
  mutate(
    race = case_when(
      race == 3 ~ 1,  # White
      race == 4 ~ 2,  # Black/African American
      race == 1 ~ 3,  # Hispanic or Latino
      race == 2 ~ 3,  # Hispanic or Latino
      race == 6 ~ 4, # Asian
      race == 7 ~ 5, #Other/multiracial
      TRUE ~ NA_real_  # Everything else to NA
    ),
    sex_at_birth = case_when(
        gender == 1 ~ 0, # Male
        gender == 2 ~ 1, # Female
        TRUE ~ NA_real_  # Everything else to NA
    ),
    education = case_when(
        highest_grade == 1 ~ 1,
        highest_grade == 2 ~ 2,
        highest_grade == 3 ~ 3,
        highest_grade == 4 ~ 4,
        highest_grade == 5 ~ 5,
        TRUE ~ NA_real_  # Everything else to NA
    ),
    marital_status = case_when(
        marital_status == 1 ~ 1,
        marital_status == 2 ~ 2,
        marital_status == 3 ~ 3,
        TRUE ~ NA_real_  # Everything else to NA
    ),
    nativity = case_when(
        birthplace == 1 ~ 1,
        birthplace == 2 ~ 2,
        TRUE ~ NA_real_  # Everything else to NA
    )
  )

demographics_1720 <- demographics_1720 %>%
                        subset(select=c(id,age,race,sex_at_birth,nativity,marital_status,education,weight))

## Health Insurance
insurance_1720 <- insurance_1720 %>% 
                        rename(
                            id = SEQN,
                            health_insurance = HIQ011
                        )

insurance_1720 <- insurance_1720 %>% subset(select=c(id,health_insurance))

insurance_1720 <- insurance_1720 %>%
  mutate(
    health_insurance = case_when(
      health_insurance == 2 ~ 0,  # No
      health_insurance == 1 ~ 1, # Yes
      TRUE ~ NA_real_  # Everything else to NA
    )
)

## Overall health
oh_1720 <- oh_1720 %>%
                rename(
                    id = SEQN,
                    overall_health = HUQ010
                )

oh_1720 <- oh_1720 %>% subset(select=c(id,overall_health))

oh_1720 <- oh_1720 %>%
  mutate(
    overall_health = case_when(
      overall_health == 5 ~ 1,  # Poor
      overall_health == 4 ~ 2, #Fair
      overall_health == 3 ~ 3, #Good
      overall_health == 2 ~ 4, #Very good
      overall_health == 1 ~ 5, #Excellent
      TRUE ~ NA_real_  # Everything else to NA
    )
  )

## Alcohol
alcohol_1720 <- alcohol_1720 %>% 
                        rename(
                            id = SEQN,
                            ever_drank = ALQ111,
                            current_drink = ALQ121
                        )

alcohol_1720 <- alcohol_1720 %>% subset(select=c(id,ever_drank,current_drink))

alcohol_1720$ever_drank[alcohol_1720$ever_drank == 7] <- NA
alcohol_1720$ever_drank[alcohol_1720$ever_drank == 9] <- NA

alcohol_1720$current_drink[alcohol_1720$current_drink == 77] <- NA
alcohol_1720$current_drink[alcohol_1720$current_drink == 99] <- NA

## Smoking
smoking_1720 <- smoking_1720 %>% 
                        rename(
                            id = SEQN,
                            ever_smoke = SMQ020,
                            current_smoke = SMQ040
                        )

smoking_1720 <- smoking_1720 %>% subset(select=c(id,ever_smoke,current_smoke))

smoking_1720$ever_smoke[smoking_1720$ever_smoke == 7] <- NA
smoking_1720$ever_smoke[smoking_1720$ever_smoke == 9] <- NA

smoking_1720$current_smoke[smoking_1720$current_smoke == 7] <- NA
smoking_1720$current_smoke[smoking_1720$current_smoke == 9] <- NA


## Merge
df_list <- list(demographics_1720,insurance_1720,oh_1720,alcohol_1720,smoking_1720)

nhanes_1720 <- reduce(df_list, full_join, by = "id")
nhanes_1720 <- nhanes_1720 %>% filter(age >= 18 & age < 80)

nhanes_1720_og <- nhanes_1720
nhanes_1720 <- nhanes_1720_og

## Imputation
nhanes_1720$race <- as.factor(nhanes_1720$race)
nhanes_1720$sex_at_birth <- as.factor(nhanes_1720$sex_at_birth)
nhanes_1720$nativity <- as.factor(nhanes_1720$nativity)
nhanes_1720$marital_status <- as.factor(nhanes_1720$marital_status)
nhanes_1720$education <- as.factor(nhanes_1720$education)
nhanes_1720$health_insurance <- as.factor(nhanes_1720$health_insurance)
nhanes_1720$overall_health <- as.factor(nhanes_1720$overall_health)
nhanes_1720$ever_drank <- as.factor(nhanes_1720$ever_drank)
nhanes_1720$current_drink <- as.factor(nhanes_1720$current_drink)
nhanes_1720$ever_smoke <- as.factor(nhanes_1720$ever_smoke)
nhanes_1720$current_smoke <- as.factor(nhanes_1720$current_smoke)

### Impute everything EXCEPT recent drinking and smoking habits
nhanes_1720 <- nhanes_1720 %>% subset(select=-c(current_drink,current_smoke))

pred_matrix <- make.predictorMatrix(nhanes_1720) #Exclude id and weight from being used in imputation
pred_matrix[,"id"] <- 0
pred_matrix[,"weight"] <- 0

nhanes_1720_impute_evers <- nhanes_1720 %>%
                    mice(m=1,
                         predictorMatrix = pred_matrix) %>%
                    complete()

###Separate into smokers and nonsmokers
nhanes_1720_current_smoke <- nhanes_1720_og %>% subset(select=c(id,current_smoke))
nhanes_1720_current_smoke$current_smoke <- as.factor(nhanes_1720_current_smoke$current_smoke)

nhanes_1720_nonsmokers <- nhanes_1720_impute_evers[nhanes_1720_impute_evers$ever_smoke == 2,]
nhanes_1720_nonsmokers <- left_join(nhanes_1720_nonsmokers,nhanes_1720_current_smoke,by="id")

nhanes_1720_smokers <- nhanes_1720_impute_evers[nhanes_1720_impute_evers$ever_smoke == 1,]
nhanes_1720_smokers <- left_join(nhanes_1720_smokers,nhanes_1720_current_smoke,by="id")

### Impute recent smoking habits
pred_matrix <- make.predictorMatrix(nhanes_1720_smokers)
pred_matrix[,"id"] <- 0
pred_matrix[,"weight"] <- 0

nhanes_1720_smokers_impute <- nhanes_1720_smokers %>%
                    mice(m=1,
                         predictorMatrix = pred_matrix) %>%
                    complete()

### Recombine datasets
nhanes_1720_imputed <- rbind(nhanes_1720_nonsmokers,nhanes_1720_smokers_impute)

nhanes_1720_imputed <- nhanes_1720_imputed %>%
  mutate(
    smoking_habits = case_when(
      ever_smoke == 2 ~ 1,  # Never smoked
      ever_smoke == 1 & current_smoke == 3 ~ 2,  #Former smoker
      current_smoke == 2 ~ 3,  # Sometimes smokes
      current_smoke == 1 ~ 4,  # Daily smoker
      TRUE ~ NA_real_  # Everything else to NA
    )
  )

nhanes_1720_imputed <- nhanes_1720_imputed %>%
                subset(select=-c(ever_smoke,current_smoke))
nhanes_1720_imputed$smoking_habits <- as.factor(nhanes_1720_imputed$smoking_habits)

### Separate imputed dataset into drinkers, and non drinkers
nhanes_1720_current_drink <- nhanes_1720_og %>% subset(select=c(id,current_drink))
nhanes_1720_current_drink$current_drink <- as.factor(nhanes_1720_current_drink$current_drink)

nhanes_1720_nondrinkers <- nhanes_1720_imputed[nhanes_1720_imputed$ever_drank == 2,]
nhanes_1720_nondrinkers <- left_join(nhanes_1720_nondrinkers,nhanes_1720_current_drink,by="id")

nhanes_1720_drinkers <- nhanes_1720_imputed[nhanes_1720_imputed$ever_drank == 1,]
nhanes_1720_drinkers <- left_join(nhanes_1720_drinkers,nhanes_1720_current_drink,by="id")

### Impute recent drinking habits for drinkers
pred_matrix <- make.predictorMatrix(nhanes_1720_drinkers)
pred_matrix[,"id"] <- 0
pred_matrix[,"weight"] <- 0

nhanes_1720_drinkers_impute <- nhanes_1720_drinkers %>%
                    mice(m=1,
                         predictorMatrix = pred_matrix) %>%
                    complete()

### Recombine datasets
nhanes_1720_imputed <- rbind(nhanes_1720_nondrinkers,nhanes_1720_drinkers_impute)

nhanes_1720_imputed <- nhanes_1720_imputed %>%
  mutate(
    drinking_habits = case_when(
      ever_drank == 2 ~ 1,  # Never drank
      current_drink == 0 ~ 2,  #Less than yearly
      current_drink %in% c(7,8,9,10) ~ 3,  # Monthly or less
      current_drink %in% c(5,6) ~ 4,  # Two to four times a month
      current_drink %in% c(1,2,3,4)~ 5, # Twice or more a week
      TRUE ~ NA_real_  # Everything else to NA
    )
  )

nhanes_1720_imputed <- nhanes_1720_imputed %>%
                subset(select=-c(ever_drank,current_drink))

nhanes_1720_imputed$drinking_habits <- as.factor(nhanes_1720_imputed$drinking_habits)

# 2021-2023
##Demographics
demographics_2123 <- demographics_2123 %>% 
                        rename(
                            id = SEQN,
                            age = RIDAGEYR,
                            race = RIDRETH3,
                            birthplace = DMDBORN4,
                            marital_status = DMDMARTZ,
                            highest_grade = DMDEDUC2,
                            gender = RIAGENDR,
                            weight = WTMEC2YR
                        ) %>%
                      select(id, age, race, birthplace, marital_status, highest_grade, gender, weight)

demographics_2123 <- demographics_2123 %>%
  mutate(
    race = case_when(
      race == 3 ~ 1,  # White
      race == 4 ~ 2,  # Black/African American
      race == 1 ~ 3,  # Hispanic or Latino
      race == 2 ~ 3,  # Hispanic or Latino
      race == 6 ~ 4, # Asian
      race == 7 ~ 5, #Other/multiracial
      TRUE ~ NA_real_  # Everything else to NA
    ),
    sex_at_birth = case_when(
        gender == 1 ~ 0, # Male
        gender == 2 ~ 1, # Female
        TRUE ~ NA_real_  # Everything else to NA
    ),
    education = case_when(
        highest_grade == 1 ~ 1,
        highest_grade == 2 ~ 2,
        highest_grade == 3 ~ 3,
        highest_grade == 4 ~ 4,
        highest_grade == 5 ~ 5,
        TRUE ~ NA_real_  # Everything else to NA
    ),
    marital_status = case_when(
        marital_status == 1 ~ 1,
        marital_status == 2 ~ 2,
        marital_status == 3 ~ 3,
        TRUE ~ NA_real_  # Everything else to NA
    ),
    nativity = case_when(
        birthplace == 1 ~ 1,
        birthplace == 2 ~ 2,
        TRUE ~ NA_real_  # Everything else to NA
    )
  )

demographics_2123 <- demographics_2123 %>%
                        subset(select=c(id,age,race,sex_at_birth,nativity,marital_status,education,weight))

## Health insurance
insurance_2123 <- insurance_2123 %>% 
                        rename(
                            id = SEQN,
                            health_insurance = HIQ011
                        )

insurance_2123 <- insurance_2123 %>% subset(select=c(id,health_insurance))

insurance_2123 <- insurance_2123 %>%
  mutate(
    health_insurance = case_when(
      health_insurance == 2 ~ 0,  # No
      health_insurance == 1 ~ 1, # Yes
      TRUE ~ NA_real_  # Everything else to NA
    )
)

## Overall Health
oh_2123 <- oh_2123 %>%
                rename(
                    id = SEQN,
                    overall_health = HUQ010
                )

oh_2123 <- oh_2123 %>% subset(select=c(id,overall_health))

oh_2123 <- oh_2123 %>%
  mutate(
    overall_health = case_when(
      overall_health == 5 ~ 1,  # Poor
      overall_health == 4 ~ 2, #Fair
      overall_health == 3 ~ 3, #Good
      overall_health == 2 ~ 4, #Very good
      overall_health == 1 ~ 5, #Excellent
      TRUE ~ NA_real_  # Everything else to NA
    )
  )

## Alcohol
alcohol_2123 <- alcohol_2123 %>% 
                        rename(
                            id = SEQN,
                            ever_drank = ALQ111,
                            current_drink = ALQ121
                        )
alcohol_2123 <- alcohol_2123 %>% subset(select=c(id,ever_drank,current_drink))

alcohol_2123$ever_drank[alcohol_2123$ever_drank == 7] <- NA
alcohol_2123$ever_drank[alcohol_2123$ever_drank == 9] <- NA

alcohol_2123$current_drink[alcohol_2123$current_drink == 77] <- NA
alcohol_2123$current_drink[alcohol_2123$current_drink == 99] <- NA

## Smoking
smoking_2123 <- smoking_2123 %>% 
                        rename(
                            id = SEQN,
                            ever_smoke = SMQ020,
                            current_smoke = SMQ040
                        )
smoking_2123 <- smoking_2123 %>% subset(select=c(id,ever_smoke,current_smoke))

smoking_2123$ever_smoke[smoking_2123$ever_smoke == 7] <- NA
smoking_2123$ever_smoke[smoking_2123$ever_smoke == 9] <- NA

smoking_2123$current_smoke[smoking_2123$current_smoke == 7] <- NA
smoking_2123$current_smoke[smoking_2123$current_smoke == 9] <- NA

## Merge
df_list <- list(demographics_2123,insurance_2123,oh_2123,alcohol_2123,smoking_2123)

nhanes_2123 <- reduce(df_list, full_join, by = "id")  # Replace with actual column name
nhanes_2123 <- nhanes_2123 %>% filter(age >= 18 & age < 80)

nhanes_2123_og <- nhanes_2123
nhanes_2123 <- nhanes_2123_og

##Impute Process
nhanes_2123$race <- as.factor(nhanes_2123$race)
nhanes_2123$sex_at_birth <- as.factor(nhanes_2123$sex_at_birth)
nhanes_2123$nativity <- as.factor(nhanes_2123$nativity)
nhanes_2123$marital_status <- as.factor(nhanes_2123$marital_status)
nhanes_2123$education <- as.factor(nhanes_2123$education)
nhanes_2123$health_insurance <- as.factor(nhanes_2123$health_insurance)
nhanes_2123$overall_health <- as.factor(nhanes_2123$overall_health)
nhanes_2123$ever_drank <- as.factor(nhanes_2123$ever_drank)
nhanes_2123$current_drink <- as.factor(nhanes_2123$current_drink)
nhanes_2123$ever_smoke <- as.factor(nhanes_2123$ever_smoke)
nhanes_2123$current_smoke <- as.factor(nhanes_2123$current_smoke)

### Impute everything EXCEPT recent drinking and smoking habits
nhanes_2123 <- nhanes_2123 %>% subset(select=-c(current_drink,current_smoke))

pred_matrix <- make.predictorMatrix(nhanes_2123)
pred_matrix[,"id"] <- 0
pred_matrix[,"weight"] <- 0

nhanes_2123_impute_evers <- nhanes_2123 %>%
                    mice(m=1,
                         predictorMatrix = pred_matrix) %>%
                    complete()

### Separate into smokers and nonsmokers
nhanes_2123_current_smoke <- nhanes_2123_og %>% subset(select=c(id,current_smoke))
nhanes_2123_current_smoke$current_smoke <- as.factor(nhanes_2123_current_smoke$current_smoke)

nhanes_2123_nonsmokers <- nhanes_2123_impute_evers[nhanes_2123_impute_evers$ever_smoke == 2,]
nhanes_2123_nonsmokers <- left_join(nhanes_2123_nonsmokers,nhanes_2123_current_smoke,by="id")

nhanes_2123_smokers <- nhanes_2123_impute_evers[nhanes_2123_impute_evers$ever_smoke == 1,]
nhanes_2123_smokers <- left_join(nhanes_2123_smokers,nhanes_2123_current_smoke,by="id")

### Impute recent smoking habits
pred_matrix <- make.predictorMatrix(nhanes_2123_smokers)
pred_matrix[,"id"] <- 0
pred_matrix[,"weight"] <- 0

nhanes_2123_smokers_impute <- nhanes_2123_smokers %>%
                    mice(m=1,
                         predictorMatrix = pred_matrix) %>%
                    complete()

### Recombine datasets
nhanes_2123_imputed <- rbind(nhanes_2123_nonsmokers,nhanes_2123_smokers_impute)

nhanes_2123_imputed <- nhanes_2123_imputed %>%
  mutate(
    smoking_habits = case_when(
      ever_smoke == 2 ~ 1,  # Never smoked
      ever_smoke == 1 & current_smoke == 3 ~ 2,  #Former smoker
      current_smoke == 2 ~ 3,  # Sometimes smokes
      current_smoke == 1 ~ 4,  # Daily smoker
      TRUE ~ NA_real_  # Everything else to NA
    )
  )

nhanes_2123_imputed <- nhanes_2123_imputed %>%
                subset(select=-c(ever_smoke,current_smoke))
nhanes_2123_imputed$smoking_habits <- as.factor(nhanes_2123_imputed$smoking_habits)

### Separate imputed dataset into drinkers, and non drinkers
nhanes_2123_current_drink <- nhanes_2123_og %>% subset(select=c(id,current_drink))
nhanes_2123_current_drink$current_drink <- as.factor(nhanes_2123_current_drink$current_drink)

nhanes_2123_nondrinkers <- nhanes_2123_imputed[nhanes_2123_imputed$ever_drank == 2,]
nhanes_2123_nondrinkers <- left_join(nhanes_2123_nondrinkers,nhanes_2123_current_drink,by="id")

nhanes_2123_drinkers <- nhanes_2123_imputed[nhanes_2123_imputed$ever_drank == 1,]
nhanes_2123_drinkers <- left_join(nhanes_2123_drinkers,nhanes_2123_current_drink,by="id")

### Impute recent drinking habits for drinkers
pred_matrix <- make.predictorMatrix(nhanes_2123_drinkers)
pred_matrix[,"id"] <- 0
pred_matrix[,"weight"] <- 0

nhanes_2123_drinkers_impute <- nhanes_2123_drinkers %>%
                    mice(m=1,
                         predictorMatrix = pred_matrix) %>%
                    complete()

### Recombine datasets
nhanes_2123_imputed <- rbind(nhanes_2123_nondrinkers,nhanes_2123_drinkers_impute)

nhanes_2123_imputed <- nhanes_2123_imputed %>%
  mutate(
    drinking_habits = case_when(
      ever_drank == 2 ~ 1,  # Never drank
      current_drink == 0 ~ 2,  #Less than yearly
      current_drink %in% c(7,8,9,10) ~ 3,  # Monthly or less
      current_drink %in% c(5,6) ~ 4,  # Two to four times a month
      current_drink %in% c(1,2,3,4)~ 5, # Twice or more a week
      TRUE ~ NA_real_  # Everything else to NA
    )
  )

nhanes_2123_imputed <- nhanes_2123_imputed %>%
                subset(select=-c(ever_drank,current_drink))

nhanes_2123_imputed$drinking_habits <- as.factor(nhanes_2123_imputed$drinking_habits)

# Merge
nhanes_imputed <- rbind(nhanes_1720_imputed,nhanes_2123_imputed)

nhanes_imputed$race <- as.factor(nhanes_imputed$race)
nhanes_imputed$sex_at_birth <- as.factor(nhanes_imputed$sex_at_birth)
nhanes_imputed$nativity <- as.factor(nhanes_imputed$nativity)
nhanes_imputed$marital_status <- as.factor(nhanes_imputed$marital_status)
nhanes_imputed$education <- as.factor(nhanes_imputed$education)
nhanes_imputed$health_insurance <- as.factor(nhanes_imputed$health_insurance)
nhanes_imputed$overall_health <- as.factor(nhanes_imputed$overall_health)
nhanes_imputed$drinking_habits <- as.factor(nhanes_imputed$drinking_habits)
nhanes_imputed$smoking_habits <- as.factor(nhanes_imputed$smoking_habits)

nhanes_1720_imputed$weight <- nhanes_1720_imputed$weight * 2/3
nhanes_2123_imputed$weight <- nhanes_2123_imputed$weight * 1/3

nhanes_df_final <- rbind(as.data.frame(nhanes_1720_imputed),as.data.frame(nhanes_2123_imputed))

nhanes_df_final <- nhanes_df_final[nhanes_df_final$age >= 18 & nhanes_df_final$age < 80,]

nhanes_weights <- nhanes_df_final %>%
                    subset(select=c(id,
                             weight))

nhanes_df_final <- nhanes_df_final %>%
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

nhanes_df_final$data_label <- "NHANES"
nhanes_df_final$participation <- 0

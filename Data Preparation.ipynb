recodeVars2 <- function(df,df_label,num_recode){
    
    # 1. Straight Recode
    
    if(df_label == 1){ #If AoU data
        df <- df %>%
                mutate(highest_grade=recode(highest_grade,"Highest Grade: One Through Four"="Less than 9th grade", 
                                            "Highest Grade: Five Through Eight"="Less than 9th grade",
                                            "Highest Grade: Never Attended"="Less than 9th grade",
                                            "Highest Grade: Nine Through Eleven"="9-11th grade",
                                            "Highest Grade: Twelve Or GED"="High school graduate/GED or equivalent",
                                            "Highest Grade: College One to Three"="Some college or AA degree",
                                            "Highest Grade: College Graduate"="College graduate or above",
                                            "Highest Grade: Advanced Degree"="College graduate or above",
                                            "PMI: Prefer Not To Answer"="NA",
                                            "PMI: Skip"="NA"),
                      marital_status=recode(marital_status,"Current Marital Status: Married"="Married/Living with Partner",
                                            "Current Marital Status: Living With Partner"="Married/Living with Partner",
                                            "Current Marital Status: Widowed"="Widowed/Divorced/Separated",
                                            "Current Marital Status: Divorced"="Widowed/Divorced/Separated",
                                            "Current Marital Status: Separated"="Widowed/Divorced/Separated",
                                            "Current Marital Status: Never Married"="Never Married",
                                            "PMI: Prefer Not To Answer"="NA",
                                            "PMI: Skip"="NA"),
                      health_insurance=recode(health_insurance,"Health Insurance: Yes"="Yes",
                                              "Health Insurance: No"="No",
                                              "PMI: Dont Know"="NA",
                                              "PMI: Skip"="NA",
                                              "PMI: Prefer Not To Answer"="NA"),
                      alcohol_use_freq=recode(alcohol_use_freq,"Drink Frequency Past Year: Never"="Never",
                                              "Drink Frequency Past Year: Monthly Or Less"="Monthly or less",
                                              "Drink Frequency Past Year: 2 to 4 Per Month"="Two to four times a month",
                                              "Drink Frequency Past Year: 2 to 3 Per Week"="Twice or more a week",
                                              "Drink Frequency Past Year: 4 or More Per Week"="Twice or more a week",
                                              "PMI: Dont Know"="NA",
                                              "PMI: Skip"="NA",
                                              "PMI: Prefer Not To Answer"="NA"),
                      alcohol_use=recode(alcohol_use,"Alcohol Participant: No"="No",
                                         "Alcohol Participant: Yes"="Yes",
                                         "PMI: Prefer Not To Answer"="NA",
                                         "PMI: Skip"="NA"),
                      ever_smoked=recode(ever_smoked,"100 Cigs Lifetime: No"="No",
                                         "100 Cigs Lifetime: Yes"="Yes",
                                         "PMI: Dont Know"="NA",
                                         "PMI: Skip"="NA",
                                         "PMI: Prefer Not To Answer"="NA"),
                     current_smoke=recode(current_smoke,"Smoke Frequency: Not At All"="Not current smoker",
                                          "Smoke Frequency: Some Days"="Sometimes",
                                          "Smoke Frequency: Every Day"="Daily",
                                          "PMI: Dont Know"="NA",
                                          "PMI: Skip"="NA",
                                          "PMI: Prefer Not To Answer"="NA"),
                     birthplace=recode(birthplace,"Birthplace: USA"="Born in USA",
                                                 "PMI: Other"="Other",
                                                 "PMI: Dont Know"="NA",
                                                 "PMI: Skip"="NA",
                                                 "PMI: Prefer Not To Answer"="NA"),
                     race=recode(race,"Middle Eastern or North African"="Other/Multiracial",
                                      "More than one population"="Other/Multiracial",
                                      "Native Hawaiian or Other Pacific Islander"="Other/Multiracial")
                      )
        for(i in 1:nrow(df)){
            if(df$ethnicity[i] == "Hispanic or Latino"){
                df$race[i] <- "Hispanic or Latino"
            }
        }
        
        df <- df %>%
                subset(select=-c(ethnicity))
        
    }else if(df_label==0){ #If NHANES data
        df <- df %>%
                subset(select=-c(bmi))
        df <- df %>%
                mutate(highest_grade=recode(highest_grade, "1"="Less than 9th grade", 
                                             "2"="9-11th grade",
                                             "3"="High school graduate/GED or equivalent",
                                             "4"="Some college or AA degree",
                                             "5"="College graduate or above",
                                             "7"="NA",
                                             "9"="NA"),
                      marital_status=recode(marital_status, "1"="Married/Living with Partner",
                                             "2"="Widowed/Divorced/Separated",
                                             "3"="Never Married",
                                             "77"="NA",
                                             "99"="NA"),
                      health_insurance=recode(health_insurance, "1"="Yes",
                                               "2"="No",
                                               "7"="NA",
                                               "9"="NA"),
                      alcohol_use=recode(alcohol_use, "2"="No",
                                          "1"="Yes",
                                          "7"="NA",
                                          "9"="NA"),
                      alcohol_use_freq=recode(alcohol_use_freq,"0"="Never",
                                              "7"="Monthly or less",
                                              "8"="Monthly or less",
                                              "9"="Monthly or less",
                                              "10"="Monthly or less",
                                              "6"="Two to four times a month",
                                              "5"="Two to four times a month",
                                              "1"="Twice or more a week",
                                              "2"="Twice or more a week",
                                              "3"="Twice or more a week",
                                              "4"="Twice or more a week",
                                              "77"="NA",
                                              "99"="NA"),
                      ever_smoked=recode(ever_smoked, "2"="No",
                                          "1"="Yes",
                                          "7"="NA",
                                          "9"="NA"),
                      current_smoke=recode(current_smoke,"3"="Not current smoker",
                                          "2"="Sometimes",
                                          "1"="Daily",
                                          "7"="NA",
                                          "9"="NA"),
                       race=recode(race,"3"="White",
                                   "4"="Black or African American",
                                   "6"="Asian",
                                   "1"="Hispanic or Latino",
                                   "2"="Hispanic or Latino",
                                   "7"="Other/Multiracial"),
                       gender=recode(gender,"1"="Male",
                                     "2"="Female",
                                     "7"="NA",
                                     "9"="NA"),
                       birthplace=recode(birthplace,"1"="Born in USA",
                                             "2"="Other",
                                             "77"="NA",
                                             "99"="NA")
                       )
    }
    
    df <- df[df$race == "White" | df$race == "Black or African American" | df$race == "Asian" | df$race == "Hispanic or Latino" | df$race == "Other/Multiracial",]    

    df[df=="NA"] <- NA
    
    alcohol_temp <- c()
    smoking_temp <- c()
    
    # 2. Recode alcohol and smoking
    
    for(i in 1:nrow(df)){ #Harmonize alcohol and somking variables
        if(is.na(df$alcohol_use[i]) == TRUE){
            alcohol_temp[i] <- NA
        }else if(df$alcohol_use[i] == "No"){
            alcohol_temp[i] <- "Never drank"
        }else if(df$alcohol_use[i] == "Yes"){
            if(is.na(df$alcohol_use_freq[i]) == TRUE){
                alcohol_temp[i] <- NA
            }else if(df$alcohol_use_freq[i] == "Never"){
                alcohol_temp[i] <- "Less than yearly"
            }else if(df$alcohol_use_freq[i] == "NA"){
                alcohol_temp[i] <- NA
            }else if(df$alcohol_use_freq[i] == "Monthly or less"){
                alcohol_temp[i] <- "Monthly or less"
            }else if(df$alcohol_use_freq[i] == "Two to four times a month"){
                alcohol_temp[i] <- "Two to four times a month"
            }else if(df$alcohol_use_freq[i] == "Twice or more a week"){
                alcohol_temp[i] <- "Twice or more a week"
            }
        }else{
            alcohol_temp[i] <- NA
        }
        
        if(is.na(df$ever_smoked[i]) == TRUE){
            smoking_temp[i] <- NA
        }else if(df$ever_smoked[i] == "No"){
            smoking_temp[i] <- "Never smoked"
        }else if(df$ever_smoked[i] == "Yes"){
            if(is.na(df$current_smoke[i]) == TRUE){
                smoking_temp[i] <- NA
            }else if(df$current_smoke[i] == "NA"){
                smoking_temp[i] <- NA
            }else if(df$current_smoke[i] == "Not current smoker"){
                smoking_temp[i] <- "Past smoker"
            }else if(df$current_smoke[i] == "Sometimes"){
                smoking_temp[i] <- "Sometimes smokes"
            }else if(df$current_smoke[i] == "Daily"){
                smoking_temp[i] <- "Daily smoker"
            }else{
                smoking_temp[i] <- NA
            }
        }else{
            smoking_temp[i] <- NA
        }
            
    }
            
    
    df$alcohol_final <- alcohol_temp
    df$smoking_final <- smoking_temp
    
            
    # 3. Recode to numbers if selected
            
    if(num_recode == 1){
        df <- df %>%
                mutate(race=recode(race,"White"=1,
                                      "Black or African American"=2,
                                      "Hispanic or Latino"=3,
                                      "Asian"=4,
                                      "Other/Multiracial"=5),
                      gender=recode(gender, "Male"=1,
                                       "Female"=2),
                      highest_grade=recode(highest_grade, "Less than 9th grade" = 1, 
                                             "9-11th grade"=2,
                                             "High school graduate/GED or equivalent"=3,
                                             "Some college or AA degree"=4,
                                             "College graduate or above"=5),
                      marital_status=recode(marital_status, "Married/Living with Partner"=1,
                                             "Widowed/Divorced/Separated"=2,
                                             "Never Married"=3),
                      health_insurance=recode(health_insurance, "Yes"=2,
                                               "No"=1),
                      alcohol_final=recode(alcohol_final, "Never drank"=1,
                                            "Less than yearly"=2,
                                            "Monthly or less"=3,
                                            "Two to four times a month"=4,
                                            "Twice or more a week"=5),
                      smoking_final=recode(smoking_final,"Never smoked"=1,
                                              "Past smoker"=2,
                                              "Sometimes smokes"=3,
                                              "Daily smoker"=4),
                      birthplace=recode(birthplace,"Born in USA"=1,
                                                "Other"=2)
                      )
    }
        
    # 4. Convert to factors, and drop levels 
            
    df$race <- as.factor(df$race)
    df$gender <- as.factor(df$gender)
    df$birthplace <- as.factor(df$birthplace)
    df$highest_grade <- as.factor(df$highest_grade)
    df$marital_status <- as.factor(df$marital_status)
    df$health_insurance <- as.factor(df$health_insurance)
    df$smoking_final <- as.factor(df$smoking_final)
    df$alcohol_final <- as.factor(df$alcohol_final)
            
    df <- df %>%
            subset(select = -c(alcohol_use, alcohol_use_freq, ever_smoked, current_smoke,height_cm,weight_kg))
            
    df <- droplevels(df)        
    
    return(df)
    
 
} # Recode function
colnames(temp_aou)[1] <- "id"
colnames(temp_aou)[8:26] <- c("alcohol_use","alcohol_use_freq","cigar_use",
                              "highest_grade","esmoke_use","employment_status",
                              "home_ownership","income","health_insurance","household_size","marital_status",
                              "smokeless_use","ever_smoked","current_smoke","birthplace",
                              "height_cm","weight_kg")
# head(temp_aou)
temp_aou_selected <- temp_aou %>% #Select data needed & reorder
                        select(id,age,race,ethnicity,gender,highest_grade,marital_status,
                           weight_kg,height_cm,health_insurance,alcohol_use,
                           alcohol_use_freq,ever_smoked,current_smoke,birthplace)
temp_aou_recoded <- recodeVars2(temp_aou_selected,df_label=1,num_recode=1) #Recode variables (harmonize)
temp_aou_recoded <- left_join(temp_aou_recoded,oh_df_final,by="id") #Combine vars and overall health
temp_aou_recoded$data_label <- "All of Us" # Add data label
temp_aou_recoded$participation <- 1 # Add data label
temp_aou_recoded <- temp_aou_recoded[temp_aou_recoded$age < 80 & temp_aou_recoded$age >= 18,] #Save age 18-79
# head(temp_aou_recoded)

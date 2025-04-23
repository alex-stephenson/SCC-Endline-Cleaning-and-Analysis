rm(list = ls())

library(cleaningtools)
library(tidyverse)
library(plyr)
library(readxl)
library(analysistools)
library(addindicators)
library(impactR4PHU)

questions <- read_excel("inputs/IRF_ENDLINE_TOOL_FEB2025.xlsx", sheet = "survey")
choices <- read_excel("inputs/IRF_ENDLINE_TOOL_FEB2025.xlsx", sheet = "choices")

raw_data <- read_excel("inputs/as_raw_data.xlsx") %>% 
  select(-savings)
deletion <- read_excel("inputs/as_deletion_log.xlsx")
combined_clogs <- read_excel("combined clogs/corrected_combined cleaning clogs.xlsx") %>% 
  filter(! uuid %in% deletion$uuid)
#clean_load <- read_excel("output/final_output/final_output_for IRF_ENDLINE ANALYSIS.xlsx", sheet = "cleaned_data")

## clog review complete
clog_review <- cleaningtools::review_cleaning_log(
  raw_dataset = raw_data,
  raw_data_uuid_column = "_uuid",
  cleaning_log = combined_clogs,
  cleaning_log_change_type_column = "change_type",
  change_response_value = "change_response",
  cleaning_log_question_column = "question",
  cleaning_log_uuid_column = "uuid",
  cleaning_log_new_value_column = "new_value")

my_clean_data <- create_clean_data(raw_dataset = raw_data,
                                   raw_data_uuid_column = "_uuid",
                                   cleaning_log = combined_clogs, 
                                   cleaning_log_uuid_column = "uuid",
                                   cleaning_log_question_column = "question",
                                   cleaning_log_new_value_column = "new_value",
                                   cleaning_log_change_type_column = "change_type")



my_clean_data_deleted <- my_clean_data %>% 
  filter(! `_uuid` %in% deletion$uuid)



first_review <- review_cleaning(
  raw_dataset = raw_data,
  raw_dataset_uuid_column = "_uuid",
  clean_dataset = my_clean_data_deleted,
  clean_dataset_uuid_column = "_uuid",
  cleaning_log = combined_clogs,
  cleaning_log_uuid_column = "uuid",
  cleaning_log_change_type_column = "change_type",
  cleaning_log_question_column = "question",
  cleaning_log_new_value_column = "new_value",
  cleaning_log_old_value_column = "old_value",
  cleaning_log_added_survey_value = "added_survey",
  cleaning_log_no_change_value = c("no_action", "no_change"),
  deletion_log = deletion,
  deletion_log_uuid_column = "uuid",
  check_for_deletion_log = T
)


## validate these issues
first_review %>%
  filter(comment == "Changes were not applied") %>% 
  mutate(cl.new_value = round(as.numeric(cl.new_value)),
         df.new_value = round(as.numeric(df.new_value))) %>% 
  filter(cl.new_value != df.new_value)

## validate these issues
first_review %>%
 # filter(comment == "Entry missing in cleaning log") %>% 
  mutate(df.new_value = round(as.numeric(df.new_value)),
         df.old_value = round(as.numeric(df.old_value))) %>% 
  filter(df.new_value != df.new_value)

first_review %>%
  filter(comment != "Entry missing in cleaning log") %>% 
  mutate(df.new_value = round(as.numeric(df.new_value)),
         df.old_value = round(as.numeric(df.old_value))) %>% 
  filter(df.new_value != df.new_value)


first_review %>%
  filter(comment != "No action with different value in new value column.") %>% 
  mutate(df.new_value = round(as.numeric(df.new_value)),
         df.old_value = round(as.numeric(df.old_value))) %>% 
  filter(df.new_value != df.new_value)

first_review %>%
  filter(comment == "No action with different value in new value column.") %>% 
  filter(df.change_type == "no_action") %>% 
  mutate(cl.new_value = round(as.numeric(cl.new_value)),
         cl.old_value = round(as.numeric(cl.old_value))) %>% 
  filter(cl.old_value != cl.new_value) 





#Adding the composite indicators

my_data_with_indicators <- my_clean_data_deleted %>%
  add_fcs(
    cutoffs = "alternative",
    fsl_fcs_cereal = "FCSStapCer7",
    fsl_fcs_legumes = "FCSPr7",
    fsl_fcs_veg = "FCSVeg7",
    fsl_fcs_fruit = "FCSFruit7",
    fsl_fcs_meat = "FCSMeatFish7",
    fsl_fcs_dairy = "FCSDairy7",
    fsl_fcs_sugar = "FCSSugar7",
    fsl_fcs_oil = "FCSFat7"
  ) %>% 
  add_rcsi(
    fsl_rcsi_lessquality = "rCSILessQlty",
    fsl_rcsi_borrow  = "rCSIBorrow",
    fsl_rcsi_mealsize = "rCSIMealSize",
    fsl_rcsi_mealadult = "rCSIMealAdult",
    fsl_rcsi_mealnb = "rCSIMealNb"
  )%>% 
  
  # add_hdds(
  #   fsl_hdds_cereals = "DDSStapCer7",
  #   fsl_hdds_tubers = "DDSRoots7",
  #   fsl_hdds_veg = "DDSVeg7",
  #   fsl_hdds_fruit = "DDSFruit7",
  #   fsl_hdds_meat = "DDSMeat7",
  #   fsl_hdds_eggs = "DDSEggs7",
  #   fsl_hdds_fish = "DDSFish7",
  #   fsl_hdds_legumes = "DDSPr7",
  #   fsl_hdds_dairy = "DDSDairy7",
  #   fsl_hdds_oil = "DDSFat7",
  #   fsl_hdds_sugar = "DDSSugar7",
  #   fsl_hdds_condiments = "DDSCond7",
  #   yes_val = "yes",
  #   no_val = "no"
  # ) 
  
  add_hhs(
    fsl_hhs_nofoodhh = "HHSNoFood",
    fsl_hhs_nofoodhh_freq = "HHSNoFood_FR",
    fsl_hhs_sleephungry = "HHSBedHung",
    fsl_hhs_sleephungry_freq = "HHSBedHung_FR",
    fsl_hhs_alldaynight = "HHSNotEat",
    fsl_hhs_alldaynight_freq = "HHSNotEat_FR",
    yes_answer = "yes",
    no_answer = "no",
    rarely_answer = "rarely",
    sometimes_answer = "sometimes",
    often_answer = "often"
  )


my_data_with_indicators <- my_data_with_indicators %>%
  add_lcsi(
    fsl_lcsi_stress1 = "LhCSIDomAsset",
    fsl_lcsi_stress2 = "LhCSICrdtFood",
    fsl_lcsi_stress3 = "LhCSISavingUrban",
    fsl_lcsi_stress4 = "LhCSIBorrowCash",
    fsl_lcsi_crisis1 = "LhCSIProdAsset",
    fsl_lcsi_crisis2 = "LhCSIHealthEdu",
    fsl_lcsi_crisis3 = "LhCSIOutSchool",
    fsl_lcsi_emergency1 = "LhCSIHouseLand",
    fsl_lcsi_emergency2 = "LhCSIBegged",
    fsl_lcsi_emergency3 = "LhCSIMoveUrban",
    yes_val = "yes",
    no_val = "no",
    exhausted_val = "no_exhausted",
    not_applicable_val = "n/a"
  )


# writexl::write_xlsx(cleaned_data,paste("C:\\Users\\Mercy Kalondu\\OneDrive - ACTED\\Bureau\\old laptop files\\REACH WORK\\SOM 2406 Banadir baseline/final_cleaned_data3.xlsx"))

cleaned_data <- my_data_with_indicators %>% mutate(ExpFood_spent1 = ifelse(ExpFood >0, ExpFood, 0),
                                                   ExpFood_Debt_spent1 = ifelse(ExpFood_Debt >0, ExpFood_Debt, 0),
                                                   ExpNFDebt_spent1 = ifelse(ExpNFDebt >0, ExpNFDebt, 0),
                                                   ExpNFMed_spent1 = ifelse(ExpNFMed >0, ExpNFMed, 0),
                                                   ExpNFRent_spent1 = ifelse(ExpNFRent >0, ExpNFRent, 0),
                                                   ExpNFWat_spent1 = ifelse(ExpNFWat >0, ExpNFWat, 0),
                                                   ExpNFCstrc_spent1 = ifelse(ExpNFCstrc >0, ExpNFCstrc, 0),
                                                   ExpNFCloth_spent1 = ifelse(ExpNFCloth >0, ExpNFCloth, 0),
                                                   ExpNFFuel_spent1 = ifelse(ExpNFFuel >0, ExpNFFuel, 0),
                                                   ExpNFEdu_spent1 = ifelse(ExpNFEdu >0, ExpNFEdu, 0),
                                                   ExpNFKhat_spent1 = ifelse(ExpNFKhat >0, ExpNFKhat, 0),
                                                   ExpNFOther_spent1 = ifelse(ExpNFOther >0, ExpNFOther, 0)
                                                   
) %>%
  relocate(ExpFood_spent1, .after = ExpFood) %>% relocate(ExpFood_Debt_spent1, .after = ExpFood_Debt) %>%
  relocate(ExpNFDebt_spent1, .after = ExpNFDebt) %>% relocate(ExpNFMed_spent1, .after = ExpNFMed) %>%
  relocate(ExpNFRent_spent1, .after = ExpNFRent) %>% relocate(ExpNFCstrc_spent1, .after = ExpNFCstrc) %>%
  relocate(ExpNFCloth_spent1, .after = ExpNFCloth) %>% relocate(ExpNFFuel_spent1, .after = ExpNFFuel) %>%
  relocate(ExpNFWat_spent1, .after = ExpNFWat) %>% relocate(ExpNFEdu_spent1, .after = ExpNFEdu) %>%
  relocate(ExpNFKhat_spent1, .after = ExpNFKhat) %>% relocate(ExpNFOther_spent1, .after = ExpNFOther)
# calculating the MEB values 

cleaned_data <- cleaned_data %>% 
  mutate(MEB = ifelse(region == 'banaadir', 229,
                      
                      NA))


cleaned_data <- cleaned_data %>% 
  mutate(
    TotalHHEXP1 = as.numeric(ExpFood) + as.numeric(ExpFood_Debt) + as.numeric(ExpNFDebt) + 
      as.numeric(ExpNFMed) + as.numeric(ExpNFWat) + as.numeric(ExpNFCstrc) + 
      as.numeric(ExpNFCloth) + as.numeric(ExpNFFuel) + as.numeric(ExpNFEdu) + 
      as.numeric(ExpNFKhat) + as.numeric(ExpNFOther) + as.numeric(ExpNFRent),
    
    ExpFoodProportion1 = ifelse(!is.na(ExpFood), as.numeric(ExpFood) / TotalHHEXP1 * 100, 0),
    ExpNFMedProportion1 = ifelse(!is.na(ExpNFMed), as.numeric(ExpNFMed) / TotalHHEXP1 * 100, 0),
    ExpNFCstrcProportion1 = ifelse(!is.na(ExpNFCstrc), as.numeric(ExpNFCstrc) / TotalHHEXP1 * 100, 0),
    ExpNFClothProportion1 = ifelse(!is.na(ExpNFCloth), as.numeric(ExpNFCloth) / TotalHHEXP1 * 100, 0),
    ExpNFWatProportion1 = ifelse(!is.na(ExpNFWat), as.numeric(ExpNFWat) / TotalHHEXP1 * 100, 0),
    ExpFood_DebtProportion1 = ifelse(!is.na(ExpFood_Debt), as.numeric(ExpFood_Debt) / TotalHHEXP1 * 100, 0),
    ExpNFDebtProportion1 = ifelse(!is.na(ExpNFDebt), as.numeric(ExpNFDebt) / TotalHHEXP1 * 100, 0),
    
    TotalDebt = rowSums(dplyr::select(., ExpFood_Debt, ExpNFDebt), na.rm = TRUE),
    ExpTotalDebtProportion1 = ifelse(TotalDebt == 0, 0, TotalDebt / TotalHHEXP1 * 100),
    
    ExpNFFuelProportion1 = ifelse(!is.na(ExpNFFuel), as.numeric(ExpNFFuel) / TotalHHEXP1 * 100, 0),
    ExpNFEduProportion1 = ifelse(!is.na(ExpNFEdu), as.numeric(ExpNFEdu) / TotalHHEXP1 * 100, 0),
    ExpNFRentProportion1 = ifelse(!is.na(ExpNFRent), as.numeric(ExpNFRent) / TotalHHEXP1 * 100, 0),
    ExpNFKhatProportion1 = ifelse(!is.na(ExpNFKhat), as.numeric(ExpNFKhat) / TotalHHEXP1 * 100, 0),
    ExpNFOtherProportion1 = ifelse(!is.na(ExpNFOther), as.numeric(ExpNFOther) / TotalHHEXP1 * 100, 0),
    
    NFOther_NFKhat = rowSums(dplyr::select(., ExpNFOther, ExpNFKhat), na.rm = TRUE),
    ExpNFOther_NFKhatProportion1 = ifelse(NFOther_NFKhat == 0, 0, NFOther_NFKhat / TotalHHEXP1 * 100),
    
    savings_amnt = ifelse(Savings > 0, Savings, 0),
    savings_Have = ifelse(Savings > 0, 'yes', ifelse(Savings == 0, 'no', '')),
    debt_amnt_Have = ifelse(debt_amnt > 0, debt_amnt, 0),
    debt1 = ifelse(debt_amnt > 0, 'yes', ifelse(debt_amnt == 0, 'no', '')),
    
    average_monthly_income_pp1 = ifelse(!is.na(average_monthly_income), 
                                        average_monthly_income / HH_size, NA),
    Big_size_HH = ifelse(HH_size > 6, 'yes', 'no'),
    
    low_income = ifelse(!is.na(average_monthly_income) & average_monthly_income < 130, 'yes',
                        ifelse(!is.na(average_monthly_income) & average_monthly_income >= 130, 'no', '')),
    
    HHH_older = ifelse(!is.na(HHH_age_final) & HHH_age_final >= 55, 'yes', 
                       ifelse(!is.na(HHH_age_final) & HHH_age_final < 55, 'no', '')),
    
    HHH_age_final = ifelse(resp_HHH == 'yes', resp_age, 
                           ifelse(resp_HHH == 'no', HHH_age, NA)),
    HHH_gender_final = ifelse(resp_HHH == 'yes', resp_gender, 
                              ifelse(resp_HHH == 'no', HHH_gender, NA)),
    
    HHH_age_gender_category = ifelse(HHH_age_final < 50 & HHH_gender_final == 'male', '18-49 male',
                                     ifelse(HHH_age_final < 50 & HHH_gender_final == 'female', '18-49 female',
                                            ifelse(HHH_age_final >= 50 & HHH_age_final < 70 & HHH_gender_final == 'male', '50-69 male',
                                                   ifelse(HHH_age_final >= 50 & HHH_age_final < 70 & HHH_gender_final == 'female', '50-69 female',
                                                          ifelse(HHH_age_final >= 70 & HHH_gender_final == 'male', '70+ male', '70+ female')))))
  ) %>%
  relocate(savings_Have, .after = Savings) %>% 
  relocate(debt_amnt_Have, .after = debt_amnt) %>%
  relocate(Big_size_HH, .after = HH_size) %>% 
  relocate(low_income, .after = average_monthly_income)
#%>% relocate(new_idp, .after = HH_arrived) %>%
# relocate(HHH_older, .after = HHH_age_final)

cleaned_data <- cleaned_data %>%
  mutate(ecmen1 = ifelse(TotalHHEXP-(ExpNFKhat + ExpNFOther) > MEB, 1, 0))
#######Computing CARI######
# Current Status using FCS and RCSI

cleaned_data <- cleaned_data %>%
  mutate(Current_Status = case_when(
    fsl_fcs_cat == "Acceptable" & fsl_rcsi_score < 4 ~ 1,     
    fsl_fcs_cat == "Acceptable" & fsl_rcsi_score >= 4 ~ 2,      
    fsl_fcs_cat == "Borderline" ~ 3,                             
    fsl_fcs_cat == "Poor" ~ 4                                    
  ))

# Assign value labels for Current Status
Current_Status_labels <- c(
  "1" = "Food Secure",
  "2" = "Marginally Food Secure",
  "3" = "Moderately Food Insecure",
  "4" = "Severely Food Insecure"
)

# Convert Current_Status to a factor with labels
cleaned_data$Current_Status <- factor(cleaned_data$Current_Status, 
                                      levels = c(1, 2, 3, 4), 
                                      labels = Current_Status_labels)

# Create new column current_status_numeric with 1, 2, 3, 4 based on Current_Status factor
cleaned_data <- cleaned_data %>%
  mutate(current_status_numeric = as.numeric(Current_Status))

# Economic Vulnerability (ECMEN)
# ECMEN Calculation
cleaned_data <- cleaned_data %>% 
  mutate(SMEB = ifelse(region == 'banaadir', 151,
                       
                       NA))
cleaned_data <- cleaned_data %>%
  mutate(
    ECMEN = case_when(
      TotalHHEXP1-(ExpNFKhat + ExpNFOther) > MEB ~ 1,  
      TotalHHEXP1-(ExpNFKhat + ExpNFOther) >= SMEB & TotalHHEXP1-(ExpNFKhat + ExpNFOther) <= MEB ~ 3,  
      TotalHHEXP1-(ExpNFKhat + ExpNFOther) <= SMEB ~ 4  
    )
  )


#Classify Livelihood Coping Strategies (LCS)
cleaned_data <- cleaned_data %>%
  mutate(LCS_class = case_when(
    fsl_lcsi_cat == "None" ~ 1,            
    fsl_lcsi_cat == "Stress" ~ 2,             
    fsl_lcsi_cat == "Crisis" ~ 3,             
    fsl_lcsi_cat == "Emergency" ~ 4         
  ))

# Combine Coping Capacity Components
cleaned_data <- cleaned_data %>%
  mutate(Coping_Capacity = pmax(ECMEN, LCS_class, na.rm = TRUE))

# Assign value labels for Coping Capacity
Coping_Capacity_labels <- c(
  "1" = "Food Secure",
  "2" = "Marginally Food Secure",
  "3" = "Moderately Food Insecure",
  "4" = "Severely Food Insecure"
)

# Convert Coping_Capacity to a factor with labels
cleaned_data$Coping_Capacity <- factor(cleaned_data$Coping_Capacity, 
                                       levels = c(1, 2, 3, 4), 
                                       labels = Coping_Capacity_labels)

# Create new column coping_capacity_numeric with 1, 2, 3, 4 based on Coping_Capacity factor
cleaned_data <- cleaned_data %>%
  mutate(coping_capacity_numeric = as.numeric(Coping_Capacity))

# Calculate Mean_coping_capacity_ECMEN
# Convert Coping_Capacity and ECMEN to numeric values for calculation

cleaned_data <- cleaned_data %>%
  mutate(
    ECMEN_numeric = as.numeric(as.character(ECMEN)),
    Mean_coping_capacity_ECMEN = rowMeans(cbind(coping_capacity_numeric, ECMEN_numeric), na.rm = TRUE)
  )

# Calculate CARI (Comprehensive Approach to Resilience Index)
# Ensure both columns are numeric
cleaned_data <- cleaned_data %>%
  mutate(
    Mean_coping_capacity_ECMEN_numeric = as.numeric(as.character(Mean_coping_capacity_ECMEN)), 
    CARI_unrounded_ECMEN = rowMeans(cbind(current_status_numeric, Mean_coping_capacity_ECMEN_numeric), na.rm = TRUE),
    CARI_ECMEN = round(CARI_unrounded_ECMEN)  
  )

# Assign value labels for CARI_ECMEN
CARI_ECMEN_labels <- c(
  "1" = "Food Secure",
  "2" = "Marginally Food Secure",
  "3" = "Moderately Food Insecure",
  "4" = "Severely Food Insecure"
)

# Optional: Convert CARI_ECMEN to a factor with labels
cleaned_data$CARI_ECMEN <- factor(cleaned_data$CARI_ECMEN, 
                                  levels = c(1, 2, 3, 4), 
                                  labels = CARI_ECMEN_labels)


sampling_frame <- read_csv("inputs/weights.csv")

cleaned_data_weighted <- cleaned_data %>%
  add_weights(sampling_frame, 
              strata_column_dataset = "district",
              strata_column_sample = "strata.names",
              population_column = "population")



# writing the raw and cleaned data
data<-list(questions =questions,choices=choices,raw_data=raw_data,cleaned_data=cleaned_data_weighted)
writexl::write_xlsx(data,paste("output/clean_and_raw.xlsx"))


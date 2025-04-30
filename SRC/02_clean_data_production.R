rm(list = ls())

library(cleaningtools)
library(tidyverse)
library(readxl)
library(analysistools)
library(addindicators)
library(impactR4PHU)

questions <- read_excel("inputs/IRF_ENDLINE_TOOL_FEB2025.xlsx", sheet = "survey")
choices <- read_excel("inputs/IRF_ENDLINE_TOOL_FEB2025.xlsx", sheet = "choices")

raw_data <- ImpactFunctions::get_kobo_data("aJ8Dxysm2GG3EgJDwc88Ap", un = "abdirahmanaia")

deletion <- read_excel("inputs/as_deletion_log.xlsx")

dir_path <- "combined clogs"
all_files <- list.files(
  path = dir_path,
  recursive = TRUE,
  full.names = TRUE
)

# Function to read and convert all columns to character
read_and_clean <- function(file, sheet) {
  read_excel(file, sheet = sheet) %>%
    mutate(across(everything(), as.character)) %>% 
    mutate(file_path = file)
}

# Read and combine all files into a single dataframe
combined_clogs <- map_dfr(all_files, sheet = 'cleaning_log', read_and_clean) %>% 
  filter(! uuid %in% deletion$uuid) %>% 
  filter(change_type != "remove_survey") %>% 
  distinct(uuid, question, old_value, new_value, .keep_all = T)

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

check <- first_review %>%
  dplyr::mutate(dplyr::across(c(df.new_value, cl.new_value, df.old_value, cl.old_value), as.numeric)) %>%
  dplyr::mutate(dplyr::across(c(df.new_value, cl.new_value, df.old_value, cl.old_value), round)) %>%
  filter(df.new_value != cl.new_value | df.old_value != cl.old_value) 




#Adding the composite indicators

## in MEBs

MEB_df <- read_excel("inputs/MEB_Values.xlsx") %>% 
  select(region, MEB, SMEB)

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
  ) %>% 
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
  ) %>%
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

expense_vars <- c(
  "ExpFood", "ExpFood_Debt", "ExpNFDebt", "ExpNFMed", "ExpNFRent",
  "ExpNFWat", "ExpNFCstrc", "ExpNFCloth", "ExpNFFuel", "ExpNFEdu",
  "ExpNFKhat", "ExpNFOther"
)


cleaned_data <- my_data_with_indicators %>%
  left_join(MEB_df) %>% 
  mutate(
    TotalHHEXP1 = rowSums(dplyr::across(all_of(expense_vars)), na.rm = TRUE),
    across(all_of(expense_vars),
           ~ if_else(!is.na(.), . / TotalHHEXP1 * 100, 0),
           .names = "{.col}Proportion1"),

    TotalDebt = rowSums(across(c("ExpFood_Debt", "ExpNFDebt")), na.rm = TRUE),
    ExpTotalDebtProportion1 = ifelse(TotalDebt == 0, 0, TotalDebt / TotalHHEXP1 * 100),
    
    NFOther_NFKhat = rowSums(across(c("ExpNFOther", "ExpNFKhat")), na.rm = TRUE),
    ExpNFOther_NFKhatProportion1 = ifelse(NFOther_NFKhat == 0, 0, NFOther_NFKhat / TotalHHEXP1 * 100),
    
    savings_amnt = ifelse(Savings > 0, Savings, 0),
    savings_Have = ifelse(Savings > 0, 'yes', ifelse(Savings == 0, 'no', '')),
    
    debt_amnt_Have = ifelse(debt_amnt > 0, debt_amnt, 0),
    debt1 = ifelse(debt_amnt > 0, 'yes', ifelse(debt_amnt == 0, 'no', '')),
    
    average_monthly_income_pp1 = ifelse(!is.na(average_monthly_income), 
                                        average_monthly_income / HH_size, NA),
    
    Big_size_HH = ifelse(HH_size > 6, 'yes', 'no'),
    
    low_income = case_when(
      !is.na(average_monthly_income) & average_monthly_income < 130  ~ 'yes',
      !is.na(average_monthly_income) & average_monthly_income >= 130 ~ 'no',
      TRUE ~ ''
    ),
    
    HHH_older = case_when(
      !is.na(HHH_age_final) & HHH_age_final >= 55 ~ 'yes',
      !is.na(HHH_age_final) & HHH_age_final < 55  ~ 'no',
      TRUE ~ ''
    ),

    HHH_age_final = case_when(
      resp_HHH == 'yes' ~ resp_age,
      resp_HHH == 'no'  ~ HHH_age,
      TRUE ~ NA_real_
    ),
    
    HHH_gender_final = case_when(
      resp_HHH == 'yes' ~ resp_gender,
      resp_HHH == 'no'  ~ HHH_gender,
      TRUE ~ NA_character_
    ),
    
    HHH_age_gender_category = case_when(
      HHH_age_final < 50 & HHH_gender_final == 'male'   ~ '18-49 male',
      HHH_age_final < 50 & HHH_gender_final == 'female' ~ '18-49 female',
      HHH_age_final >= 50 & HHH_age_final < 70 & HHH_gender_final == 'male'   ~ '50-69 male',
      HHH_age_final >= 50 & HHH_age_final < 70 & HHH_gender_final == 'female' ~ '50-69 female',
      HHH_age_final >= 70 & HHH_gender_final == 'male'   ~ '70+ male',
      HHH_age_final >= 70 & HHH_gender_final == 'female' ~ '70+ female'
    
    )) %>%
  relocate(savings_Have, .after = Savings) %>% 
  relocate(debt_amnt_Have, .after = debt_amnt) %>%
  relocate(Big_size_HH, .after = HH_size) %>% 
  relocate(low_income, .after = average_monthly_income)


#######Computing CARI######
# Current Status using FCS and RCSI

# Assign value labels for Current Status
Current_Status_labels <- c(
  "1" = "Food Secure",
  "2" = "Marginally Food Secure",
  "3" = "Moderately Food Insecure",
  "4" = "Severely Food Insecure"
)

cleaned_data <- cleaned_data %>%
  mutate(ecmen1 = ifelse(TotalHHEXP- NFOther_NFKhat > MEB, 1, 0),
         Current_Status = case_when(
           fsl_fcs_cat == "Acceptable" & fsl_rcsi_score < 4 ~ 1,
           fsl_fcs_cat == "Acceptable" & fsl_rcsi_score >= 4 ~ 2,
           fsl_fcs_cat == "Borderline" ~ 3,
           fsl_fcs_cat == "Poor" ~ 4),
         Current_Status = factor(Current_Status,
                                 levels = c(1, 2, 3, 4), 
                                 labels = Current_Status_labels),
         current_status_numeric = as.numeric(Current_Status))
     

# Economic Vulnerability (ECMEN)
cleaned_data <- cleaned_data %>% 
  mutate(ECMEN = case_when(
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
cleaned_data <-cleaned_data %>% 
  mutate(Coping_Capacity = factor(Coping_Capacity,
                                  levels = c(1, 2, 3, 4), 
                                  labels = Coping_Capacity_labels),
         coping_capacity_numeric = as.numeric(Coping_Capacity))

                  
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
writexl::write_xlsx(data,paste("inputs/clean_and_raw_check.xlsx"))

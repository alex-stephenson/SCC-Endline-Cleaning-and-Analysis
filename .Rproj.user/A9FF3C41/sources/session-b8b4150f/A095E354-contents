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

### add indicators

my_data_with_indicators <- my_clean_data_deleted %>%
  add_hhs(
    fsl_hhs_nofoodhh = "HHSNoFood",
    fsl_hhs_nofoodhh_freq= "HHSNoFood_FR",
    fsl_hhs_sleephungry = "HHSBedHung",
    fsl_hhs_sleephungry_freq = "HHSBedHung_FR",
    fsl_hhs_alldaynight  = "HHSNotEat",
    fsl_hhs_alldaynight_freq  = "HHSNotEat_FR",
    yes_answer = "yes",
    no_answer = "no",
    rarely_answer = "rarely",
    sometimes_answer = "sometimes",
    often_answer = "often"
  ) %>% 
  add_rcsi(
    fsl_rcsi_lessquality = "rCSILessQlty",
    fsl_rcsi_borrow  = "rCSIBorrow",
    fsl_rcsi_mealsize = "rCSIMealSize",
    fsl_rcsi_mealadult = "rCSIMealAdult",
    fsl_rcsi_mealnb = "rCSIMealNb"
  ) %>% add_fcs(
  cutoffs = "alternative",
  fsl_fcs_cereal = "FCSPr7",
  fsl_fcs_legumes = "FCSStapCer7",
  fsl_fcs_veg = "FCSVeg7",
  fsl_fcs_fruit = "FCSFruit7",
  fsl_fcs_meat = "FCSMeatFish7",
  fsl_fcs_dairy = "FCSDairy7",
  fsl_fcs_sugar = "FCSSugar7",
  fsl_fcs_oil = "FCSFat7")


cleaned_data <- my_data_with_indicators %>% mutate(ExpFood_spent = ifelse(ExpFood >0, ExpFood, 0),
                                                   ExpFood_Debt_spent = ifelse(ExpFood_Debt >0, ExpFood_Debt, 0),
                                                   ExpNFDebt_spent = ifelse(ExpNFDebt >0, ExpNFDebt, 0),
                                                   ExpNFMed_spent = ifelse(ExpNFMed >0, ExpNFMed, 0),
                                                   ExpNFWat_spent = ifelse(ExpNFWat >0, ExpNFWat, 0),
                                                   ExpNFCstrc_spent = ifelse(ExpNFCstrc >0, ExpNFCstrc, 0),
                                                   ExpNFCloth_spent = ifelse(ExpNFCloth >0, ExpNFCloth, 0),
                                                   ExpNFFuel_spent = ifelse(ExpNFFuel >0, ExpNFFuel, 0),
                                                   ExpNFEdu_spent = ifelse(ExpNFEdu >0, ExpNFEdu, 0),
                                                   ExpNFKhat_spent = ifelse(ExpNFKhat >0, ExpNFKhat, 0),
                                                   ExpNFOther_spent = ifelse(ExpNFOther >0, ExpNFOther, 0)
                                                   
) %>%
  relocate(ExpFood_spent, .after = ExpFood) %>% relocate(ExpFood_Debt_spent, .after = ExpFood_Debt) %>%
  relocate(ExpNFDebt_spent, .after = ExpNFDebt) %>% relocate(ExpNFMed_spent, .after = ExpNFMed) %>%
  relocate(ExpNFRent_spent, .after = ExpNFRent) %>% relocate(ExpNFCstrc_spent, .after = ExpNFCstrc) %>%
  relocate(ExpNFCloth_spent, .after = ExpNFCloth) %>% relocate(ExpNFFuel_spent, .after = ExpNFFuel) %>%
  relocate(ExpNFWat_spent, .after = ExpNFWat) %>% relocate(ExpNFEdu_spent, .after = ExpNFEdu) %>%
  relocate(ExpNFKhat_spent, .after = ExpNFKhat) %>% relocate(ExpNFOther_spent, .after = ExpNFOther)

cleaned_data <- cleaned_data %>%
  mutate(MEB = case_when(
    region == "lower_juba" ~ 113,
    region == "middle_shabelle" ~ 134,
    region == "sanaag" ~ 129,
    region == "putland" ~ 124,
    TRUE ~ NA_real_  # this handles all other cases
  ))



cleaned_data <- cleaned_data %>%
  mutate(
    TotalHHEXP = as.numeric(TotalHHEXP),
    ExpNFKhat = as.numeric(ExpNFKhat),
    ExpNFOther = as.numeric(ExpNFOther),
    ecmen = ifelse(TotalHHEXP - (ExpNFKhat + ExpNFOther) >= MEB, 1, 0)
  )


cleaned_data <- cleaned_data %>% mutate(
  TotalHHEXP = ExpFood + ExpFood_Debt + ExpNFDebt + ExpNFMed + ExpNFWat + ExpNFCstrc + ExpNFCloth +
    ExpNFFuel + ExpNFEdu + ExpNFKhat + ExpNFOther + ExpNFRent,
  ExpFoodProportion = ifelse(!is.na(ExpFood), ExpFood/TotalHHEXP*100,0),
  ExpNFMedProportion = ifelse(!is.na(ExpNFMed), ExpNFMed/TotalHHEXP*100, 0),
  ExpNFCstrcProportion = ifelse(!is.na(ExpNFCstrc), ExpNFCstrc/TotalHHEXP*100, 0),
  ExpNFClothProportion = ifelse(!is.na(ExpNFCloth), ExpNFCloth/TotalHHEXP*100, 0),
  ExpNFWatProportion = ifelse(!is.na(ExpNFWat), ExpNFWat/TotalHHEXP*100, 0),
  ExpFood_DebtProportion = ifelse(!is.na(ExpFood_Debt), ExpFood_Debt/TotalHHEXP*100, 0),
  ExpNFDebtProportion = ifelse(!is.na(ExpNFDebt), ExpNFDebt/TotalHHEXP*100, 0),
  TotalDebt = rowSums(dplyr::select(., ExpFood_Debt,ExpNFDebt), na.rm = TRUE),
  ExpTotalDebtProportion = ifelse(TotalDebt == '', 0, TotalDebt/TotalHHEXP*100),
  ExpNFFuelProportion = ifelse(!is.na(ExpNFFuel), ExpNFFuel/TotalHHEXP*100, 0),
  ExpNFEduProportion = ifelse(!is.na(ExpNFEdu), ExpNFEdu/TotalHHEXP*100, 0),
  ExpNFRentProportion = ifelse(!is.na(ExpNFRent), ExpNFRent/TotalHHEXP*100, 0),
  ExpNFKhatProportion = ifelse(!is.na(ExpNFKhat), ExpNFKhat/TotalHHEXP*100, 0),
  ExpNFOtherProportion = ifelse(!is.na(ExpNFOther), ExpNFOther/TotalHHEXP*100, 0),
  NFOther_NFKhat = rowSums(dplyr::select(., ExpNFOther,ExpNFKhat), na.rm = TRUE),
  ExpNFOther_NFKhatProportion = ifelse(NFOther_NFKhat == '', '', NFOther_NFKhat/TotalHHEXP*100),
  savings_amnt_Have = ifelse(savings_amnt >0, savings_amnt, 0),
  
  savings = ifelse(savings_amnt >0, 'yes', ifelse(savings_amnt == 0,'no', '')),
  debt_amnt_Have = ifelse(debt_amnt >0, debt_amnt, 0),
  debt = ifelse(debt_amnt >0, 'yes', ifelse(debt_amnt == 0, 'no', '')),
  average_monthly_income_pp = ifelse(!is.na(average_monthly_income), average_monthly_income/HH_size, ''),
  Big_size_HH = ifelse(HH_size >6, 'yes', 'no'),
  low_income = ifelse(!is.na(average_monthly_income) & average_monthly_income < 130, 'yes',
                      ifelse(!is.na(average_monthly_income) & average_monthly_income >= 130, 'no', '')),
  #new_idp = ifelse(!is.na(HH_arrived) & HH_arrived >= '2021-01-02', 'yes', ifelse(!is.na(HH_arrived) & HH_arrived < '2021-01-02', 'no', '')),
  HHH_older = ifelse(!is.na(HHH_age_final) & HHH_age_final >= 55, 'yes', ifelse(!is.na(HHH_age_final) & HHH_age_final < 55, 'no', '')),
  HHH_age_final = ifelse(resp_HHH == 'yes', resp_age, ifelse(resp_HHH == 'no', HHH_age, '')),
  HHH_gender_final = ifelse(resp_HHH == 'yes', resp_gender, ifelse(resp_HHH == 'no', HHH_gender, '')),
  HHH_age_gender_category = ifelse(HHH_age_final<50&HHH_gender_final=='male','18-49 male',
                                   ifelse(HHH_age_final<50&HHH_gender_final=='female','18-49 female',
                                          ifelse(HHH_age_final>=50&HHH_age_final<70&HHH_gender_final=='male','50-69 male',
                                                 ifelse(HHH_age_final>=50&HHH_age_final<70&HHH_gender_final=='female','50-69 female',
                                                        ifelse(HHH_age_final>=70&HHH_gender_final=='male','70+ male', '70+ female'))))),
  
  
  
) %>%
  relocate(rCSIName, .after = rCSI) %>%
  relocate(savings_amnt_Have, .after = savings_amnt) %>% relocate(debt_amnt_Have, .after = debt_amnt) %>%
  relocate(Big_size_HH, .after = HH_size) %>% relocate(low_income, .after = average_monthly_income) #%>% relocate(new_idp, .after = HH_arrived) %>%
# relocate(HHH_older, .after = HHH_age_final.y)

cleaned_data <- cleaned_data %>%
  mutate(ecmen = ifelse(TotalHHEXP-(ExpNFKhat + ExpNFOther) >= MEB, 1, 0))
cleaned_data$LhCSIDomAssetCalc <- ifelse(cleaned_data$LhCSIDomAsset == 'yes' | cleaned_data$LhCSIDomAsset == 'no_exhausted', 1, 0)
cleaned_data$LhCSICrdtFoodCalc <- ifelse(cleaned_data$LhCSICrdtFood == 'yes' | cleaned_data$LhCSICrdtFood == 'no_exhausted', 1, 0)
cleaned_data$LhCSISavingUrbanCalc <- ifelse(cleaned_data$LhCSISavingUrban == 'yes' | cleaned_data$LhCSISavingUrban == 'no_exhausted', 1, 0)
cleaned_data$LhCSISavingRuralCalc <- ifelse(cleaned_data$LhCSISavingRural == 'yes' | cleaned_data$LhCSISavingRural == 'no_exhausted', 1, 0)
cleaned_data$LhCSIBorrowCashCalc <- ifelse(cleaned_data$LhCSIBorrowCash == 'yes' | cleaned_data$LhCSIBorrowCash == 'no_exhausted', 1, 0)
cleaned_data$LhCSIProdAssetCalc <- ifelse(cleaned_data$LhCSIProdAsset == 'yes' | cleaned_data$LhCSIProdAsset == 'no_exhausted', 1, 0)
cleaned_data$LhCSISoldAnimalCalc <- ifelse(cleaned_data$LhCSISoldAnimal == 'yes' | cleaned_data$LhCSISoldAnimal == 'no_exhausted', 1, 0)
cleaned_data$LhCSIHealthEduCalc <- ifelse(cleaned_data$LhCSIHealthEdu == 'yes' | cleaned_data$LhCSIHealthEdu == 'no_exhausted', 1, 0)
cleaned_data$LhCSISeedStockCalc <- ifelse(cleaned_data$LhCSISeedStock == 'yes' | cleaned_data$LhCSISeedStock == 'no_exhausted', 1, 0)
cleaned_data$LhCSIExpFodderCalc <- ifelse(cleaned_data$LhCSIExpFodder == 'yes' | cleaned_data$LhCSIExpFodder == 'no_exhausted', 1, 0)
cleaned_data$LhCSIOutSchoolCalc <- ifelse(cleaned_data$LhCSIOutSchool == 'yes' | cleaned_data$LhCSIOutSchool == 'no_exhausted', 1, 0)
cleaned_data$LhCSIHouseLandCalc <- ifelse(cleaned_data$LhCSIHouseLand == 'yes' | cleaned_data$LhCSIHouseLand == 'no_exhausted', 1, 0)
cleaned_data$LhCSIBeggedCalc <- ifelse(cleaned_data$LhCSIBegged == 'yes' | cleaned_data$LhCSIBegged == 'no_exhausted', 1, 0)
cleaned_data$LhCSIFemAnimalCalc <- ifelse(cleaned_data$LhCSIFemAnimal == 'yes' | cleaned_data$LhCSIFemAnimal == 'no_exhausted', 1, 0)
cleaned_data$LhCSIMoveUrbanCalc <- ifelse(cleaned_data$LhCSIMoveUrban == 'yes' | cleaned_data$LhCSIMoveUrban == 'no_exhausted', 1, 0)
cleaned_data$LhCSIMoveRuralCalc <- ifelse(cleaned_data$LhCSIMoveRural == 'yes' | cleaned_data$LhCSIMoveRural == 'no_exhausted', 1, 0)

# Calculate LhCSIAverage
cleaned_data$LhCSIAverage <- (coalesce(cleaned_data$LhCSIDomAssetCalc, 0) * 2 + coalesce(cleaned_data$LhCSICrdtFoodCalc, 0) * 2 +
                                coalesce(cleaned_data$LhCSISavingUrbanCalc, 0) * 2 + coalesce(cleaned_data$LhCSISavingRuralCalc, 0) * 2 +
                                coalesce(cleaned_data$LhCSIBorrowCashCalc, 0) * 2 + coalesce(cleaned_data$LhCSIProdAssetCalc, 0) * 3 +
                                coalesce(cleaned_data$LhCSISoldAnimalCalc, 0) * 3 + coalesce(cleaned_data$LhCSIHealthEduCalc, 0) * 3 +
                                coalesce(cleaned_data$LhCSISeedStockCalc, 0) * 3 + coalesce(cleaned_data$LhCSIExpFodderCalc, 0) * 3 +
                                coalesce(cleaned_data$LhCSIOutSchoolCalc, 0) * 3 + coalesce(cleaned_data$LhCSIHouseLandCalc, 0) * 4 +
                                coalesce(cleaned_data$LhCSIBeggedCalc, 0) * 4 + coalesce(cleaned_data$LhCSIFemAnimalCalc, 0) * 4 +
                                coalesce(cleaned_data$LhCSIMoveUrbanCalc, 0) * 4 + coalesce(cleaned_data$LhCSIMoveRuralCalc, 0) * 4)

# Calculate Stress, Crisis, Emergency
cleaned_data$Stress <- ifelse((coalesce(cleaned_data$LhCSIDomAssetCalc, 0) + coalesce(cleaned_data$LhCSICrdtFoodCalc, 0) +
                                 coalesce(cleaned_data$LhCSISavingUrbanCalc, 0) + coalesce(cleaned_data$LhCSISavingRuralCalc, 0) +
                                 coalesce(cleaned_data$LhCSIBorrowCashCalc, 0)) > 0, 1, 0)

cleaned_data$Crisis <- ifelse((coalesce(cleaned_data$LhCSIProdAssetCalc, 0) + coalesce(cleaned_data$LhCSISoldAnimalCalc, 0) +
                                 coalesce(cleaned_data$LhCSIHealthEduCalc, 0) + coalesce(cleaned_data$LhCSISeedStockCalc, 0) +
                                 coalesce(cleaned_data$LhCSIExpFodderCalc, 0) + coalesce(cleaned_data$LhCSIOutSchoolCalc, 0)) > 0, 1, 0)

cleaned_data$Emergency <- ifelse((coalesce(cleaned_data$LhCSIHouseLandCalc, 0) + coalesce(cleaned_data$LhCSIBeggedCalc, 0) +
                                    coalesce(cleaned_data$LhCSIFemAnimalCalc, 0) + coalesce(cleaned_data$LhCSIMoveUrbanCalc, 0) +
                                    coalesce(cleaned_data$LhCSIMoveRuralCalc, 0)) > 0, 1, 0)


cleaned_data$LhCSICategory <- ifelse(cleaned_data$Emergency == 1, "4",
                                     ifelse(cleaned_data$Crisis == 1, "3",
                                            ifelse(cleaned_data$Stress == 1, "2", "1")))


# Calculate LhCSICategory and LhCSICategoryName
cleaned_data$LhCSICategoryName <- ifelse(cleaned_data$Emergency == 1, "Emergency", 
                                         ifelse(cleaned_data$Crisis == 1, "Crisis", 
                                                ifelse(cleaned_data$Stress == 1, "Stress", "Neutral")))





### add weights

sampling_frame <- read_csv("inputs/weights.csv")

cleaned_data_weighted <- cleaned_data %>%
  add_weights(sampling_frame, 
              strata_column_dataset = "district",
              strata_column_sample = "strata.names",
              population_column = "population")



# writing the raw and cleaned data
data<-list(questions =questions,choices=choices,raw_data=raw_data,cleaned_data=cleaned_data_weighted)
writexl::write_xlsx(data,paste("output/clean_and_raw.xlsx"))


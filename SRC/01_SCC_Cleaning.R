rm(list = ls())

library(cleaningtools)
library(tidyverse)
library(readxl)
library(ImpactFunctions) ## devtools::install_github("alex-stephenson/ImpactFunctions") if required. Used for Kobo API access.


my_raw_dataset <- ImpactFunctions::get_kobo_data(asset_id = "a78BPkkB3ZDwihdL5uqd5J", un = "abdirahmanaia")

tool_path <- "inputs/IRF_ENDLINE_TOOL_FEB2025.xlsx"
questions <- read_excel(tool_path, sheet = "survey")
choices <- read_excel(tool_path, sheet = "choices")

###check duration###############################
# Survey time check function
mindur <- 15
maxdur <- 50

kobo_settings_output <- robotoolbox::kobo_settings()

kobo_data_metadata <- get_kobo_metadata(dataset = my_raw_dataset, un = "abdirahmanaia", asset_id = "a78BPkkB3ZDwihdL5uqd5J")

data_in_processing <- kobo_data_metadata$df_and_duration %>% 
  mutate(district = str_to_lower(district)) %>% 
  mutate(across(c(if_other, `pro_safe_why/prokpi6_other`), as.character)) %>% 
  select(-uuid)

raw_metadata_length <- kobo_data_metadata$audit_files_length


data_in_processing <- data_in_processing %>%
  mutate(length_valid = case_when(
    interview_duration < mindur ~ "Too short",
    interview_duration > maxdur ~ "Too long",
    TRUE ~ "Okay"
  ))

#data_in_processing <- data_in_processing %>% 
#  filter(today == "2025-04-22")

# read in the FO/district mapping
fo_district_mapping <- read_excel("inputs/SCC_FO_Base_Assignment.xlsx") %>%
  select(district, fo_in_charge = fo_in_charge_for_code) %>%
  mutate_all(tolower)

data_in_processing <- data_in_processing %>% 
  left_join(fo_district_mapping, by = join_by("district" == "district"))

# checks

##############logic list, check list to check)
df_list_logical_checks <- read_excel("inputs/SCC_logical_checks.xlsx")

# we should exclude all questions from outlier checks that aren't integer response types (integer is the only numerical response type)
excluded_questions <- questions %>%
  filter(type != "integer" & type != "calculate") %>%
  pull(name) %>%
  unique()

# intersect 
excluded_questions_in_data <- intersect(colnames(data_in_processing), excluded_questions)

outlier_cols_not_4_checking <- my_raw_dataset %>% 
  select(matches("geopoint|gps|_index|_submit|submission|_sample_|_id|enum_id|hh_size|enum_age
                 |_Proportion|_Calc|_Have|_spent|HHH_age_final")) %>% 
  colnames()


##group data by FO
group_by_fo <- data_in_processing %>%
  dplyr::group_by(fo_in_charge)


output <-  group_by_fo %>%
  dplyr::group_split() %>%
  purrr::map( ~cleaningtools::check_duration( 
    dataset = .,
    column_to_check="interview_duration",
    uuid_column = "_uuid",
    log_name = "duration_log",
    lower_bound = 15,
    higher_bound = 60) %>% 
  check_duplicate(
    columns_to_check = c("resp_phone"),
    log_name = "duplicate phone",
    uuid_column = "_uuid") %>%
  check_pii(
    element_name = "checked_dataset",
    uuid_column = "_uuid") %>% 
  check_others(
    uuid_column = "_uuid",
    columns_to_check = names(my_raw_dataset |>
                                        dplyr::select(ends_with("_other")) |>
                                        dplyr::select(-contains(".")))) %>% 
  check_value(
    uuid_column = "_uuid"
  ) %>% 
  check_logical_with_list(uuid_column = "_uuid",
                          list_of_check = df_list_logical_checks,
                          check_id_column = "check_id",
                          check_to_perform_column = "check_to_perform",
                          columns_to_clean_column = "columns_to_clean",
                          description = "description",
                          bind_checks = TRUE) %>%
  check_outliers(
    uuid_column = "_uuid",
    element_name = "checked_dataset",
    kobo_survey = questions,
    kobo_choices = choices,
    cols_to_add_cleaning_log = NULL,
    strongness_factor = 1.5,
    minimum_unique_value_of_variable = NULL,
    remove_choice_multiple = TRUE,
    sm_separator = "/",
    columns_not_to_check = c(excluded_questions_in_data,"interview_duration" , "length_valid",outlier_cols_not_4_checking)))



cleaning_log <- output %>%
  purrr::map(~ .[] %>%
               create_combined_log() %>% 
               add_info_to_cleaning_log(
                 dataset = "checked_dataset",
                 cleaning_log = "cleaning_log",
                 dataset_uuid_column = "_uuid",
                 information_to_add = c("region","enumerator_ID", "district","today", "ngo", "resp_phone", "fo_in_charge")
               )
  )


# write to each FO's cleaning log folder
cleaning_log %>% purrr::map(~ create_xlsx_cleaning_log(.[], 
                                                       cleaning_log_name = "cleaning_log",
                                                       change_type_col = "change_type",
                                                       column_for_color = "check_binding",
                                                       header_front_size = 10,
                                                       header_front_color = "#FFFFFF",
                                                       header_fill_color = "#ee5859",
                                                       header_front = "Calibri",
                                                       body_front = "Calibri",
                                                       body_front_size = 10,
                                                       use_dropdown = T,
                                                       sm_dropdown_type = "numerical",
                                                       kobo_survey = kobo_survey,
                                                       kobo_choices = kobo_choice,
                                                       output_path = paste0("01_cleaning_logs/",
                                                                            unique(.[]$checked_dataset$fo_in_charge),
                                                                            "/",
                                                                            "cleaning_log_",
                                                                            unique(.[]$checked_dataset$fo_in_charge),
                                                                            "_",
                                                                            date_time_now,
                                                                            ".xlsx")))

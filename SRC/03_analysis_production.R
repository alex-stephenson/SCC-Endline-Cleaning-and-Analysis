
###########################################################
########################## Setup ##########################
###########################################################

rm(list = ls())

library(tidyverse)
library(cleaningtools)
library(analysistools)
library(presentresults)
library(readxl)

# write the aggregation file with a timestamp to more easily keep track of different versions
date_time_now <- format(Sys.time(), "%a_%b_%d_%Y_%H%M%S")

# load datasets for processing
file_path <- "output/clean_and_raw.xlsx"
main_data <- read_excel(file_path, 'cleaned_data') 

raw_data <- read_excel("inputs/as_raw_data.xlsx") %>% 
  select(-savings)

## tool
kobo_tool_name <- "inputs/IRF_ENDLINE_TOOL_FEB2025.xlsx"
kobo_survey <- read_excel(kobo_tool_name, sheet = "survey") %>%
  mutate(type = str_squish(type)) %>% 
  mutate(`label::English` = ifelse( 
    `label::English` == "If yes, which goods and commodity prices have increased?" & name == "community_price_goods", "If yes, which community goods and commodity prices have increased?", `label::English`))


kobo_choice <- read_excel(kobo_tool_name, sheet = "choices") %>% 
  distinct(list_name, name, .keep_all = T) %>% 
  filter(list_name != "settlement" | list_name == "settlement" & name %in% main_data$settlement) %>% 
  filter(name != "Bulsho IDP")

## load in the LOA
loa <- readxl::read_excel("inputs/scc_endline_loa.xlsx")
deletion <- read_excel("inputs/as_deletion_log.xlsx")
combined_clogs <- read_excel("combined clogs/corrected_combined cleaning clogs.xlsx") %>% 
  filter(! uuid %in% deletion$uuid)




############################### create HH survey design and analysis #################################################

#main_data_cols <- main_data %>% 
#  select(any_of(contains(loa_questions)), weights) 

SCC_endline_Survey_Design <- main_data %>% 
  srvyr::as_survey_design(., strata = "district", weights = weights)

my_analysis <- create_analysis(SCC_endline_Survey_Design, sm_separator = "/", loa = loa)

results_table <- my_analysis$results_table

## remove some issues highlighted in the review_kobo_labels

review_kobo_labels_results <- review_kobo_labels(kobo_survey,
                                                 kobo_choice,
                                                 label_column = "label::English",
                                                 results_table = results_table)

label_dictionary <- create_label_dictionary(kobo_survey, 
                                            kobo_choice, 
                                            label_column = "label::English",
                                            results_table = results_table)

results_table_labeled <- add_label_columns_to_results_table(
  results_table,
  label_dictionary
)

### making of the tables

df_main_analysis_table <- presentresults::create_table_variable_x_group(results_table = results_table_labeled, value_columns = "stat")


# Replace NA values in list columns with NULL
df_main_analysis_table <- df_main_analysis_table %>%
  mutate(across(where(is.list), ~ map(.x, ~ ifelse(is.na(.x), list(NULL), .x))))

# # Replace NA values in non-list columns with "NA"
# df_main_analysis_table <- df_main_analysis_table %>%
#   mutate(across(where(~ !is.list(.x)), ~ ifelse(is.na(.x), "NA", .x)))

df_main_analysis_table <- df_main_analysis_table %>%
  mutate(across(where(~ !is.list(.x) & is.numeric(.x)), ~ replace(.x, is.na(.x), NA))) %>%
  mutate(across(where(~ !is.list(.x) & !is.numeric(.x)), ~ ifelse(is.na(.x), "NA", .x)))



# Step 8: Export the main analysis percentages table -------------------------
presentresults::create_xlsx_variable_x_group(
  table_group_x_variable = df_main_analysis_table,
  file_path = paste0("output/results_table_long_percent.xlsx"),
  value_columns = c("stat","n"),
  overwrite = TRUE
)


# Step 9: Create and process the statistics table (counts: n, N, weighted) ----
# Ensure `df_data$results_table` is used correctly for creating a table by group
df_stats_table <- presentresults::create_table_variable_x_group(
  results_table = results_table_labeled,
  value_columns = c("n")
)

# Handle NA values in df_stats_table
df_stats_table <- df_stats_table %>%
  mutate(across(where(is.list), ~ map(.x, ~ ifelse(is.na(.x), list(NULL), .x)))) %>%
  mutate(across(where(~ !is.list(.x)), ~ ifelse(is.na(.x), "", .x)))

# Export the processed stats table to Excel
presentresults::create_xlsx_variable_x_group(
  table_group_x_variable = df_stats_table,  # Use the processed table
  file_path = paste0("output/results_table_long_values.xlsx"),
  value_columns = c("n"),
  overwrite = TRUE  
)


### create wide format now::

df_wide_analysis_table <- presentresults::create_table_group_x_variable(results_table = results_table_labeled, value_columns = "stat")


presentresults::create_xlsx_group_x_variable(
  table_group_x_variable = df_wide_analysis_table,
  file_path = paste0("output/results_table_wide_percent.xlsx"),
  overwrite = TRUE
)

df_wide_analysis_table_num <- presentresults::create_table_group_x_variable(
  results_table = results_table_labeled, value_columns = c("n"))

presentresults::create_xlsx_group_x_variable(
  table_group_x_variable = df_wide_analysis_table_num,
  file_path = paste0("output/results_table_wide_values.xlsx"),
  overwrite = TRUE
)


### final clean up of data

cols_to_remove <- c("consent_no", "enumerator_sensitivity", "deviceid", "audit", "enumerator_ID", 
                    "resp_phone", "resp_age", "_submission_time", "_validation_status", 
                    "_notes", "_status", "_submitted_by", "__version__", "_tags", "_index", "audit_URL")

raw_data_no_pii <- raw_data %>% 
  select(-any_of(cols_to_remove))

main_data_no_pii <- main_data %>% 
  select(-any_of(cols_to_remove))

deletion <- deletion %>% 
  left_join(raw_data %>%  select(`_uuid`, enumerator_ID), by = join_by("uuid" == "_uuid"))


final_output <- list(raw_data = raw_data_no_pii, cleaned_data = main_data_no_pii, survey = kobo_survey, choices = kobo_choice, deletion_log = deletion, cleaning_logs = combined_clogs)

writexl::write_xlsx(final_output, "output/all_data_logbook.xlsx")

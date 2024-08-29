###                                                                          ###
### This query pulls data from AWQMS and compares it based on 12 attributes  ###
### to determine if the result is a duplicate. The results that are exported ###
### will need to be investigated in AWQMS to confirm they are in fact        ###
### duplicates. If duplicate results are found, then a DCP and/or CAR will   ###
### need to be started to correct the issue. The script also determines      ###
### if a result is an outlier based on percentiles calculated from 10 years  ###
### of data in AWQMS.                                                        ###
###                                                                          ###
### This script was adapted from Travis Pritchard's IR duplicate and         ###
### outlier scripts. Lines 32, 33 and 37 should be the only lines that need  ###
### updated before running the script.                                       ###
###                                                                          ###

#install.packages("devtools")
#devtools::install_github("TravisPritchardODEQ/AWQMSdata",dependencies = TRUE, force = TRUE, upgrade = FALSE)

### Load tools and packages necessary for this script
library(tidyverse)
library(AWQMSdata)
library(lubridate)
library(readxl)
library(openxlsx)

### Disable scientific notation
options(scipen = 999999)

### Set working directory
setwd("//deqlab1/Assessment/AWQMS/Validation")

### Set the data window by changing these dates
### For quarterly audits, set the date range to one year
### For pre-Integrated Report audits, set the date range to five years
Start_Date <- '2024-01-01' 
End_Date <- '2024-01-31'

### Pull in list of parameter name translations and WQS units
UnitConv <- read_xlsx("//deqlab1/Assessment/AWQMS/Validation/NormalizedUnits.xlsx")
OutPerc <- read_xlsx("//deqlab1/Assessment/AWQMS/Validation/OutlierPercentiles_2024-08-19.xlsx") # make sure the date matches the file

### Load convert units function
source("https://raw.githubusercontent.com/DEQdbrown/AWQMS_Audit/main/FUNCTION_convert_units.R")

### Quarterly Audit data pull 
#All_Data <- AWQMS_Data(WQX_submit_date >= Start_Date, WQX_submit_date <= End_Date, filterQC = FALSE) %>%
#  mutate(SampleStartDate = as.Date(SampleStartDate, format = "%Y-%m-%d"))

### Pre-IR Audit data pull
All_Data <- AWQMS_Data(startdate = Start_Date, enddate = End_Date, filterQC = FALSE) %>%
  mutate(SampleStartDate = as.Date(SampleStartDate, format = "%Y-%m-%d"))

### Normalize units to help identify duplicates
NormUnits_Data <- All_Data %>%
  left_join(UnitConv, c('SampleMedia', 'chr_uid', 'Char_Name', 'Result_Unit', 'Unit_UID')) %>% # joins data with the UnitsConv file
  convert_units(unit_col = 'Unit_UID', pref_unit_col = 'Pref_Unit_UID', 
                result_col = 'Result_Numeric') %>% # this is the function listed above
  filter(Result_Operator != '<') %>% # remove non-detects
  filter(!str_detect(Activity_Type, 'Blank|Spike')) %>% # remove blanks and spikes
  relocate(c(Conv_Result, Preferred_Unit, Pref_Unit_UID), .before = ResultCondName) # moves three columns forward in the file
  

### Create a dataset of suspected duplicate data - These need to be investigated in AWQMS
straight_dups <- NormUnits_Data %>%
  filter(AU_ID != '99') %>%
  mutate(act_depth_height = ifelse(act_depth_height == 'NA', NA, act_depth_height)) %>%
  group_by(SampleMedia, # Added to Travis' code because this script deals with all media
           MLocID,
           Char_Name,
           Activity_Type,
           SampleStartDate,
           SampleStartTime,
           Statistical_Base,
           Preferred_Unit, # Replaced IRResultNWQSunit
           act_depth_height,
           Result_Depth,
           Analytical_method,
           Result_Unit,
           ParamUID, # Replaced wqstd_code
           Sample_Fraction,
           Char_Speciation,
           Time_Basis,
           Taxonomic_Name,
           act_sam_compnt_name) %>%
  mutate(num = n(),
         num_distinct_results = n_distinct(Preferred_Unit),
         num_resUID = n_distinct(Result_UID)) %>%
  mutate(group_num = cur_group_id()) %>%
  ungroup() %>%
  arrange(MLocID, SampleStartDate, Char_Name) %>%
  filter(num_resUID > 1 & num_distinct_results == 1) %>%
  arrange(group_num) %>%
  mutate(dup_type = "Duplicate")

### Filter out media specific known non-duplicates
strght_dups <- straight_dups %>%
  filter(
    case_when(
      SampleMedia == 'Habitat' & Char_Name == 'Canopy Measure' & num == '6' ~ FALSE,
      SampleMedia == 'Habitat' & Char_Name == 'Depth' & num == '5' ~ FALSE,
      SampleMedia == 'Habitat' & Char_Name == 'Distance From Left Bank' & num == '5' ~ FALSE,
      SampleMedia == 'Habitat' & Char_Name == 'Substrate Size Class, Midpoint' & num == '5' ~ FALSE,
      SampleMedia == 'Habitat' & Char_Name == 'Substrate Size Class, Transect' & num == '5' ~ FALSE,
      SampleMedia == 'Habitat' & Char_Name == 'Embeddedness' & num <= '5' ~ FALSE,
      SampleMedia == 'Habitat' & Char_Name == 'Bank Undercut' & num == '2' ~ FALSE,
      SampleMedia == 'Habitat' & Char_Name == 'Big trees (choice list)' & num == '2' ~ FALSE,
      SampleMedia == 'Habitat' & Char_Name == 'Small trees (choice list)' & num == '2' ~ FALSE,
      SampleMedia == 'Habitat' & Char_Name == 'Thalweg Wetted Width' & num == '2' ~ FALSE,
      SampleMedia == 'Habitat' & Char_Name == 'Understory Herbaceous (choice list)' & num == '2' ~ FALSE,
      SampleMedia == 'Habitat' & str_detect(Char_Name, 'Woody') & num == '2' ~ FALSE,
      SampleMedia == 'Habitat' & str_detect(Char_Name, 'Human Influence') & num == '2' ~ FALSE,
      SampleMedia == 'Habitat' & str_detect(Char_Name, 'Ground Coverage') & num == '2' ~ FALSE,
      TRUE ~ TRUE))

### Create a dataset of day/time/method duplicate results - These need to be investigated in AWQMS
day_time_dups <- NormUnits_Data %>%
  filter(AU_ID != '99') %>% 
  filter(!(Result_UID %in% straight_dups$Result_UID)) %>%
  mutate(act_depth_height = ifelse(act_depth_height == 'NA', NA, act_depth_height )) %>%
  group_by(MLocID,
           Char_Name,
           Activity_Type, 
           SampleStartDate,
           SampleStartTime,
           Statistical_Base,
           act_depth_height,
           Result_Depth,
           Analytical_method,
           ParamUID, # Replaced wqstd_code
           Sample_Fraction,
           Char_Speciation,
           Time_Basis) %>%
  mutate(num = n(),
         num_distinct_results = n_distinct(Preferred_Unit),
         num_resUID = n_distinct(Result_UID),
         num_activity_ID = n_distinct(act_id)) %>%
  mutate(group_num =cur_group_id() + 10000000000) %>%
  filter(num > 1,
         num_distinct_results > 1) %>%
  ungroup() %>%
  arrange(group_num, MLocID, SampleStartDate, Char_Name) %>% 
  mutate(dup_type = "Same Day/Time/Method; dif result")

### Combine duplicates and write file to Validation folder
all_together <- bind_rows(strght_dups, day_time_dups) %>%
  mutate(Determination = NA, 
         DCP = NA) %>%
  select(-org_name, -StationDes, -ParamUID, -ComboName, -CommonName, -AWQMS) %>%
  relocate(c(group_num, dup_type, num_resUID), .before = OrganizationID) %>%
  relocate(c(Determination, DCP), .before = dup_type)

write.xlsx(all_together, file = paste0("AWQMS_duplicates_", Sys.Date(), "test.xlsx"))

### Calculate outliers, filter out NAs and non-detects, write file to Validation folder
Outliers <- NormUnits_Data %>%
  left_join(OutPerc, c('SampleMedia', 'ParamUID', 'Char_Speciation', 'Sample_Fraction', 
                       'Statistical_Base', 'Preferred_Unit'), relationship = 'many-to-many') %>%
  mutate(out_type = case_when(Conv_Result < p01 ~ "Below the 1st percentile",
                                Conv_Result > p99 ~ "Above the 99th percentile",
                                is.na(p01) | is.na(p99) ~ "No percentile data found"),
         Determination = NA,
         DCP = NA) %>%
  select(-org_name, -StationDes, -ParamUID, -ComboName, -CommonName, -AWQMS) %>%
  relocate(c(p01, p99, out_type, Determination, DCP), .before = ResultCondName)

manual_check <- Outliers %>%
  filter(!is.na(out_type),
         Result_Operator != '<')

write.xlsx(manual_check, file = paste0("AWQMS_outliers_", Sys.Date(), "test.xlsx"))

###                                                                          ###
### This query pulls station information from AWQMS and the StationsDB then  ###
### compares them to determine if any stations are missing from either list. ###
### There are two functions in the script that look for slightly different   ###
### potential issues. The functions create a total of three lists which are  ###
### exported in the final line of the script. The issues need to be resolved ###
### in the SQL database or, potentially, in AWQMS.                           ###
###                                                                          ###
### This script was adapted from Travis Pritchard's IR duplicate and         ###
### outlier scripts.                                                         ###
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

### Complete the stations check
## Look for stations that should be attributed to multiple organizations
source("https://raw.githubusercontent.com/DEQdbrown/AWQMS_Audit/main/FUNCTION_missing_multiorg_sites.R")


## Look for stations where the OrgID in AWQMS doesn't match the OrgID in the StationsDB
source("https://raw.githubusercontent.com/DEQdbrown/AWQMS_Audit/main/FUNCTION_identify_station_errors.R")

## Export findings to Excel
print_list <- list('oregon_multiorg_sites' = station_mismatches$Oregon,
                   'nonoregon_multiorg_sites' = station_mismatches$NonOregon,
                   'station_errors' = station_errors)

openxlsx::write.xlsx(print_list, file = paste0("multi_org_station_to_add.xlsx"))
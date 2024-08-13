###                                                                          ###
### This function converts the units from AWQMS into preferred units for all ###
### parameters found in AWQMS. At times additions will need to be made to    ###
### this list. Changes made here will be used in both the OutlierDevelopment ###
### and Duplicate&Outlier scripts.                                           ###
###                                                                          ###

convert_units <- function(data, unit_col, pref_unit_col, result_col) {
  data <- data %>%
    mutate(Conv_Result = case_when(
      !!sym(unit_col) == 12 & !!sym(pref_unit_col) == 14 ~ !!sym(result_col)*0.000001, # convert pg/l to ug/l
      !!sym(unit_col) == 13 & !!sym(pref_unit_col) == 14 ~ !!sym(result_col)*0.001, # convert ng/l to ug/l
      !!sym(unit_col) == 14 & !!sym(pref_unit_col) == 15 ~ !!sym(result_col)*0.001, # convert ug/l to mg/l
      !!sym(unit_col) == 15 & !!sym(pref_unit_col) == 14 ~ !!sym(result_col)*1000, # convert mg/l to ug/l
      !!sym(unit_col) == 19 & !!sym(pref_unit_col) == 14 ~ !!sym(result_col)*0.000001, # convert mg/m3 to ug/l
      !!sym(unit_col) == 19 & !!sym(pref_unit_col) == 15 ~ !!sym(result_col)*0.001, # convert mg/m3 to mg/l
      !!sym(unit_col) == 29 & !!sym(pref_unit_col) == 511 ~ !!sym(result_col), # convert ueq/L to ueq/l
      !!sym(unit_col) == 32 & !!sym(pref_unit_col) == 31 ~ !!sym(result_col), # convert mS/cm to uS/cm
      !!sym(unit_col) == 35 & !!sym(pref_unit_col) == 1 ~ NA, # WQX issue pH data submitted in mV
      !!sym(unit_col) == 36 & !!sym(pref_unit_col) == 14 ~ !!sym(result_col)*1.5, # convert pCi/l to ug/l
      !!sym(unit_col) == 47 & !!sym(pref_unit_col) == 15 ~ !!sym(result_col)*1000, # convert mg/ml to mg/l
      !!sym(unit_col) == 49 & !!sym(pref_unit_col) == 15 ~ !!sym(result_col)*1000, # convert g/l to mg/l
      !!sym(unit_col) == 50 & !!sym(pref_unit_col) == 15 ~ !!sym(result_col)*1.42905, # convert ml/l to mg/l
      !!sym(unit_col) == 60 & !!sym(pref_unit_col) == 31 ~ !!sym(result_col), # convert umho/cm to uS/cm
      !!sym(unit_col) == 83 & !!sym(pref_unit_col) == 85 ~ !!sym(result_col)*0.000001, # convert ng/kg to mg/kg
      !!sym(unit_col) == 84 & !!sym(pref_unit_col) == 85 ~ !!sym(result_col)*0.001, # convert ug/kg to mg/kg
      !!sym(unit_col) == 86 & !!sym(pref_unit_col) == 261 ~ !!sym(result_col), # convert g/kg to ppth
      !!sym(unit_col) == 88 & !!sym(pref_unit_col) == 85 ~ !!sym(result_col)*0.001, # convert ng/g to mg/kg
      !!sym(unit_col) == 89 & !!sym(pref_unit_col) == 85 ~ !!sym(result_col), # convert ug/g to mg/kg
      !!sym(unit_col) == 91 & !!sym(pref_unit_col) == 14 ~ !!sym(result_col), # convert ppb to ug/l
      !!sym(unit_col) == 92 & !!sym(pref_unit_col) == 14 ~ !!sym(result_col)*1000, # convert ppm to ug/l
      !!sym(unit_col) == 92 & !!sym(pref_unit_col) == 15 ~ !!sym(result_col), # convert ppm to mg/l
      !!sym(unit_col) == 93 & !!sym(pref_unit_col) == 261 ~ !!sym(result_col), # convert ppth to ppth
      !!sym(unit_col) == 109 & !!sym(pref_unit_col) == 170 ~ !!sym(result_col), # convert uE/m2/sec to umol/m2/sec
      !!sym(unit_col) == 113 & !!sym(pref_unit_col) == 44 ~ !!sym(result_col)*0.028316832, # convert cfs to m3/sec 
      !!sym(unit_col) == 123 & !!sym(pref_unit_col) == 44 ~ !!sym(result_col)*0.0000630902, # convert gal/min to m3/sec
      !!sym(unit_col) == 126 & !!sym(pref_unit_col) == 44 ~ !!sym(result_col)*0.043812636574074, # convert mgd to m3/sec
      !!sym(unit_col) == 139 & !!sym(pref_unit_col) == 137 ~ !!sym(result_col)*0.0254, # convert in to m
      !!sym(unit_col) == 140 & !!sym(pref_unit_col) == 137 ~ !!sym(result_col)*0.3048, # convert ft to m
      !!sym(unit_col) == 146 & !!sym(pref_unit_col) == 164 ~ !!sym(result_col), # convert MPN to cfu/100ml
      !!sym(unit_col) == 150 & !!sym(pref_unit_col) == 148 ~ !!sym(result_col)*1000, # convert #/l to #/ml
      !!sym(unit_col) == 151 & !!sym(pref_unit_col) == 164 ~ !!sym(result_col), # convert #/100ml to cfu/100ml
      !!sym(unit_col) == 156 & !!sym(pref_unit_col) == 15 ~ !!sym(result_col), # convert 0/00 to ppth
      !!sym(unit_col) == 159 & !!sym(pref_unit_col) == 15 ~ !!sym(result_col)/0.00135968, # convert tons/ac ft to mg/l
      !!sym(unit_col) == 172 & !!sym(pref_unit_col) == 261 ~ NA, # WQX issue total solids data submitted in tons/day
      !!sym(unit_col) == 227 & !!sym(pref_unit_col) == 53 ~ !!sym(result_col)*25.4, # convert inHg to mmHg
      !!sym(unit_col) == 247 & !!sym(pref_unit_col) == 246 ~ ((!!sym(result_col)-32)*0.5556), # convert deg F to deg C
      !!sym(unit_col) == 262 & !!sym(pref_unit_col) == 164 ~ !!sym(result_col), # convert MPN/100ml to cfu/100ml
      !!sym(unit_col) == 270 & !!sym(pref_unit_col) == 44 ~ !!sym(result_col)*0.028316832, # convert ft3/sec to m3/sec
      !!sym(unit_col) == 278 & !!sym(pref_unit_col) == 15 ~ !!sym(result_col)*1000, # convert mg/cm2 to mg/l
      !!sym(unit_col) == 278 & !!sym(pref_unit_col) == 306 ~ !!sym(result_col)*1000, # convert mg/cm2 to ug/cm2
      !!sym(unit_col) == 298 & !!sym(pref_unit_col) == 103 ~ !!sym(result_col), # convert FNU to NTU
      !!sym(unit_col) == 299 & !!sym(pref_unit_col) == 103 ~ !!sym(result_col), # convert NTRU to NTU
      !!sym(unit_col) == 341 & !!sym(pref_unit_col) == 261 ~ !!sym(result_col), # convert PSU to ppt
      !!sym(unit_col) == 100076 & !!sym(pref_unit_col) == 31 ~ !!sym(result_col)*10, # convert uS/m to uS/cm
      TRUE ~ !!sym(result_col)
    ))
  return(data)
}


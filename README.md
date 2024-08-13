# AWQMS_Audit
This repository houses the scripts and function needed to audit AWQMS.

The OutlierDevelopment script pulls 10 years’ worth of data from AWQMS, compares the parameter list to the existing file, checks for new unit conversions, converts results into preferred units and calculates the 1st and 99th percentile for each parameter.

The Duplicate&OutlierQuery generates a file of potential duplicate data and a file of potential outliers in AWQMS based on a user defined time frame.

The FUNCTION_convert_units function handles all of the unit conversions necessary for the two scripts to achieve their purpose.

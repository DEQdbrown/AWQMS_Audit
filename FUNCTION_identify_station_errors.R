
identify_station_errors <- function(){
  
  #connect to AWQMS
  con <- DBI::dbConnect(odbc::odbc(), 'AWQMS-cloud',
                        UID      =   Sys.getenv('AWQMS_usr'),
                        PWD      =  Sys.getenv('AWQMS_pass'))
  
  #Get stations from AWQMS monitoring location view
  AWQMS_stations <- dplyr::tbl(con, 'monitoring_locations_vw') |>
    dplyr::group_by(mloc_id) |>
    dplyr::collect()
  
  #disconnect from AWQMS
  DBI::dbDisconnect(con)
  
  
  # Get stations from VW_StationsAllDataAllOrgs view in STATIONS ------------
  
  
  #Connect to stations
  station_con <- DBI::dbConnect(odbc::odbc(), "STATIONS")
  
  #Get VW_StationsAllDataAllOrgs
  stations_stations <- dplyr::tbl(station_con, "VW_StationsAllDataAllOrgs") |>
    dplyr::select(orgid, MLocID, EcoRegion3, EcoRegion4,HUC8, HUC8_Name, HUC10,
                  HUC12, HUC12_Name, Reachcode, Measure,AU_ID, WaterTypeCode, WaterBodyCode,
                  ben_use_code, FishCode, SpawnCode,DO_code,DO_SpawnCode, BacteriaCode,
                  pH_code) |>
    dplyr::collect()
  DBI::dbDisconnect(con)
  
  #Identify stations in AWQMS that do not properly join to the stations database
  stations_errors <- dplyr::anti_join(AWQMS_stations, stations_stations,
                                      by = dplyr::join_by('org_id' == 'orgid',
                                                          'mloc_id' == 'MLocID') )
  #Return the mismatched stations
  return(stations_errors)
  
}


#Run the function from above
station_errors <- identify_station_errors()
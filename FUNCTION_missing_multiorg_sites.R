
missing_multiorg_sites <- function(){


# Connect to AWQMS stations -----------------------------------------------


con <- DBI::dbConnect(odbc::odbc(), 'AWQMS-cloud',
                      UID      =   Sys.getenv('AWQMS_usr'),
                      PWD      =  Sys.getenv('AWQMS_pass'))

#Get stations with more than 1 org

AWQMS_stations_summary <- dplyr::tbl(con, 'monitoring_locations_vw') |>
  dplyr::group_by(mloc_id) |>
  dplyr::summarise(num_orgs = dplyr::n_distinct(org_id)) |>
  dplyr::filter(num_orgs > 1) |>
  dplyr::collect()

stations <- unique(AWQMS_stations_summary$mloc_id)

AWQMS_station <- dplyr::tbl(con, 'monitoring_locations_vw') |>
  dplyr::filter(mloc_id %in% stations) |>
  dplyr::collect()


DBI::dbDisconnect(con)


# Connect to stations -----------------------------------------------------


station_con <- DBI::dbConnect(odbc::odbc(), "STATIONS")
stations_filter <- dplyr::tbl(station_con, "VW_StationsAllDataAllOrgs") |>
  dplyr::select(orgid, MLocID, EcoRegion3, EcoRegion4,HUC8, HUC8_Name, HUC10,
                HUC12, HUC12_Name, Reachcode, Measure,AU_ID, WaterTypeCode, WaterBodyCode,
                ben_use_code, FishCode, SpawnCode,DO_code,DO_SpawnCode, BacteriaCode,
                pH_code) |>
  dplyr::filter(MLocID %in% stations) |>
  dplyr::collect()



missing_station <- AWQMS_station |>
  dplyr::left_join(stations_filter,
                   by = dplyr::join_by('org_id' == 'orgid',
                                       'mloc_id' == 'MLocID') ) |>
  dplyr::filter(is.na(HUC8))


Oregon_multiorg_sites <- missing_station |>
  dplyr::filter(stringr::str_detect(mloc_id, '-ORDEQ')) |>
  dplyr::transmute(MLocID = mloc_id,
            orgid = org_id)


nonoregon_multiorg_sites <- missing_station |>
  dplyr::filter(stringr::str_detect(mloc_id, '-ORDEQ', negate = TRUE)) |>
  dplyr::transmute(MLocID = mloc_id,
            station_name = mloc_name,
            orgid = org_id)

return(list(
  Oregon = Oregon_multiorg_sites,
  NonOregon = nonoregon_multiorg_sites
))

}

#Run the function from above
station_mismatches <- missing_multiorg_sites()
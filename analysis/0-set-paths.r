library(here)

# ---------  Source data -------------------

## Marine Cadastre
ais_marinecadastre_2022_path <- here("data", "source", "ais_marine_cadastre")
noaa_vessel_key_path <- here("data", "source", "noaa-vessel-key.csv")
## Global Fishing Watch
ais_gfw_2022_path <- here("data", "source", "global-fishing-watch", "bq-results-20231023-194005-1698090126928.csv") # nolint: line_length_linter.
## Shape Files
rice_critical_area_path <- here("data", "source", "Proposed_RicesWhale_20230724_Shapefile", "Proposed_RicesWhale_20230724.shp")
gulf_area_path <- here("data", "source", "gulf_shape_file", "iho.shp")



# ----------  Processed data --------------
## Marine Cadastre
### Critical Habitat
#### 1. Folder of filtered rds files

ais_mar_cadas_crit_hab_path <- here("data", "processed", "ais_marine_critical_habitat_rds")

#### 2. Combined rds files after they had inside_gulf added to them 

critical_habitat_ships_gom_path <- here("data", 'processed', 'critical_habitat_ships_gom.rds')
#### 3. Get the sattelite exclusive mmsi's from the terrestiral data and save into folder
sattelite_exclusive_path <- here("data","processed","search_sattelite_exclusive")
#### 4. Create a complete terresital ais data 
complete_crit_hab_terr_path <- here("data", "processed", "complete_crit_hab_terr_path.rds")

## Complete data with sattelite and terrestrial
#### 5. Combine complete terrestial data and sattelite data
terr_sat_crit_hab_sf_path <- here("data", "processed", "terr_sat_crit_hab_sf.rds")

#### 6. Vessel Key Length For unique vessels based on 
complete_key_inside_gulf_path <- here("data", "processed", "complete_key_inside_gulf.rds")

#### 7. terr_gulf_dupes
terr_gulf_dupes_path <- here("data", "processed", "terr_gulf_dupes.rds")

#### 8. terr_sat_crit_hab_sf_filled

terr_sat_crit_hab_sf_filled_path = here("data", "processed", "terr_sat_crit_hab_sf_filled.rds")

#### 9.
transit_distance_df_path = here("data", "processed", "transit_distance_df.rds")

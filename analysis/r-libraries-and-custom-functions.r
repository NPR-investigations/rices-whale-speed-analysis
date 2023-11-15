



# Libraries

library(furrr) # Parralize purrr
library(tidytable) # Fast table operations
library(tidyverse) # Data wrnaling
library(here) # File managment
library(sf) # Spatial
library(mapview) # Mapping viz
library(janitor) # Cleaning data
library(lubridate) # Date
library(tictoc) # Timing
library(glue) # Dealing with string 
library(gt) # Show tables
library(leaflet) # Map Making

# Files to be accessible to all scripts
rice_critical_area = read_sf(rice_critical_area_path)
gulf_area = read_sf(gulf_area_path)


# Display functions

cat_table <- function(df, title, subtitle = NA){

  if(is.na(subtitle)){
    subtitle = ""
  }

  df  %>% 
    gt() %>% 
    tab_header(
    title = md(title),
    subtitle = subtitle
  ) %>% 
    opt_interactive()
}

# Geospatial functions

## Extending a shape file by x miles
extend_buffer_x_miles = function(shape, miles){
    # Check CRS and transform if necessary
  if(st_crs(shape)$proj4string != "+proj=longlat +datum=WGS84 +no_defs"){
    shape <- st_transform(shape, 4326)
  }
  
  # Transform to a suitable projected CRS for accurate distance measurement
  # (e.g., Pseudo-Mercator, but consider a more local projection if available)
  shape_merc <- st_transform(shape, 3857)
  
  # Buffer - extend shape by 5 miles
# Note: 1 mile is approximately 1609.34 meters
shape_buffered <- st_buffer(shape_merc, dist = miles * 1609.34)

# Optionally, transform back to EPSG:4326 if needed
shape_buffered_4326 <- st_transform(shape_buffered, 4326)
  
}

## Cleaning Text
clean_text <- function(text) {
  text %>% 
    str_to_lower() %>%                          # Convert to lowercase
    str_remove_all("[[:punct:]]") %>%           # Remove punctuation
    str_replace_all("\\s+", "_")                # Replace spaces with underscores
}


is_point_inside_polygon = function(sf_table, shape_file, column_name){

  # Convert AIS data to same CRS as shapefile
  sf_table = sf_table  %>% 
    sf::st_transform(st_crs(shape_file))

  # Perform spatial join to see if inside or outside

  inside_shape_file = st_within(sf_table, shape_file)
  sf_table$inside_shape_file_column <- lengths(inside_shape_file) > 0

  # Rename column to column name variable
  sf_table = sf_table  %>% 
    rename({{column_name}} := inside_shape_file_column)

  return(sf_table)

}


read_filter_ais = function(file, shape_file){    
    # Read csv
    df = read_csv(file) %>% 
         janitor::clean_names()

    # with CRS 4269 according to the data source
    df_sf = st_as_sf(df, 
                        coords = c("lon", "lat"), 
                        crs = 4269)    

    df_sf_transformed = df_sf  %>% 
      is_point_inside_polygon(shape_file, "inside_polygon")
 

    # Get list of ships that passed through polygon at any point

    ls_ships_pass_through_id = df_sf_transformed  %>% 
        filter(inside_polygon == TRUE) %>% 
        distinct(mmsi) %>% 
        pull(mmsi)

    # Filter for the ship ids
    df_sf_filtered = df_sf_transformed  %>% 
        filter(mmsi %in% ls_ships_pass_through_id )

    # Return the filtered data
    return(df_sf_filtered)

}

# Function to see what files you still have to parse if needing to restart a parrallel task


get_remaining_files = function(ais_files, parsed_files){

  # Extract dates from file paths
  dates_csv <- gsub(".*/AIS_([^/]+)\\.csv$", "\\1", ais_files)
  dates_rds <- gsub(".*/AIS_([^/]+)\\.rds$", "\\1", parsed_files )

  # Find dates that are in 'dates_csv' but not in 'dates_rds'
  missing_dates <- setdiff(dates_csv, dates_rds)

  # Get the full file paths of the missing files
  missing_files <- ais_files[dates_csv %in% missing_dates]

  return(missing_files)


}


read_all_filtered_data = function(dir_path){

  # Create list of all files 
  ls_rds_filtered_files = list.files(path = dir_path, full.names = TRUE)

  # Combine all files into one sf dataframe
  df_sf_filtered  = purrr::map_df(ls_rds_filtered_files, readRDS, .progress = TRUE)

  return(df_sf_filtered)


}

## Mapping function

prep_df_for_map = function(sf_table){

  df = sf_table  %>% 
    dplyr::mutate(
      centroid = st_centroid(geometry),
      lon = st_coordinates(centroid)[, 1],
      lat = st_coordinates(centroid)[, 2]
    )


  df$popup_content <- paste("<strong>Date:</strong> ",
                            df$base_date_time, 
                            "<strong>Ais Source:</strong> ", df$ais_source,
                            "<strong>MMSI:</strong> ", df$mmsi,
                            "<strong>IMO:</strong> ", df$imo,
                            "<strong>Vessel Name:</strong> ", df$vessel_name,
                            "<br><strong>SOG:</strong> ", df$sog_for_streak,
                            "<br><strong>lat:</strong> ", df$lat,
                            "<br><strong>lon:</strong> ", df$lon)

  return(df)
  }

  single_map = function(sf_table, shape_file){

      df_for_map = prep_df_for_map(sf_table)

      ais_source_pal <- inside_pal <- colorFactor(c("purple", "yellow"), domain = df_for_map$ais_source)

      map = leaflet() %>% 
        addTiles() %>%
        # addPolygons(data = gulf_area_extended, fillColor = "blue", color = "blue", weight = 2, fillOpacity = 0.4) %>% 
        addPolygons(data = shape_file, fillColor = "blue", color = "blue", weight = 2) %>% 
        addCircleMarkers(data = df_for_map ,
                        lng = ~lon, 
                        lat = ~lat,
                        fillColor = "yellow",
                        fillOpacity = 0.8,
                        radius = 5,
                        stroke = TRUE,
                        weight = 2,
                        popup = ~popup_content)
      return(map)
  }
classify_transits = function(df){

  df_result = df  %>%
     # Sort the dataframe by 'base_date_time' and then by 'mmsi'
     arrange(base_date_time) %>% 
    # Initialize 'change_point' to TRUE for the first row to count the initial state as a transit
     mutate(change_point = ifelse(row_number() == 1, TRUE, inside_polygon != lag(inside_polygon)),
    # Create a 'transit_num' column which increments when an object enters or exits the polygon
       transit_num = cumsum(change_point), .by = c(mmsi, imo, vessel_name, vessel_type)) %>%  
     # Categorize valid and invalid transits
    mutate(lag_inside_gulf = lag(inside_gulf),  # Create a new variable that takes the previous row's inside_gulf
      not_valid = change_point & !lag_inside_gulf,
      .by = c(mmsi, imo, vessel_name, vessel_type)) 


  return(df_result)


}

# This one add the rule about a day
classify_transits2 = function(df){
  df_result = df %>%
    arrange(base_date_time) %>%
    mutate(
      time_diff = difftime(base_date_time, lag(base_date_time, default = first(base_date_time)), units = "days"),
      gap_transit = ifelse(time_diff > 1, TRUE, FALSE),  # Marking long gaps as transits
      change_point = ifelse(row_number() == 1, TRUE, inside_polygon != lag(inside_polygon) | gap_transit),
      transit_num = cumsum(change_point),
      .by = c(mmsi, imo, vessel_name, vessel_type)
    ) %>%
    mutate(
      lag_inside_gulf = lag(inside_gulf),
      not_valid = change_point & !lag_inside_gulf,
      .by = c(mmsi, imo, vessel_name, vessel_type)
    )

  return(df_result)
}





classify_speeding_streaks = function(df, speed_limit){

  # Dynamically generate column names based on the provided speed limit
  speeding_colname = paste0("speeding_", speed_limit)
  streak_colname = paste0("speeding_streak_", speed_limit)
  streak_id_colname = paste0("streak_id_", speed_limit)
  is_speeding_colname = paste0("is_speeding_", speed_limit)

  df_result = df  %>% 
    # Order the dataframe chronologically by timestamp and by ship identifier
    arrange(base_date_time) %>% 
    # Clean the 'sog' values: Replace placeholder speed value 102.3 with 0 for streak calculations
    mutate(sog_for_streak = replace(sog, sog == 102.3, 0))  %>% 
    # Set NA speeds to 0 in 'sog_for_streak' for consistent streak calculations
    mutate(sog_for_streak = replace(sog_for_streak, is.na(sog_for_streak) == TRUE, 0))  %>%  
     # Replace the placeholder speed value 102.3 with NA in the original 'sog' column
    mutate(sog = replace(sog, sog == 102.3, NA_integer_)) %>% 
    # Determine if the ship is speeding and calculate the consecutive speeding streaks
    mutate(speeding = ifelse(sog_for_streak > speed_limit, 1, 0), # Binary column: 1 if speeding, 0 otherwise
         speeding_streak = cumsum(speeding) - cummax((1-speeding)*cumsum(speeding)), .by = c(mmsi, imo, vessel_name, vessel_type, transit_num)) %>% 
    # Calculate streak id's
     mutate(
        is_speeding = speeding_streak > 0,
        was_speeding = lag(is_speeding, default = FALSE),
        speeding_change = is_speeding != was_speeding,
        streak_id = cumsum(speeding_change), 
        .by = c(mmsi, imo, vessel_name, vessel_type, transit_num))   %>% 
    # Rename the calculated columns using the dynamically generated column names
    rename( {{ speeding_colname }} := speeding,
            {{is_speeding_colname}} := is_speeding,
            {{ streak_colname }} := speeding_streak,
            {{streak_id_colname}} := streak_id) 
    # Get rid of columns that are not needed
    # select(-c( was_speeding, speeding_change))

  return(df_result)
}

# Distance weighted average methdology ------------------------------


classify_distances = function(sf_table){

    sf_result = sf_table  %>% 
    # Reproject the data into UTM Zone 15N so that we can measure meters    
    tidytable::distinct(mmsi, imo, vessel_name, vessel_type, transit_num, base_date_time, .keep_all = TRUE) %>% 
    tidytable::arrange(base_date_time)  %>%
    st_as_sf() %>%  
    st_transform(26915) %>% 
    as_tibble() %>% 
    tidytable::mutate(distance_meters = st_distance(geometry, lag(geometry), by_element = TRUE), # Calculate distance to previous point in meters
      distance_nautical_miles = distance_meters / 1852, # Convert distance to nautical miles
      time_diff_hours = as.numeric(difftime(base_date_time, lag(base_date_time), units = "hours")), # Time difference in hours
      .by = c(mmsi, imo, vessel_name, vessel_type, transit_num))  %>% 
    # Remove the NA from the first entry because of the lag
    # tidytable::filter(!is.na(distance_nautical_miles)) %>%
    tidytable::mutate(
      speed_segment_weight = distance_nautical_miles / sum(distance_nautical_miles, na.rm = TRUE), # Weight of each segment based on nautical mile distance
      .by = c(mmsi, imo, vessel_name, vessel_type, transit_num)) %>% 
    # calulate sog for each segment
    tidytable::mutate(sog_between = distance_nautical_miles / time_diff_hours ) %>%
    # Convert back to the original CRS
    st_as_sf() %>% 
    st_transform(4269)  
    return(sf_result)


}

summarise_dwas = function(sf_table){
  df_result = sf_table  %>% 
    as_tibble() %>% 
    tidytable::summarize(
        start_transit_date = min(base_date_time), # Start time of transit
        end_transit_date = max(base_date_time), # # End time of transit
        n_ais_responses = n(),
        total_distance_nm = sum( distance_nautical_miles, na.rm = TRUE),
        dwas = sum(sog * speed_segment_weight, na.rm = TRUE), # Calculate weighted average speed in knots
        .by = c(mmsi, imo, vessel_name, vessel_type, transit_num)
      ) %>% 
    mutate(transit_in_hours = as.numeric(difftime(end_transit_date , start_transit_date , units = "hours")),
      transit_in_minutes = as.numeric(difftime(end_transit_date , start_transit_date , units = "mins"))) 
}

summarise_dwas2 = function(sf_table){
  df_result = sf_table  %>% 
    as_tibble() %>% 
    tidytable::summarize(
        start_transit_date = min(base_date_time), # Start time of transit
        end_transit_date = max(base_date_time), # # End time of transit
        n_ais_responses = n(),
        total_distance_nm = sum( distance_nautical_miles, na.rm = TRUE),
        dwas = sum(sog_between * speed_segment_weight, na.rm = TRUE), # Calculate weighted average speed in knots
        .by = c(mmsi, imo, vessel_name, vessel_type, transit_num)
      ) %>% 
    mutate(transit_in_hours = as.numeric(difftime(end_transit_date , start_transit_date , units = "hours")),
      transit_in_minutes = as.numeric(difftime(end_transit_date , start_transit_date , units = "mins"))) 
}


 make_transit_schedule = function(sf){

     # Convert the spatial dataframe 'sf' to a 'tidytable' format
    df_result = sf  %>% 
      as_tibble() %>% 
      as_tidytable() %>% 
      # Remove the 'geometry' column, which contains spatial data
      select(-geometry)  %>% 
      # Aggregate the data based on 'mmsi' and 'transit_num' 
      tidytable::summarise(
        # Identify the earliest and latest date-time values for each transit
        start_transit_date = min(base_date_time), # Start time of transit
        end_transit_date = max(base_date_time), # # End time of transit
        # Count the number of AIS responses for each transit
        number_ais_responses = n(),  
        # Compute descriptive statistics for the 'sog' column (Speed Over Ground)
        across(.cols = c(sog),
              .fns = list(min = min , max = max, mean = mean, median = median),
              na.rm = TRUE,
              .names = "{col}_{fn}"),
        # Capture maximum speeding streak values for two specific speed limits
        max_speeding_streak_10 = max(speeding_streak_10),
        max_speeding_streak_12 = max(speeding_streak_12),
        # Group the summarization by ship identifier and transit number
        .by = c(mmsi, transit_num))  %>%  
      # Calculate transit duration in both hours and minutes             
      mutate(transit_in_hours = as.numeric(difftime(end_transit_date , start_transit_date , units = "hours")),
      transit_in_minutes = as.numeric(difftime(end_transit_date , start_transit_date , units = "mins")))  %>% 
      # Classify as speeding or not
      mutate(speeding_above_10 = case_when(
       max_speeding_streak_10  >= 3 ~ "speed_above_10",
       max_speeding_streak_10  < 3 ~ "speed_below_10",
       TRUE ~ "PROBLEM"),
       speeding_above_12 = case_when(
       max_speeding_streak_12  >= 3 ~ "speed_above_12",
       max_speeding_streak_12  < 3 ~ "speed_below_12",
       TRUE ~ "PROBLEM"

   ))   

    return(df_result)



}


make_transit_schedule2 = function(sf){

     # Convert the spatial dataframe 'sf' to a 'tidytable' format
    df_result = sf  %>% 
      as_tibble() %>% 
      as_tidytable() %>% 
      # Remove the 'geometry' column, which contains spatial data
      select(-geometry)  %>% 
      # Aggregate the data based on 'mmsi' and 'transit_num' 
      tidytable::summarise(
        # Identify the earliest and latest date-time values for each transit
        start_transit_date = min(base_date_time), # Start time of transit
        end_transit_date = max(base_date_time), # # End time of transit
        # Count the number of AIS responses for each transit
        number_ais_responses = n(),  
        # Compute descriptive statistics for the 'sog' column (Speed Over Ground)
        across(.cols = c(sog),
              .fns = list(min = min , max = max, mean = mean, median = median),
              na.rm = TRUE,
              .names = "{col}_{fn}"),
        # Capture maximum speeding streak values for two specific speed limits
        max_speeding_streak_10 = max(speeding_streak_10),
        # Group the summarization by ship identifier and transit number
        .by = c(mmsi, imo, vessel_name, vessel_type, transit_num))  %>%  
      # Calculate transit duration in both hours and minutes             
      mutate(transit_in_hours = as.numeric(difftime(end_transit_date , start_transit_date , units = "hours")),
      transit_in_minutes = as.numeric(difftime(end_transit_date , start_transit_date , units = "mins")))  %>% 
      # Classify as speeding or not
      mutate(speeding_above_10 = case_when(
       max_speeding_streak_10 >= 3 ~ "speed_above_10",
       max_speeding_streak_10 < 3 ~ "speed_below_10",
       TRUE ~ "PROBLEM")
   )   

    return(df_result)



}

transit_summary = function(df, grouping_var){

  df_result = df  %>% 
    tidytable::summarise(number_transits = n(), .by = c({{grouping_var}})) %>% 
    mutate(pct = (number_transits/sum(number_transits))*100) 

}

transit_ship_type_summary = function(df, grouping_var){

  speed_num <- gsub(".*_above_([0-9]+)", "\\1", deparse(substitute(grouping_var)))
  desc_variable = glue("pct_speed_above_{speed_num}")

  df_result = df  %>% 
    tidytable::summarise(number_transits = n() , .by = c(vessel_group_2018, {{grouping_var}})) %>% 
    mutate(pct = (number_transits/sum(number_transits))*100, .by = vessel_group_2018) %>%
    dplyr::pivot_wider(names_from = {{grouping_var}}, values_from = c(contains("number"), pct), values_fill = 0)  %>% 
    arrange(desc(!!sym(desc_variable))) %>% 
    dplyr::left_join(df_number_ship_types_clean, join_by(vessel_group_2018)) %>% 
    rename(number_ships = n)
}
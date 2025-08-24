# f_gsheets_routing.R
`%ni%` <- Negate(`%in%`)
# .............................
create_route <- function(
    origin = NULL, 
    destination = NULL, 
    routetype = NULL, 
    swap_coordinates = TRUE, 
    select_overview = 'simplified', 
    select_exclude = 'ferry') {
  
  if (swap_coordinates) {
    origin <- c(origin[2], origin[1])
    destination <- c(destination[2], destination[1])
  }
  
  if (is.na(select_exclude)) select_exclude <- NULL
  
  route <- mapboxapi::mb_directions(
    # access_token = token,
    origin = origin,
    destination = destination,
    exclude = select_exclude, 
    overview = select_overview, 
    profile = routetype,  # Options: "driving", "walking", "cycling"
    output = "sf"        # Return as sf object
  )
  
  return(route)
}
# .............................

# .............................
separate_coordinates <- function(data = NULL) {
  # .............................
  # in different fields in source data as
  # .............................
  coord <- data %>%
    mutate(
      # Only fill origin_lat/lon if they are NA and bing_start is not NA
      origin_lat = if_else(is.na(origin_lat) & !is.na(bing_start),
                           as.numeric(sub(",.*", "", bing_start)),
                           origin_lat),
      origin_lon = if_else(is.na(origin_lon) & !is.na(bing_start),
                           as.numeric(sub(".*,", "", bing_start)),
                           origin_lon),
      # Only fill destination_lat/lon if they are NA and bing_end is not NA
      destination_lat = if_else(is.na(destination_lat) & !is.na(bing_end),
                                as.numeric(sub(",.*", "", bing_end)),
                                destination_lat),
      destination_lon = if_else(is.na(destination_lon) & !is.na(bing_end),
                                as.numeric(sub(".*,", "", bing_end)),
                                destination_lon)
    )
  
  return(coord)
}
# .............................




# .............................
select_rerouting <- function(data = NULL, old_data = NULL, reroute_ids = NULL) {
  # reroute_ids <- 54
  if (!is.null(old_data)) {
    already_processed <- old_data
    if (!is.null(reroute_ids)) {
      already_processed <- already_processed |> dplyr::filter(route_id %ni% reroute_ids) # force recalcualte
    }
    #coord <- data |> dplyr::filter(route_id %ni% already_processed$route_id)
  } else {
    #coord <- data
  }
  #coord <- separate_coordinates(data = coord)
  return(already_processed)
}
# .............................

# .............................
update_coord_df <- function(data = NULL, old_data = NULL) {
  if (!is.null(old_data)) {
    coord <- data |> dplyr::filter(route_id %ni% already_processed$route_id)
  } else {
    coord <- data
  }
  coord <- separate_coordinates(data = coord)
  return(coord)
}
# .............................


# .............................
createNewAllgeojson <- function(input = NULL, output = NULL, filename_all_output = "all_routes.geojson") {
  # .............................
  # read all subfiles
  # .............................
  # List all .geojson files in folder
  files <- list.files(input, pattern = "\\.geojson$", full.names = TRUE)
  files_id <- grep("routes", files) # only routes files
  files <- files[files_id] # remove suomi
  # Read and combine all into one sf object
  routes_list <- lapply(files, st_read, quiet = TRUE)
  all_routes <- do.call(rbind, routes_list)
  # Write combined GeoJSON
  sf::st_write(all_routes, paste0(output, filename_all_output), driver = "GeoJSON", delete_dsn = TRUE)
  # .............................
}
# .............................



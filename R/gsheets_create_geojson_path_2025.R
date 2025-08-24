# gsheets_create_geojson_path_2025.R
library(sf)

# todo:
# W 40v. köpis 2017? pyöräily Mikkeller
# Tallink kesäpyröärliyt tallinna

# token <- "your token"
# mapboxapi::mb_access_token(token, install = TRUE)
token <- Sys.getenv("MAPBOX_PUBLIC_TOKEN")

source("C:/Users/talon/git/Reteatteri/R/f_gsheets_routing.R")

# Folder containing geojson files
output_folder <- "I:/Oma Drive/data/travelling/Routput/"

all_folder <- "C:/Users/talon/git/compass/geojsonmap/Rgeojson/"

suomikartta <- TRUE # FALSE

if (suomikartta) {
  tab_name_c <- "suomiautoilu"
  select_full_file_name <- "suomi_routes.geojson"
} else {
  select_full_file_name <- "all_routes.geojson"
  # .............................
  tab_name_c <- c("routes2000", 
                  "routes2001", 
                  "routes2002", 
                  "routes2003", 
                  "routes2004", 
                  "routes2005", 
                  "routes2006", 
                  "routes2007", 
                  "routes2008", 
                  "routes2009", 
                  "routes2010", 
                  "routes2011", 
                  "routes2012", 
                  "routes2015", 
                  "routes2018", 
                  "routes2019", 
                  "routes2022", 
                  "routes2023",
                  "routes2024", 
                  "routes2025")
  
}
# .............................
for (i_tab_name in tab_name_c) {
  print(i_tab_name)
  # .............................
  paths_data <- readr::read_csv('I:/Oma Drive/data/googlesheetRpaths/gsheet_paths_2025.txt',show_col_types = FALSE)
  links_filtered <- paths_data |>
    filter(data == i_tab_name) |> pull(link)
  # .............................
  coord_orig <- gsheet::gsheet2tbl(links_filtered)
  file_path_input <- paste0(output_folder, i_tab_name, ".geojson")
  if (file.exists(file_path_input)) {
    already_processed <- sf::st_read(file_path_input)
  } else {
    message("File does not exist: ", file_path_input)
    already_processed <- NULL
  }
  # .............................
  # reroute
  # .............................
  select_reroute_ids <- NULL  # c(20,21)
  select_reroute_ids <- c(select_reroute_ids, coord_orig$route_id[which(coord_orig$reroute == 1)])
  already_processed <- select_rerouting(old_data = already_processed, reroute_ids = select_reroute_ids)
  coord <- update_coord_df(data = coord_orig, old_data = already_processed)
  # .............................
  
  
  if (dim(coord)[1]>0) {
    # .............................
    # routing
    # .............................
    routes <- list()
    for (i in c(1:dim(coord)[1])) {
      print(
        paste0(i, ": ", coord$route_id[i], ": ", coord$origin_lat[i], 
               ", ", coord$origin_lon[i], " to ",
               coord$destination_lat[i], ", ", 
               coord$destination_lon[i], " (", coord$routetype[i], ")"))
      
      if (isTRUE(coord$realroute[i] %in% c("cablecar","ferrymanual", "swimming"))) {
        # Create LINESTRING
        routes[[i]] <- sf::st_sf(
          #route_id = coord$route_id[i],
          geometry = sf::st_sfc(
            sf::st_linestring(
              matrix(
                c(coord$origin_lon[i], coord$destination_lon[i],   # X = lon
                  coord$origin_lat[i], coord$destination_lat[i]),  # Y = lat
                ncol = 2
              )
            ),
            crs = 4326
          )
        )
        routes[[i]]$distance <- 0
        routes[[i]]$duration <- 0
      } else {
        routes[[i]] <- create_route(
          origin = c(coord$origin_lon[i],coord$origin_lat[i]),
          destination = c(coord$destination_lon[i],
                          coord$destination_lat[i]), 
          select_overview = 'full', 
          routetype = coord$routetype[i], 
          select_exclude = coord$exclude[i],
          swap_coordinates = FALSE
        )
        print(paste("Distance:", routes[[i]]$distance))
      }
    }
    # .............................
    
    # .............................
    # include meta data
    # .............................
    route_all <- data.table::rbindlist(routes)
    route_all$route_id <- coord$route_id
    route_all$origin_lat <- coord$origin_lat
    route_all$origin_lon <- coord$origin_lon
    route_all$destination_lat <- coord$destination_lat
    route_all$destination_lon <- coord$destination_lon
    route_all$routetype <- coord$routetype
    route_all <- route_all |> mutate(realroute = ifelse(is.na(coord$realroute), coord$routetype, coord$realroute))
    route_all$exclude <- coord$exclude
    # .............................
    
    # .............................
    # Convert data.table to sf using the existing geometry column
    route_all_sf <- st_as_sf(route_all, crs = st_crs(already_processed))
    # route_all_2 <- rbind(already_processed, route_all_sf)
    route_all_2 <- dplyr::bind_rows(already_processed, route_all_sf)
    route_all_2 <- route_all_2 |> mutate(realroute = ifelse(is.na(realroute), routetype, realroute))
    
    route_all_2 <- route_all_2 |> dplyr::arrange(route_id) |> distinct(route_id, .keep_all = TRUE) # some changes in function, later no need.
    
    file_output_name <- paste0(output_folder, i_tab_name, ".geojson")
    sf::st_write(route_all_2, file_output_name, driver = "GeoJSON", delete_dsn = TRUE)
    # .............................
  } else {
    print("no updates")
  }
}

if (suomikartta) {
  sf::st_write(route_all_2, paste0(all_folder, select_full_file_name), driver = "GeoJSON", delete_dsn = TRUE)
} else {
  createNewAllgeojson(input = output_folder, output = all_folder, filename_all_output = select_full_file_name)
}







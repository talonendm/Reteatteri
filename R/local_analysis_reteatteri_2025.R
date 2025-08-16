# git Reteatteri/R/ local_analysis_reteatteri_2025.R
# ...............................................
rstudioapi::writeRStudioPreference("data_viewer_max_columns", 100L)
# ...............................................
library(gsheet) # not in shinyapps
library(dplyr)
library(stats)
library(readr)
library(dplyr)
library(zoo)
library(RColorBrewer)
library(viridis)  # for colorblind-friendly palettes
# ...............................................
source("C:/Users/talon/git/Reteatteri/R/f_local_reteatteri2025.R")
# gsheets is used: install.packages("googlesheets4") # library(googlesheets4)
# ...............................................
paths_data <- readr::read_csv('I:/Oma Drive/data/googlesheetRpaths/gsheet_paths_2025.txt',show_col_types = FALSE)
links_filtered <- paths_data |>
  filter(data == "raflasafkadata") |> pull(link)

restaurantsheet <- gsheet::gsheet2tbl(links_filtered)
# ...............................................
data <- cleanGoogleSheet(restaurantsheet)
da0 <- data |> dplyr::select(id, rundi, restaurant = Mesta, rank, host = Isäntä, info, latitude = lat, longitude = lon, avg, genre = genreclean, vuosi)  
# ...............................................
# points, Scale from 0 to 100, scaled by the number hosts per round -----------
# ...............................................
da0.g <- da0 |> dplyr::group_by(rundi) |> dplyr::reframe(n_hosts = n())
da0 <- da0 |> dplyr::left_join(da0.g, "rundi")
da0 <- da0 |> 
  dplyr::mutate(
    max_point_avg_c = 100 / (n_hosts) * 1,
    min_point_avg_c = 100 / (n_hosts) * (n_hosts - 1) # everyone ranked as worst restaurant
  )
# ...............................................
da0$avg[is.na(da0$avg)] <- da0$min_point_avg_c[is.na(da0$avg)]
# ...............................................

# ...............................................
da0$totalPoints <- round(100 - 100 * ((da0$avg - da0$max_point_avg_c) / (da0$min_point_avg_c - da0$max_point_avg_c)),0)
da0 <- da0 |> dplyr::arrange(-totalPoints, avg)
da0$totalrank <- c(1:dim(da0)[1])
da0$totalrank[da0$totalPoints == 0 ] <- NA
# ...............................................


# ...............................................
# voting started at round 10 ------------
# ...............................................
da1 <- da0 |> dplyr::filter(rundi>=10) |> dplyr::arrange(id)
fig_cum_avg(data = da1, windowsize = 8)
fig_cum_avg(data = da1, windowsize = 0)
fig_cum_avg(data = da1, windowsize = -1)
fig_rank_plots(data = da1, ranktype = 0)
fig_rank_plots(data = da1, ranktype = 1)


fig_cum_avg(data = da1, windowsize = 3, matriximage = 1)
fig_cum_avg(data = da1, windowsize = 3, matriximage = 2)
fig_cum_avg(data = da1, windowsize = 1, matriximage = 2)
# ...............................................
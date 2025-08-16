# ...............................................
# f_local_reteatteri2025.R
# ...............................................
# Host name manipulation -----------------
changeHost <- function(data) {
  
  # Define mapping (all lowercase keys)
  replacements <- c(
    "jaakko" = "JA",
    "mikko"  = "MI",
    "jussi"  = "JU",
    "kari"   = "KA",
    "lauri"  = "LA", "lyry" = "LA",
    "rude"   = "MA",
    "salkki" = "HE",
    "grande" = "GR"
  )
  
  # Normalize all host names to lowercase before mapping
  data$Isäntä <- tolower(data$Isäntä)
  data$Isäntä <- dplyr::recode(data$Isäntä, !!!replacements, .default = data$Isäntä)
  
  return(data)
}

# ...............................................

# ...............................................
# who visited restaurant ------------------
hasVisited <- function(data = NULL) {
  data$JUm <- as.numeric(data$JUs>0)
  data$MAm <- as.numeric(data$MAs>0)
  data$LAm <- as.numeric(data$LAs>0)
  data$KAm <- as.numeric(data$KAs>0)
  data$JAm <- as.numeric(data$JAs>0)
  data$MIm <- as.numeric(data$MIs>0)
  data$HEm <- as.numeric(data$HEs>0)
  data$GRm <- as.numeric(data$GRs>0)
  return(data)
}
# ...............................................

# ...............................................
myf <- function(arg = NA, tyyppi = NA , da4 = NA){
  #  print(da3$rundi)
  # print(arg)
  # https://stackoverflow.com/questions/18828486/using-apply-function-on-a-matrix-with-na-entries
  # JU1 <- as.data.frame( (100 / (1 + tapply(arg, da4$rundi, FUN=sum,na.rm=TRUE))))
  # JU1$rundi <- c(1:dim(JU1)[1])
  da4$values <- arg
  weight_by_rundi <- da4 |> group_by(rundi) |> dplyr::summarise(vv = 100 / (1 + sum(values, na.rm  = TRUE ) ) )
  weight_by_rundi$vv[weight_by_rundi$vv == 100] <- NA
  names(weight_by_rundi)[2] <- tyyppi
  da4 <- merge(da4, weight_by_rundi, by = "rundi")
  return(da4)
}
# ...............................................


# ...............................................
# proportional points - formula ---------
calculateProportionalPoints <- function(voteweight = NA, voterank =NA) {
  poge <- round(voteweight * voterank, 3) 
  poge[poge == 0] <- NA
  return(poge)
}
# ...............................................


# ...............................................
# cleans google sheet everytime - this can be optimized later --------------------
cleanGoogleSheet <- function(restaurants0 = restaurantsheet) {
  
  restaurants1 <- changeHost(data = restaurants0)
  restaurants2 <- hasVisited(data = restaurants1)
  
  restaurants2 <- myf(arg = restaurants2$JUm, "JU_voteweight", da4 = restaurants2)
  restaurants2 <- myf(restaurants2$MAm, "MA_voteweight", da4 = restaurants2)
  restaurants2 <- myf(restaurants2$LAm, "LA_voteweight", da4 = restaurants2)
  restaurants2 <- myf(restaurants2$KAm, "KA_voteweight", da4 = restaurants2)
  restaurants2 <- myf(restaurants2$JAm, "JA_voteweight", da4 = restaurants2)
  restaurants2 <- myf(restaurants2$MIm, "MI_voteweight", da4 = restaurants2)
  restaurants2 <- myf(restaurants2$HEm, "HE_voteweight", da4 = restaurants2)
  restaurants2 <- myf(restaurants2$GRm, "GR_voteweight", da4 = restaurants2)
  
  restaurants2$JU <- calculateProportionalPoints(voteweight = restaurants2$JU_voteweight, voterank = restaurants2$JUs)
  restaurants2$MA <- calculateProportionalPoints(voteweight = restaurants2$MA_voteweight, voterank = restaurants2$MAs)
  restaurants2$LA <- calculateProportionalPoints(voteweight = restaurants2$LA_voteweight, voterank = restaurants2$LAs)
  restaurants2$KA <- calculateProportionalPoints(voteweight = restaurants2$KA_voteweight, voterank = restaurants2$KAs)
  restaurants2$JA <- calculateProportionalPoints(voteweight = restaurants2$JA_voteweight, voterank = restaurants2$JAs)
  restaurants2$MI <- calculateProportionalPoints(voteweight = restaurants2$MI_voteweight, voterank = restaurants2$MIs)
  restaurants2$HE <- calculateProportionalPoints(voteweight = restaurants2$HE_voteweight, voterank = restaurants2$HEs)
  restaurants2$GR <- calculateProportionalPoints(voteweight = restaurants2$HE_voteweight, voterank = restaurants2$GRs)
  
  restaurants2$avg <- base::rowMeans(restaurants2[, c("JU", "MA", "LA" ,"KA" ,"JA" ,"MI" ,"HE", "GR")], na.rm = TRUE)
  
  # http://stackoverflow.com/questions/9961700/how-to-partition-when-ranking-on-a-particular-column
  da7 <- base::transform(restaurants2, rank = stats::ave(avg, rundi, FUN = function(x) rank(x, ties.method = "first"))) # rank by round
  da7$rank[is.na(da7$avg)] <- NA
  restaurants <- da7 # dplyr::left_join(restaurants3, da7, "id")
  
  coords <- (data.table::tstrsplit(restaurants$sijainti,",", names = TRUE))
  
  restaurants$lat <- as.numeric(coords[[1]])
  class(restaurants$lat)
  restaurants$lon <- as.numeric(coords[[2]])
  
  return(restaurants)
}
# ...............................................


# ...............................................
# time series
# ...............................................
fig_cum_avg <- function(data = NULL, windowsize = 0, matriximage = 0) {
  
  
  if (windowsize == 0) {
    # Step 1: Calculate cumulative average per host using "avg"
    ts_host_cumavg <- data |>
      dplyr::group_by(host) |>
      dplyr::arrange(rundi) |>
      dplyr::mutate(
        cumavg_avg = dplyr::cummean(avg)
      ) |>
      dplyr::ungroup()
    
    
    # Step 2: Plot with ggplot using viridis palette
    ggplot(ts_host_cumavg, aes(x = rundi, y = cumavg_avg, color = host)) +
      geom_line(size = 1.2) +
      geom_point(size = 2) +
      scale_color_viridis_d(option = "turbo", begin = 0, end = 0.9) +  # distinct colors
      labs(
        title = "Cumulative Average of 'avg' per Host",
        x = "Round (rundi)",
        y = "Cumulative Average 'avg'",
        color = "Host"
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 10)
      )
    
    
    
  } else if (windowsize == -1)  {
    
    ggplot(data, aes(x = host, y = avg, fill = host)) +
      geom_boxplot() +
      scale_fill_viridis_d(option = "turbo", begin = 0, end = 0.9) +  # distinct colors
      labs(
        title = "Distribution of 'avg' by Host",
        x = "Host",
        y = "Average Points ('avg')",
        fill = "Host"
      ) +
      theme_minimal() +
      theme(
        legend.position = "none"  # optional, since host is on x-axis
      )
    
  }else {
    
    
    
    ts_host_ma <- data |>
      group_by(host) |>
      arrange(rundi) |>
      mutate(
        ma_totalPoints = rollapply(
          totalPoints,
          width = windowsize,         # max window
          FUN = mean,
          align = "right",
          partial = TRUE,     # <--- allows smaller windows at start
          na.rm = TRUE
        )
      ) |>
      ungroup()
    
    
    
    if (matriximage > 0) {
      
      
      if (matriximage == 1) {
        
        ts_ranked <- ts_host_ma %>%
          group_by(rundi) %>%
          mutate(rank = rank(-ma_totalPoints, ties.method = "min")) %>%  # higher points = better rank
          ungroup()
        
        
        rank_matrix <- ts_ranked %>%
          select(rundi, host, rank) %>%
          pivot_wider(names_from = host, values_from = rank)
        
        ggplot(ts_ranked, aes(x = rundi, y = host, fill = rank)) +
          geom_tile(color = "white") +
          scale_fill_gradient(low = "gold", high = "red") + # better rank = brighter color
          labs(
            title = "Host Rankings per Round",
            x = "Round (rundi)",
            y = "Host",
            fill = "Rank"
          ) +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom"
          )
      } else {
        
        
        ts_ranked <- ts_host_ma %>%
          group_by(rundi) %>%
          mutate(rank = rank(avg, ties.method = "min")) %>%
          ungroup()
        
        ts_ranked <- ts_ranked %>%
          group_by(rundi) %>%
          arrange(rank) %>%  # ascending rank (1 = top)
          mutate(host_ordered = factor(host, levels = host)) %>%
          ungroup()
        
        
        ggplot(ts_ranked, aes(x = rundi, y = host_ordered, fill = ma_totalPoints)) +
          geom_tile(color = "white") +
          geom_text(aes(label = rank), color = "black") +  # show rank number inside tile
          scale_fill_gradient(
            low = "red",
            high = "green",
            limits = c(0, 100),
            guide = "none"  # hide the fill legend  # fixes the scale from 0 to 100
          ) +
          labs(
            title = "Host Rankings per Round",
            x = "Round (rundi)",
            y = "Host",
            fill = "Rank"
          ) +
          theme_minimal() +
          theme(
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position = "bottom"
          )
      }
      
    } else {
      
      
      
      # Define a palette with 8 distinct colors
      palette8 <- brewer.pal(n = 8, name = "Set2")
      
      # Plot with ggplot
      ggplot(ts_host_ma, aes(x = rundi, y = ma_totalPoints, color = host)) +
        geom_line(size = 1.2) +
        geom_point(size = 2) +
        scale_color_manual(values = palette8) +
        labs(
          title = paste0(windowsize,"-Round Moving Average of Total Points per Host"),
          x = "Round (rundi)",
          y = "Moving Average of Total Points",
          color = "Host"
        ) +
        theme_minimal() +
        theme(
          legend.position = "bottom",
          legend.title = element_text(size = 12),
          legend.text = element_text(size = 10)
        )
      
    }
  }
  
}
# ...............................................

fig_rank_plots <- function(data = NULL, ranktype = 0) {
  if (ranktype == 0) {
    # Filter only ranks 1-8 and compute cumulative count per host and rank
    cumulative_ranks <- data |>
      filter(rank %in% 1:8) |>
      group_by(host, rank) |>
      arrange(rundi) |>
      mutate(cum_count = row_number()) |>  # cumulative count
      ungroup() |>
      mutate(rank_type = factor(rank, levels = 1:8, labels = paste0("Rank ", 1:8)))
    
    # Determine maximum cumulative count for y-axis
    max_cum <- max(cumulative_ranks$cum_count, na.rm = TRUE)
    
    # Define distinct colors for ranks 1-8
    rank_colors <- c("darkgreen", "red", "blue", "orange", "purple", "brown", "pink", "cyan")
    names(rank_colors) <- paste0("Rank ", 1:8)
    
    # Plot
    ggplot(cumulative_ranks, aes(x = rundi, y = cum_count, color = rank_type)) +
      geom_line(aes(group = rank_type), size = 1.2) +
      geom_point(aes(size = rank_type)) +
      scale_size_manual(values = c(3, 2.8, 2.6, 2.4, 2.2, 2, 1.8, 1.5)) +
      scale_color_manual(values = rank_colors) +
      facet_wrap(~host, nrow = 2, ncol = 4, scales = "fixed") +
      scale_y_continuous(limits = c(0, max_cum), breaks = 0:max_cum) +
      labs(
        title = "Cumulative Counts of Ranks 1-8 per Host",
        x = "Round (rundi)",
        y = "Cumulative Count",
        color = "Rank",
        size = "Rank"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    
  } else if (ranktype == 1) {
    
    # Step 1: Add flags for grouped ranks
    da1_grouped <- data |>
      mutate(
        rank1_flag = ifelse(rank == 1, 1, 0),
        rank2_flag = ifelse(rank %in% c(2), 1, 0),
        rank3_flag = ifelse(rank %in% c(3), 1, 0),
        rank7_8_flag = ifelse(rank %in% c(7,8), 1, 0)
      )
    
    # Step 2: Compute cumulative counts per host
    cumulative_grouped <- da1_grouped |>
      group_by(host) |>
      arrange(rundi) |>
      mutate(
        cum_rank1 = cumsum(rank1_flag),
        cum_rank2 = cumsum(rank3_flag),
        cum_rank3 = cumsum(rank2_flag),
        cum_rank7_8 = cumsum(rank7_8_flag)
      ) |>
      ungroup() |>
      select(id, rundi, host, cum_rank1, cum_rank2, cum_rank3, cum_rank7_8) |>
      pivot_longer(cols = starts_with("cum_rank"), 
                   names_to = "rank_group", 
                   values_to = "cum_count") |>
      mutate(rank_group = recode(rank_group, 
                                 "cum_rank1" = "Rank 1",
                                 "cum_rank2" = "Rank 2",
                                 "cum_rank3" = "Rank 3",
                                 "cum_rank7_8" = "Rank 7-8"))
    
    # Step 3: Determine overall max for y-axis
    max_cum <- max(cumulative_grouped$cum_count, na.rm = TRUE)
    
    # Step 4: Plot
    ggplot(cumulative_grouped, aes(x = rundi, y = cum_count, color = rank_group)) +
      geom_line(size = 1.2) +
      geom_point(aes(size = rank_group)) +
      scale_size_manual(values = c("Rank 1" = 3, "Rank 2" = 2.4, "Rank 3" = 2.2, "Rank 7-8" = 2)) +
      facet_wrap(~host, nrow = 2, ncol = 4, scales = "fixed") +
      scale_y_continuous(limits = c(0, max_cum), breaks = 0:max_cum) +
      scale_color_manual(values = c("Rank 1" = "darkgreen",
                                    "Rank 2" = "orange",
                                    "Rank 3" = "blue",
                                    "Rank 7-8" = "red")) +
      labs(
        title = "Cumulative Counts: Rank 1-3, Rank 7-8 per Host",
        x = "Round (rundi)",
        y = "Cumulative Count",
        color = "Rank Group"
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_line(color = "gray80"),
        panel.grid.major.x = element_line(color = "gray80")
      )
    
  }
} 
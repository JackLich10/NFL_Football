source("set_up.R")

qb_epa_play <- pbp_db %>% 
  select(season, week, game_date, game_id, home_team, posteam, defteam, passer_player_name, 
         qb_epa, qb_dropback, cpoe, air_yards) %>% 
  filter(!is.na(passer_player_name), !is.na(qb_epa), qb_dropback == 1) %>% 
  group_by(season, week, game_date, game_id, home_team, posteam, defteam, passer_player_name) %>% 
  summarise(dropbacks = sum(qb_dropback, na.rm = T),
            air_yards = mean(air_yards, na.rm = T),
            qb_epa = mean(qb_epa, na.rm = T),
            cpoe = mean(cpoe, na.rm = T)) %>%
  ungroup() %>% 
  group_by(passer_player_name) %>% 
  mutate(last_game = max(game_date, na.rm = T)) %>% 
  ungroup() 

qb_adot <- function(data, qb_name, seasons, faceted = F) {
  if (faceted) {
    size <- 0.1
  } else {
    size <- 0.075
  }
  
  seasons_filtered <- data %>% 
    filter(season %in% seasons, dropbacks >= 10) %>% 
    mutate(pct_qb_epa = percent_rank(qb_epa),
           pct_cpoe = percent_rank(cpoe),
           pct_air_yards = percent_rank(air_yards),
           last_game = ifelse(last_game == game_date, 1, 0),
           mean_adot = mean(air_yards, na.rm = T)) %>% 
    collect()
  
  by_qb <- seasons_filtered %>% 
    filter(passer_player_name == qb_name) %>% 
    mutate(qualifier = ifelse(home_team == posteam, paste0(season, ": vs. ", defteam), 
                              paste0(season, ": @ ", defteam)),
           qualifier = fct_reorder(qualifier, season + (week * 0.001))) %>% 
    left_join(teams_colors_logos %>%
                select(team_abbr, team_logo_espn), 
              by = c("defteam" = "team_abbr")) 
  
  mean_adots <- by_qb %>% 
    arrange(qualifier) %>% 
    group_by(season) %>% 
    summarise(games = n(),
              mean_adot = mean(air_yards),
              first = first(qualifier),
              end = last(qualifier)) %>%
    ungroup()
  
  p <- by_qb %>%
    ggplot(aes(qualifier, air_yards)) +
    geom_line(aes(color = posteam), size = 1.5, alpha = 0.8, group = 1) +
    geom_segment(data = mean_adots, aes(x = first, xend = end, 
                                        y = mean_adot, yend = mean_adot),
                 lty = 2) +
    # geom_point() +
    geom_text(data = mean_adots, aes(first, 2, 
                                     label = ifelse(season == min(season), 
                                                    paste0(min(season), " season average: \n", round(mean_adot, 2)), 
                                                    paste(season, round(mean_adot, 2), sep = "\n"))),
              size = 2.5, fontface = "italic", hjust = 0) +
    scale_color_manual(values = NFL_pri) +
    expand_limits(y = 0) +
    #ggforce::facet_row(~ season, scales = "free_x", space = "free", shrink = F) +
    #facet_grid(~ season, scales = "free_x", space = "free", shrink = F) +
    ggimage::geom_image(aes(qualifier, air_yards, image = team_logo_espn),
                        asp = 1.618, by = "height", size = size, inherit.aes = F) +
    theme_bw() +
    theme(aspect.ratio = 9/16,
          legend.position = "none",
          strip.background = element_rect(color = "black", fill = "#C0C0C0", size = 3.5, linetype = "blank"),
          strip.text.x = element_text(face = "bold"),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.border = element_blank(),
          axis.ticks = element_line(color = "grey"),
          axis.text = element_text(size = 24/.pt),
          axis.title = element_text(size = 26/.pt),
          plot.title = element_text(face = "bold", size = 30/.pt, hjust = 0),
          plot.subtitle = element_text(face = "italic", size = 24/.pt, hjust = 0),
          plot.caption = element_text(face = "italic", size = 20/.pt),
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = paste0(qb_name, " Average Depth of Target"),
         subtitle = paste0("All ", sum(mean_adots$games), " career games from " , 
                           paste0(seasons[1], "-", seasons[length(seasons)]), 
                           " | Horizontal lines show averages by season"),
         x = NULL,
         y = "Average Depth of Target",
         caption = "Data from @nflfastR")
  if (faceted) {
    p + 
      facet_grid(~ season, scales = "free_x", space = "free") +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank())
  } else {
    p +
      geom_segment(data = mean_adots, aes(x = as.numeric(end) + 0.5, xend = as.numeric(end) + 0.5,
                                          y = 0, yend = 12),
                   size = 1.5)
  }
}

qb_adot(data = qb_epa_play, qb_name = "S.Darnold", seasons = 2018:2020)

qb_adot(data = qb_epa_play, qb_name = "D.Jones", seasons = 2019:2020)

qb_adot(data = qb_epa_play, qb_name = "A.Rodgers", seasons = 2015:2020, faceted = T)

dbDisconnect(connection)

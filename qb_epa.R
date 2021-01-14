#source("set_up.R")

pbp_db <- dplyr::tbl(
  DBI::dbConnect(
    connection <- DBI::dbConnect(RSQLite::SQLite(), "~/Desktop/RStudio/NFL_Football/pbp_db")),
  "nflfastR_pbp") 

DBI::dbDisconnect(connection)

xyac <- pbp_db %>% 
  filter(season == 2020, !is.na(posteam), !is.na(xyac_epa)) %>%
  group_by(posteam) %>% 
  summarise(xyac_epa = mean(xyac_epa, na.rm = T),
            xyac_mean_yardage = mean(xyac_mean_yardage, na.rm = T),
            xyac_success = mean(xyac_success, na.rm = T)) %>% 
  collect()

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

qb_pct_ranks <- function(data, qb_name, seasons, last_game_only = F) {
  seasons_filtered <- data %>% 
    filter(season %in% c(seasons), dropbacks >= 10) %>% 
    mutate(pct_qb_epa = percent_rank(qb_epa),
           pct_cpoe = percent_rank(cpoe),
           pct_air_yards = percent_rank(air_yards),
           mean_adot = mean(air_yards, na.rm = T),
           last_game = ifelse(last_game == game_date, 1, 0)) %>% 
    collect()
  
  long <- seasons_filtered %>% 
    pivot_longer(cols = c(qb_epa, cpoe),
                 names_to = "metric", values_to = "value") %>% 
    group_by(metric) %>% 
    mutate(percentile = percent_rank(value)) %>% 
    ungroup() %>% 
    mutate(
      metric = case_when(
        metric == "cpoe" ~ "Completion % Over Expected",
        metric == "qb_epa" ~ "QB Expected Points Added/Play"),
      metric = fct_rev(metric),
      qualifier = ifelse(home_team == posteam, paste0("vs. ", defteam), paste0("@ ", defteam)))
  
  if (last_game_only) {
    long <- long %>% 
      mutate(label = ifelse(last_game == 1 & passer_player_name == qb_name, 
                            qualifier,
                            ""))
  } else {
    long <- long %>% 
      mutate(label = ifelse(season == seasons[length(seasons)] & passer_player_name == qb_name, 
                            qualifier,
                            ""))
  }
  
  summarized <- long %>% 
    mutate(above_avg = ifelse(percentile > 0.5, 1, 0),
           below_30 = ifelse(percentile < 0.3, 1, 0),
           above_75 = ifelse(percentile > 0.75, 1, 0)) %>% 
    group_by(passer_player_name, metric) %>% 
    summarise(across(above_avg:above_75, ~ sum(., na.rm = T)),
              games = n()) %>% 
    ungroup() %>% 
    mutate(across(above_avg:above_75, ~ ./games))
  
  long %>% 
    ggplot(aes(percentile, value, color = posteam)) +
    geom_text(data = summarized %>% filter(passer_player_name == qb_name), 
              aes(x = 0.01, y = ifelse(metric == "QB Expected Points Added/Play", 1, 22), 
                  label = paste(paste0("Below 30th: ", scales::percent(below_30)), paste0("Above 50th: ", scales::percent(above_avg)), paste0("Above 75th: ", scales::percent(above_75)), sep = "\n")),
              size = 3, color = "black", hjust = 0, fontface = "italic") +
    geom_point(size = 2, alpha = 0.2) +
    geom_point(data = long %>% filter(passer_player_name == qb_name),
               size = 5.5, alpha = 0.7) +
    ggrepel::geom_label_repel(aes(color = defteam,
                         label = label),
                     size = 3,
                     arrow = arrow(length = unit(0.03, "npc"), type = "closed", 
                                   ends = "last"),
                     force = 2) +
    scale_x_continuous(breaks = seq(0, 1, 0.1), 
                       labels = function(.) paste0(100*., "th")) +
    #scale_y_continuous(breaks = scales::pretty_breaks()) +
    scale_color_manual(values = NFL_pri) +
    facet_wrap_custom(~ metric, scales = "free_y", nrow = 2,
                      scale_overrides = list(
                        scale_override(1, scale_y_continuous(breaks = seq(-1.25, 1.25, 0.25))),
                        scale_override(2, scale_y_continuous(breaks = seq(-30, 30, 10),
                                                             labels = function(.) paste0(., "%"))))) +
    #facet_wrap(~ metric, scales = "free_y", nrow = 2) +
    guides(color = F, size = F, alpha = F) +
    theme_bw() +
    theme(strip.background = element_rect(color = "black", fill = "#C0C0C0", size = 3.5, linetype = "blank"),
          strip.text.x = element_text(face = "bold"),
          panel.grid.minor.x = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.border = element_blank(),
          axis.ticks = element_line(color = "grey"),
          axis.text = element_text(size = 24/.pt),
          axis.title = element_text(size = 26/.pt),
          plot.title = element_text(face = "bold", size = 30/.pt, hjust = 0),
          plot.subtitle = element_text(face = "italic", size = 24/.pt, hjust = 0),
          plot.caption = element_text(face = "italic", size = 20/.pt)) +
    labs(title = paste0("All ", summarized$games[summarized$passer_player_name == qb_name], " games of ", qb_name),
         subtitle = paste0("Compared to all quarterback games from ", paste0(seasons[1], "-", seasons[length(seasons)])),
         x = "Percentile",
         y = NULL,
         shape = NULL,
         caption = "Data from @nflfastR")
}

qb_pct_ranks(data = qb_epa_play, qb_name = "D.Jones", seasons = 2019:2020, last_game_only = F)

qb_pct_ranks(data = qb_epa_play, qb_name = "S.Darnold", seasons = 2018:2020, last_game_only = F)


library(tidyverse)
library(espnscrapeR)
library(gt)

standings <- espnscrapeR::get_nfl_standings(2020)

ESPN_logo_url <- function(x) {
  ifelse(is.na(x), NA, paste0('https://a.espncdn.com/i/teamlogos/nfl/500/',x,'.png'))
}

week <- 12

jags_opp <- c("Steelers", "Ravens", "Bengals", "Browns", 
              "Packers", "Vikings", "Bears", "Lions",
              "Dolphins", "Chargers")

jags_division <- c("Titans", "Texans", "Colts")

jets_opp <- c("Chiefs", "Raiders", "Chargers", "Broncos", 
              "Seahawks", "Cardinals", "Rams", "49ers",
              "Colts", "Browns")

jets_division <- c("Patriots", "Bills", "Dolphins")

standings_long <- standings %>% 
  select(team_name, abb_name, wins, losses, ties, win_percent) %>% 
  mutate(
    Jets = case_when(
      team_name %in% jets_division ~ 2,
      team_name %in% jets_opp ~ 1,
      TRUE ~ 0),
    Jags = case_when(
      team_name %in% jags_division ~ 2,
      team_name %in% jags_opp ~ 1,
      TRUE ~ 0)) %>% 
  filter(Jets != 0 | Jags != 0, 
         Jets != Jags) %>% 
  pivot_longer(cols = c(Jets, Jags),
               names_to = "team",
               values_to = "games") %>% 
  filter(games > 0) %>% 
  uncount(games) %>%
  select(abb_name, team_name, wins, losses, ties, win_percent, team) %>% 
  arrange(desc(win_percent))

standings_long %>% 
  group_by(team) %>% 
  summarise(win_percent = sum(wins+0.5*ties)/sum(wins+losses+ties)) 

standings_wide <- standings_long %>% 
  filter(team == "Jets") %>% 
  select(-team) %>% 
  add_row(abb_name = "NYJ", team_name = "Jets", 
          wins = sum(standings_long[standings_long$team == "Jets",]$wins),
          losses = sum(standings_long[standings_long$team == "Jets",]$losses),
          ties = sum(standings_long[standings_long$team == "Jets",]$ties),
          win_percent = (wins+0.5*ties)/(wins+losses+ties)) %>% 
  rename_with(~ paste0(., "_Jets")) %>%
  bind_cols(standings_long %>% 
              filter(team == "Jags") %>% 
              select(-team) %>% 
              add_row(abb_name = "JAX", team_name = "Jaguars", 
                      wins = sum(standings_long[standings_long$team == "Jags",]$wins),
                      losses = sum(standings_long[standings_long$team == "Jags",]$losses),
                      ties = sum(standings_long[standings_long$team == "Jags",]$ties),
                      win_percent = (wins+0.5*ties)/(wins+losses+ties)) %>% 
              rename_with(~ paste0(., "_Jags"))) %>% 
  mutate(losses_Jags = ifelse(ties_Jags > 0, paste0(losses_Jags, "-", ties_Jags), losses_Jags),
         record_Jets = paste0(wins_Jets, "-", losses_Jets),
         record_Jags = paste0(wins_Jags, "-", losses_Jags)) %>% 
  select(-contains("wins"), -contains("losses"), -contains("ties")) %>% 
  relocate(record_Jets, .after = team_name_Jets) %>% 
  relocate(record_Jags, .after = team_name_Jags)

table <- standings_wide %>% 
  gt() %>% 
  tab_header(title = md("**Tankathon Strength of Schedule Tiebreaker**"),
             subtitle = md(glue::glue("*Through Week {week} (Easier SOS gets better pick)*"))) %>% 
  cols_label(abb_name_Jets = "",
             team_name_Jets = "Opponent",
             record_Jets = "Record",
             win_percent_Jets = "Win %",
             abb_name_Jags = "",
             team_name_Jags = "Opponent",
             record_Jags = "Record",
             win_percent_Jags = "Win %") %>% 
  fmt_percent(columns = contains("win_percent"),
              decimals = 1) %>% 
  tab_spanner(label = paste0("JETS (", 
                             paste0(standings[standings$abb_name == "NYJ",]$wins, "-", 
                                    standings[standings$abb_name == "NYJ",]$losses),
                             ") SCHEDULE"),
              columns = 1:4) %>% 
  tab_spanner(label = paste0("JAGUARS (", 
                             paste0(standings[standings$abb_name == "JAX",]$wins, "-", 
                                    standings[standings$abb_name == "JAX",]$losses),
                             ") SCHEDULE"),
              columns = 5:8) %>% 
  tab_style(style = list(
    cell_borders(
      sides = c("left", "right"),
      color = "black",
      weight = px(3))),
    locations = list(
      cells_body(
        columns = contains("win_percent")))) %>%
  tab_style(style = list(
    cell_borders(
      sides = "bottom",
      color = "black",
      weight = px(3))),
    locations = list(
      cells_column_labels(
        columns = gt::everything()))) %>% 
  tab_style(style = cell_borders(
    sides = "bottom", 
    color = "black", 
    weight = px(3)),
  locations = cells_body(
    columns = TRUE,
    rows = nrow(standings_wide) - 1)) %>% 
  tab_source_note("Table: @jacklich10 | Data: ESPN") %>%
  text_transform(locations = cells_body(columns = contains("abb_name")),
                 fn = function(x) web_image(url = ESPN_logo_url(x), height = 17)) %>% 
  data_color(columns = contains("win_percent"),
             colors = scales::col_numeric(
               palette = c("#ffffff", "#f2fbd2", "#c9ecb4", "#93d3ab", "#35b0ab"),
               domain = NULL)) %>% 
  espnscrapeR::gt_theme_538()

# Save it as png
gtsave(table, "~/Desktop/RStudio/NFL_Football/tankathon_table.png")


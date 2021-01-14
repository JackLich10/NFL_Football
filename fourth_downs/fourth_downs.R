library(tidyverse)
library(gt)

nfl_pbp <- dplyr::tbl(
  DBI::dbConnect(
    connection <- DBI::dbConnect(RSQLite::SQLite(), "~/Desktop/RStudio/NFL_Football/pbp_db")),
  "nflfastR_pbp") 

DBI::dbDisconnect(connection)

pbp <- nfl_pbp %>% 
  filter(season >= 2014, !is.na(down), !is.na(ep)) %>% 
  select(game_id, play_id, desc, down, ydstogo, yards_gained, posteam_score, defteam_score) %>% 
  # group_by(game_id) %>% 
  # mutate(row = row_number()) %>% 
  # ungroup() %>% 
  mutate(prev_yards_gained = lag(yards_gained),
         prev_yards_gained = ifelse(is.na(prev_yards_gained), lag(prev_yards_gained, n = 2), prev_yards_gained)) %>% 
  filter(down == 4) %>% 
  collect()

joined <- fourth_downs %>% 
  left_join(pbp,
            by = c("game_id", "desc", "ydstogo")) %>% 
  mutate(posteam_score_diff = posteam_score - defteam_score)

# How do 3rd downs affect 4th down decisions
joined %>% 
  mutate(go_boost = 1 * (go_boost %/% 1),
         go_boost = pmax(pmin(go_boost, 10), -10),
         prev_yards_gained_group = case_when(
           prev_yards_gained >= 5 ~ ">5",
           prev_yards_gained > 0 ~ "[1,5)",
           prev_yards_gained == 0 ~ "0",
           prev_yards_gained >= -5 ~ "[-5,0)",
           TRUE ~ "<-5" ),
         prev_yards_gained_group = fct_relevel(prev_yards_gained_group,
                                               "<-5", 
                                               "[-5,0)", 
                                               "0", 
                                               "[1,5)", 
                                               ">5"),
         prev_yards_gained_group2 = case_when(
           prev_yards_gained_group %in% c("<-5", "[-5,0)") ~ "Negative",
           prev_yards_gained_group %in% c("0") ~ "None",
           TRUE ~ "Positive")) %>% 
  group_by(go_boost = go_boost/100, prev_yards_gained_group2) %>% 
  summarise(n = n(),
            n_go = sum(go),
            n_no_go = n - n_go,
            go = mean(go)) %>% 
  ggplot(aes(go_boost, go, color = prev_yards_gained_group2)) +
  geom_vline(xintercept = 0, color = "red", lty = 2) +
  geom_smooth(aes(weight = n), se = F) +
  geom_point(aes(size = n)) +
  annotate(geom = "text", x = -0.075, y = 0.9,
           label = "Should Kick/Punt", size = 3, fontface = "italic") +
  annotate(geom = "text", x = 0.075, y = 0.1,
           label = "Should Go", size = 3, fontface = "italic") +
  scale_x_continuous(labels = scales::percent, limits = c(-0.1, 0.1)) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_color_brewer(palette = "Set1") +
  guides(size = F) +
  jacklich::theme_jack(aspect = F) +
  labs(title = "Coaches let 3rd down yardage gained influence their fourth down decisions",
       subtitle = "2014-2020 NFL seasons | 4th down recommendations by @ben_bot_baldwin",
       x = "Pre-Snap Change in Win Probability by Going on 4th Down",
       y = "Go Rate",
       color = "3rd Down Yards",
       caption = "Chart: @jacklich10 | Data: @nflfastR & @ben_bot_baldwin")

joined %>% 
  glm(go ~ go_boost + prev_yards_gained + qtr + posteam_score_diff + ydstogo, 
      family = "binomial",
      data = .) %>% 
  summary()

# Function to find all fourth down decisions for a given team
team_fourth_downs <- function(fourth_downs, team) {
  nfl_pbp <- dplyr::tbl(
    DBI::dbConnect(
      connection <- DBI::dbConnect(RSQLite::SQLite(), "~/Desktop/RStudio/NFL_Football/pbp_db")),
    "nflfastR_pbp") 
  
  DBI::dbDisconnect(connection)
  
  fourth_downs %>% 
    filter(home_team == team | away_team == team,
           #play_type != "PENALTY",
           season == 2020) %>%
    left_join(nfl_pbp %>% 
                filter(season == 2020, posteam == team | defteam == team) %>% 
                select(game_id, desc, posteam_score, defteam_score) %>% 
                collect(),
              by = c("game_id", "desc")) %>% 
    mutate(defteam = ifelse(posteam == home_team, away_team, home_team),
           qualifier = ifelse(posteam == home_team, "vs.", "@"),
           go_boost = go_boost/100,
           yardline_100 = ifelse(yardline_100 < 50, 
                                 paste0(defteam, " ", yardline_100), 
                                 ifelse(yardline_100 == 50, "50", paste0(posteam, " ", 100-yardline_100))),
           down = paste0("4th & ", ydstogo),
           qtr = case_when(
             qtr == 1 ~ "1st QTR",
             qtr == 2 ~ "2nd QTR",
             qtr == 3 ~ "3rd QTR",
             qtr == 4 ~ "4th QTR"),
           score = paste0(posteam_score, "-", defteam_score))
}

nyj_fourth_downs <- team_fourth_downs(fourth_downs = fourth_downs, team = "NYJ")

nyg_fourth_downs <- team_fourth_downs(fourth_downs = fourth_downs, team = "NYG")

nyj_fourth_downs %>% 
  filter(defteam == "NYJ") %>% 
  mutate(diff = ifelse(should_go != go, 1, 0),
         diff = factor(diff),
         qualifier = ifelse(qualifier == "vs.", "@", "vs.")) %>% 
  filter(diff == 1, should_go == 1) %>% 
  group_by(posteam, week, qualifier) %>% 
  summarise(decisions = n(),
            wp_lost = sum(go_boost),
            wp_lost_attempt = sum(go_boost)/n(),
            .groups = "drop") %>% 
  arrange(week) %>% 
  select(week, qualifier, posteam, decisions, wp_lost) %>% 
  gt::gt() %>%
  gt::tab_header(title = gt::md("**2020 NYJ Opponent's Anti-Tank 4th Down Decisions**"),
                 subtitle = gt::md("*Based on @ben_bot_baldwin 4th down model*")) %>% 
  gt::cols_label(week = "Week",
                 qualifier = "",
                 posteam = "",
                 decisions = "Decisions",
                 wp_lost = "Total WP Lost") %>% 
  gt::fmt_percent(columns = vars(wp_lost),
                  decimals = 1) %>% 
  gt::tab_style(style = list(
    gt::cell_borders(
      sides = "left",
      color = "black",
      weight = gt::px(3))),
    locations = list(
      gt::cells_body(
        columns = vars(decisions)))) %>%
  gt::tab_style(style = list(
    gt::cell_borders(
      sides = "bottom",
      color = "black",
      weight = gt::px(3))),
    locations = list(
      gt::cells_column_labels(
        columns = gt::everything()))) %>% 
  gt::data_color(
    columns = vars(wp_lost),
    colors = scales::col_numeric(
      palette = as.character(paletteer::paletteer_d("ggsci::red_material", n = 5)),
      domain = NULL)) %>% 
  gt::text_transform(locations = gt::cells_body(columns = vars(posteam)),
                     fn = function(x) gt::web_image(url = ESPN_logo_url(x), height = 30)) %>% 
  gt::tab_source_note("Table: @jacklich10 | Data: @nflfastR + @ben_bot_baldwin") %>%
  espnscrapeR::gt_theme_538() %>% 
  gt::cols_width(vars(wp_lost) ~ px(100),
                 vars(posteam, qualifier) ~ px(50),
                 everything() ~ px(80))
  
nyj_fourth_downs %>% 
  filter(defteam == "NYJ") %>% 
  mutate(diff = ifelse(should_go != go, 1, 0),
         diff = factor(diff)) %>% 
  # mutate(posteam = paste0(posteam, " (We"),
  #        posteam = fct_reorder(posteam, week)) %>% 
  left_join(nflfastR::teams_colors_logos %>% 
              select(team_abbr, team_logo_espn),
            by = c("posteam" = "team_abbr")) %>% 
  ggplot(aes(week, go_boost)) +
  geom_jitter(data = nyj_fourth_downs %>% 
                filter(defteam == "NYJ") %>% 
                mutate(diff = ifelse(should_go != go, 1, 0),
                       diff = factor(diff)) %>% 
                filter(diff == 0),
              size = 1.5, width = 0.05, alpha = 0.5) +
  geom_jitter(data = nyj_fourth_downs %>% 
                filter(defteam == "NYJ") %>% 
                mutate(diff = ifelse(should_go != go, 1, 0),
                       diff = factor(diff)) %>% 
                filter(diff == 1),
              aes(color = posteam, fill = posteam), 
              size = 3, shape = 21, stroke = 1, width = 0.05, show.legend = F) +
  ggimage::geom_image(aes(week, -0.075, image = team_logo_espn),
                      asp = 1.618, by = "height", size = 0.1, inherit.aes = F) +
  scale_y_continuous(labels = scales::percent,
                     limits = c(-0.075, 0.075)) +
  scale_fill_manual(values = NFL_pri) +
  scale_color_manual(values = NFL_sec) +
  theme_bw() +
  theme(aspect.ratio = 9/16,
        plot.title = element_text(face = "bold", size = 28/.pt, hjust = 0),
        plot.subtitle = element_text(face = "italic", size = 24/.pt),
        strip.background = element_rect(color = "black", fill = "#C0C0C0", size = 3.5, linetype = "blank"),
        strip.text.x = element_text(face = "bold"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_text(size = 24/.pt),
        axis.text.x = element_blank(),
        axis.title = element_text(face = "bold", size = 26/.pt),
        plot.caption = element_text(face = "italic", size = 20/.pt)) +
  labs(title = "NYJ opponent 4th down decision making",
       subtitle = "Bolded points represent situations where opponent made wrong decision based on @ben_bot_baldwin 4th down model",
       x = NULL,
       y = "% Increase in WP if Going For It",
       caption = "Chart: @jacklich10 | Data: @nflfastR + @ben_bot_baldwin")


nyj_fourth_downs %>% 
  filter(go == 0, posteam == "NYJ") %>% 
  mutate(desc = str_remove(desc, ",.*")) %>%
  arrange(-go_boost) %>%
  head(5) %>%
  select(posteam, qualifier, defteam, score,
         qtr, down, yardline_100, prior_wp, go_boost, desc) %>%
  gt::gt() %>%
  gt::tab_header(title = gt::md("**2020 NYJ Most Pro-Tank 4th Down Decisions**"),
                 subtitle = gt::md("*Based on @ben_bot_baldwin 4th down model*")) %>% 
  gt::cols_label(posteam = "",
                 qualifier = "",
                 defteam = "",
                 score = "Score",
                 prior_wp = "Pre-Snap WP",
                 qtr = "",
                 down = "Situation",
                 yardline_100 = "",
                 desc = "Play",
                 go_boost = "Go Gain WP") %>% 
  gt::fmt_percent(columns = vars(go_boost, prior_wp),
                  decimals = 1) %>% 
  gt::tab_style(style = list(
    gt::cell_borders(
      sides = "left",
      color = "black",
      weight = gt::px(3))),
    locations = list(
      gt::cells_body(
        columns = vars(desc)))) %>%
  gt::tab_style(style = list(
    gt::cell_borders(
      sides = "bottom",
      color = "black",
      weight = gt::px(3))),
    locations = list(
      gt::cells_column_labels(
        columns = gt::everything()))) %>% 
  gt::data_color(
    columns = vars(go_boost),
    colors = scales::col_numeric(
      palette = as.character(paletteer::paletteer_d("ggsci::red_material", n = 5)),
      domain = NULL)) %>% 
  gt::text_transform(locations = gt::cells_body(columns = vars(posteam, defteam)),
                     fn = function(x) gt::web_image(url = ESPN_logo_url(x), height = 30)) %>% 
  gt::tab_source_note("Table: @jacklich10 | Data: @nflfastR + @ben_bot_baldwin") %>%
  espnscrapeR::gt_theme_538()

nyj_fourth_downs %>% 
  filter(go == 1, posteam == "NYJ") %>% 
  mutate(desc = str_remove(desc, "\\(Shotgun\\)"),
         desc = str_remove(desc, " \\(.*")) %>%
  arrange(-go_boost) %>%
  head(5) %>%
  select(posteam, qualifier, defteam, score,
         qtr, down, yardline_100, prior_wp, go_boost, desc) %>%
  gt::gt() %>%
  gt::tab_header(title = gt::md("**2020 NYJ Most Anti-Tank 4th Down Decisions**"),
                 subtitle = gt::md("*Based on @ben_bot_baldwin 4th down model*")) %>% 
  gt::cols_label(posteam = "",
                 qualifier = "",
                 defteam = "",
                 score = "Score",
                 prior_wp = "Pre-Snap WP",
                 qtr = "",
                 down = "Situation",
                 yardline_100 = "",
                 desc = "Play",
                 go_boost = "Go Gain WP") %>% 
  gt::fmt_percent(columns = vars(go_boost, prior_wp),
                  decimals = 1) %>% 
  gt::tab_style(style = list(
    gt::cell_borders(
      sides = "left",
      color = "black",
      weight = gt::px(3))),
    locations = list(
      gt::cells_body(
        columns = vars(desc)))) %>%
  gt::tab_style(style = list(
    gt::cell_borders(
      sides = "bottom",
      color = "black",
      weight = gt::px(3))),
    locations = list(
      gt::cells_column_labels(
        columns = gt::everything()))) %>% 
  gt::data_color(
    columns = vars(go_boost),
    colors = scales::col_numeric(
      palette = as.character(paletteer::paletteer_d("ggsci::blue_material", n = 5)),
      domain = NULL)) %>% 
  gt::text_transform(locations = gt::cells_body(columns = vars(posteam, defteam)),
                     fn = function(x) gt::web_image(url = ESPN_logo_url(x), height = 30)) %>% 
  gt::tab_source_note("Table: @jacklich10 | Data: @nflfastR + @ben_bot_baldwin") %>%
  espnscrapeR::gt_theme_538()

nyj_fourth_downs %>% 
  filter(go == 0, defteam == "NYJ") %>% 
  mutate(desc = str_remove(desc, ",.*")) %>%
  arrange(-go_boost) %>%
  head(10) %>%
  select(posteam, qualifier, defteam, score,
         qtr, down, yardline_100, prior_wp, go_boost, desc) %>%
  gt::gt() %>%
  gt::tab_header(title = gt::md("**2020 NYJ Opponents Most Anti-Tank 4th Down Decisions**"),
                 subtitle = gt::md("*Based on @ben_bot_baldwin 4th down model*")) %>% 
  gt::cols_label(posteam = "",
                 qualifier = "",
                 defteam = "",
                 score = "Score",
                 prior_wp = "Pre-Snap WP",
                 qtr = "",
                 down = "Situation",
                 yardline_100 = "",
                 desc = "Play",
                 go_boost = "Go Gain WP") %>% 
  gt::fmt_percent(columns = vars(go_boost, prior_wp),
                  decimals = 1) %>% 
  gt::tab_style(style = list(
    gt::cell_borders(
      sides = "left",
      color = "black",
      weight = gt::px(3))),
    locations = list(
      gt::cells_body(
        columns = vars(desc)))) %>%
  gt::tab_style(style = list(
    gt::cell_borders(
      sides = "bottom",
      color = "black",
      weight = gt::px(3))),
    locations = list(
      gt::cells_column_labels(
        columns = gt::everything()))) %>% 
  gt::data_color(
    columns = vars(go_boost),
    colors = scales::col_numeric(
      palette = as.character(paletteer::paletteer_d("ggsci::red_material", n = 5)),
      domain = NULL)) %>% 
  gt::text_transform(locations = gt::cells_body(columns = vars(posteam, defteam)),
                     fn = function(x) gt::web_image(url = ESPN_logo_url(x), height = 30)) %>% 
  gt::tab_source_note("Table: @jacklich10 | Data: @nflfastR + @ben_bot_baldwin") %>%
  espnscrapeR::gt_theme_538()

nyg_fourth_downs %>%
  filter(go == 0, posteam == "NYG") %>% 
  mutate(desc = str_remove(desc, "\\(Punt formation\\)"),
         desc = str_remove(desc, "\\(Run formation\\)")) %>% 
  arrange(-go_boost) %>%
  head(10) %>%
  select(posteam, qualifier, defteam, score,
         qtr, down, yardline_100, prior_wp, go_boost, desc) %>%
  gt::gt() %>%
  gt::tab_header(title = gt::md("**2020 NYG Worst 4th Down Decisions**"),
                 subtitle = gt::md("*Based on @ben_bot_baldwin 4th down model*")) %>% 
  gt::cols_label(posteam = "",
                 qualifier = "",
                 defteam = "",
                 score = "Score",
                 prior_wp = "Pre-Snap WP",
                 qtr = "",
                 down = "Situation",
                 yardline_100 = "",
                 desc = "Play",
                 go_boost = "Go Gain WP") %>% 
  gt::fmt_percent(columns = vars(go_boost, prior_wp),
                  decimals = 1) %>% 
  gt::tab_style(style = list(
    gt::cell_borders(
      sides = "left",
      color = "black",
      weight = gt::px(3))),
    locations = list(
      gt::cells_body(
        columns = vars(desc)))) %>%
  gt::tab_style(style = list(
    gt::cell_borders(
      sides = "bottom",
      color = "black",
      weight = gt::px(3))),
    locations = list(
      gt::cells_column_labels(
        columns = gt::everything()))) %>% 
  gt::data_color(
    columns = vars(go_boost),
    colors = scales::col_numeric(
      palette = as.character(paletteer::paletteer_d("ggsci::red_material", n = 5)),
      domain = NULL)) %>% 
  gt::text_transform(locations = gt::cells_body(columns = vars(posteam, defteam)),
                     fn = function(x) gt::web_image(url = ESPN_logo_url(x), height = 30)) %>% 
  gt::tab_source_note("Table: @jacklich10 | Data: @nflfastR + @ben_bot_baldwin") %>%
  espnscrapeR::gt_theme_538()

delays <- fourth_downs %>% 
  filter(play_type == "PENALTY",
         str_detect(desc, "Delay of Game"),
         should_go == 1,
         season == 2020) %>% 
  left_join(games %>% 
              select(game_id, contains("coach")),
            by = c("game_id")) %>% 
  left_join(nflfastR::teams_colors_logos %>% 
              select(team_abbr, team_logo_espn),
            by = c("posteam" = "team_abbr")) %>% 
  mutate(coach = ifelse(posteam == home_team, home_coach, away_coach))


delays %>% 
  filter(!str_detect(desc, "Punt formation|Field Goal formation")) %>%
  group_by(posteam, coach, team_logo_espn) %>% 
  summarise(delays = n(),
            wp_lost = sum(go_boost)/100,
            .groups = "drop") %>% 
  filter(wp_lost >= 0.01) %>%
  mutate(coach = paste0(str_sub(coach, start = 1, end = 1), ". ", str_extract(coach, '[^ ]+$')),
         posteam = fct_reorder(posteam, wp_lost),
         label = paste0(scales::percent(wp_lost, accuracy = 0.1), " on ", delays, " att.")) %>%
  ggplot(aes(wp_lost, posteam)) +
  geom_col(aes(color = posteam, fill = posteam), 
           show.legend = F) +
  geom_text(aes(label = coach),
            hjust = 1.05, color = "white", size = 3, fontface = "bold") +
  geom_text(aes(label = label),
            hjust = -0.05, color = "black", size = 3, fontface = "bold") +
  ggimage::geom_image(aes(0.0025, posteam, image = team_logo_espn),
                      asp = 1.618, by = "height", size = 0.075, inherit.aes = F) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
  scale_fill_manual(values = NFL_pri) +
  scale_color_manual(values = NFL_sec) +
  theme_bw() +
  theme(aspect.ratio = 9/16,
        plot.title = element_text(face = "bold", size = 28/.pt, hjust = 0),
        plot.subtitle = element_text(face = "italic", size = 24/.pt),
        strip.background = element_rect(color = "black", fill = "#C0C0C0", size = 3.5, linetype = "blank"),
        strip.text.x = element_text(face = "bold"),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_text(face = "bold", size = 26/.pt),
        plot.caption = element_text(face = "italic", size = 20/.pt)) +
  labs(title = "Which coaches take 'Delay of Game' penalties in situations where they should have gone for it?",
       subtitle = "2020 Season | Should go-for-it recommendations based on @ben_bot_baldwin 4th down model",
       x = "Expected Win Probability Lost on 'Delay of Game' Penalties",
       y = NULL,
       caption = "Chart: @jacklich10 | Data: @nflfastR + @ben_bot_baldwin")

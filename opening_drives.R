source("set_up.R")

pbp_2020 <- pbp_db %>% 
  filter(season == 2020) %>% 
  collect() %>% 
  add_xpass()


pbp_2020 %>% 
  filter(pass == 1) %>% 
  select(desc, first_down, xyac_fd) %>% view()

pbp_2020 %>% 
  filter(!is.na(xpass)) %>% 
  select(posteam, pass, rush, xpass, epa) %>% 
  pivot_longer(cols = c(pass, rush),
               names_to = "type",
               values_to = "value") %>%
  filter(value == 1) %>%
  mutate(xpass_bin = .1 * (xpass %/% .1)) %>% 
  ggplot(aes(xpass_bin, epa)) +
  geom_smooth(method = "lm") +
  geom_boxplot(aes(group = xpass_bin), alpha = 0.3) +
  scale_x_continuous(breaks = seq(0, 1, 0.1),
                     labels = scales::percent) +
  facet_wrap(~ type)
    

first_poss_half <- pbp_2020 %>% 
  filter(!is.na(posteam), special == 0) %>% 
  group_by(game_id, game_half, posteam) %>% 
  summarise(first_drive = min(drive_play_id_started)) %>% 
  ungroup() %>% 
  filter(game_half != "Overtime")

first_poss_half %>% count(posteam) %>% view()

pbp_2020 %>% 
  semi_join(first_poss_half, 
            by = c("game_id", "game_half", "posteam", "drive_play_id_started" = "first_drive")) %>% 
  distinct(game_id, game_half, posteam, drive_play_id_started) %>% 
  count(posteam) %>% view()

first_drives <- pbp_2020 %>% 
  semi_join(first_poss_half, 
            by = c("game_id", "game_half", "posteam", "drive_play_id_started" = "first_drive")) %>% 
  group_by(game_id, game_half, posteam, drive_play_id_started) %>%
  summarise(drives = n_distinct(drive_play_id_started, na.rm = T),
            plays = max(drive_play_count, na.rm = T),
            first_downs = max(drive_first_downs, na.rm = T),
            td = max(series_result == "Touchdown", na.rm = T),
            fg = max(series_result == "Field goal", na.rm = T),
            epa = sum(epa, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(game_half, posteam) %>% 
  summarise(across(c(drives, plays, first_downs, td, fg, epa), sum)) %>% 
  #select(game_half, posteam, epa) %>% 
  pivot_wider(names_from = "game_half", values_from = c(drives, plays, first_downs, td, fg, epa), 
              names_sep = "_") %>%
  left_join(teams_colors_logos %>%
              select(team_abbr, team_logo_espn, team_color, team_color2), 
            by = c("posteam" = "team_abbr"))

first_drives %>% 
  ggplot(aes(epa_Half1/plays_Half1, epa_Half2/plays_Half2)) +
  geom_hline(yintercept = mean(first_drives$epa_Half1/first_drives$plays_Half1), lty = 2, color = "red") +
  geom_vline(xintercept = mean(first_drives$epa_Half2/first_drives$plays_Half2), lty = 2, color = "red") +
  ggimage::geom_image(aes(image = team_logo_espn),
                      asp = 1.618, by = "height", size = 0.075) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  scale_x_continuous(breaks = scales::pretty_breaks()) +
  theme_bw() +
  theme(aspect.ratio = 9/16,
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
        plot.caption = element_text(face = "italic", size = 20/.pt)) +
  labs(title = "Expected Points Added on Opening Drives of Halves",
       subtitle = paste0("Through ", Sys.Date(), " | Red lines denote league averages"),
       x = "EPA/Play 1st Drive of Game",
       y = "EPA/Play 1st Drive of 2nd Half",
       caption = "Data from @nflfastR")

dbDisconnect(connection)

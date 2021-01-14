source("set_up.R")

early_downs <- pbp_db %>% 
  filter(season == 2020, !is.na(posteam), between(wp, 0.1, 0.9),
         down < 3, half_seconds_remaining > 120) %>%
  group_by(posteam) %>% 
  summarise(rush = sum(rush, na.rm = T),
            pass = sum(pass, na.rm = T)) %>% 
  mutate(total_plays = rush + pass,
         pass_pct = pass/total_plays) %>% 
  collect() %>% 
  left_join(teams_colors_logos %>%
              select(team_abbr, team_logo_espn), 
            by = c("posteam" = "team_abbr"))

early_downs %>% 
  mutate(posteam = fct_reorder(posteam, pass_pct)) %>% 
  ggplot(aes(pass_pct, posteam, color = posteam, fill = posteam)) +
  geom_col(width = 0.75) +
  geom_text(aes(x = 0.01, label = ifelse(pass_pct == max(pass_pct), 
                                         paste0("# of qualifying plays: ", pass+rush), 
                                         pass+rush)),
            hjust = 0, color = "white", fontface = "bold", size = 2.5) +
  geom_text(aes(label = scales::percent(pass_pct, accuracy = 0.1)),
            hjust = 1.5, color = "white", fontface = "bold", size = 2.5) +
  ggimage::geom_image(aes(pass_pct, posteam, image = team_logo_espn),
                      asp = 1.618, by = "height", size = 0.05, inherit.aes = F) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.1))) +
  scale_fill_manual(values = NFL_pri) +
  scale_color_manual(values = NFL_sec) +
  theme_bw() +
  theme(aspect.ratio = 9/16,
        legend.position = "none",
        strip.background = element_rect(color = "black", fill = "#C0C0C0", size = 3.5, linetype = "blank"),
        strip.text.x = element_text(face = "bold"),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_text(size = 26/.pt),
        plot.title = element_text(face = "bold", size = 30/.pt, hjust = 0),
        plot.subtitle = element_text(face = "italic", size = 24/.pt, hjust = 0),
        plot.caption = element_text(face = "italic", size = 20/.pt)) +
  labs(title = "Early Down Pass Rate in Game Neutral Situations Through Week 2",
       subtitle = "1st and 2nd Down | Win prob. between 10% and 90% | Last 2 minutes of halves excluded",
       x = NULL,
       y = NULL,
       caption = "Data via @nflfastR")

epa_distribution <- pbp_2020 %>% 
  filter(season == 2020, down < 3, special == 0, 
         !is.na(posteam), !is.na(epa)) %>% 
  group_by(posteam) %>% 
  mutate(epa_mean = mean(epa, na.rm = T),
         epa_25 = quantile(epa, 0.25),
         epa_75 = quantile(epa, 0.75),
         dropback = sum(qb_dropback == 1),
         designed_run = sum(qb_dropback == 0),
         dropback_pct = dropback/(dropback + designed_run)) %>% 
  ungroup() %>% 
  filter(posteam %in% c("NYJ", "MIA", "NE", "BUF")) %>% 
  left_join(teams_colors_logos %>%
              select(team_abbr, team_logo_espn, team_color, team_color2), 
            by = c("posteam" = "team_abbr"))

epa_distribution %>% 
  ggplot(aes(epa)) +
  ggimage::geom_image(aes(-5, 0.5, image = team_logo_espn),
                      asp = 1.618, by = "height", size = 0.5, inherit.aes = F) +
  geom_text(aes(-6, 0.35, label = paste0("Early Down Pass Rate: ", scales::percent(dropback_pct))),
            hjust = 0, fontface = "italic", size = 3) +
  geom_histogram(aes(y = ..density.., fill = posteam)) +
  geom_density(aes(fill = posteam), alpha = 0.2) +
  geom_vline(aes(xintercept = epa_mean, color = posteam), size = 1.5) +
  geom_vline(aes(xintercept = epa_25, color = posteam), size = 1, lty = 2) +
  geom_vline(aes(xintercept = epa_75, color = posteam), size = 1, lty = 2) +
  scale_fill_manual(values = NFL_pri) +
  scale_color_manual(values = NFL_sec) +
  facet_wrap(~ posteam, nrow = 2) +
  theme_bw() +
  theme(legend.position = "none",
        aspect.ratio = 9/16,
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
  labs(title = "Distribution of early down offensive EPA/Play",
       subtitle = "2020 season | 1st and 2nd down only | Vertical lines denote 25th, 50th, 75th percentiles",
       x = "Offensive EPA/Play",
       y = NULL)


dbDisconnect(connection)

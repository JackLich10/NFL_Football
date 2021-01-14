
# Initialize teams
teams <- c("ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE", "DAL", "DEN",
           "DET", "GB", "HOU", "IND", "JAX", "KC", "LA", "LAC", "LV", "MIA",
           "MIN", "NE", "NO", "NYG", "NYJ", "PHI", "PIT", "SEA", "SF", "TB", 
           "TEN", "WAS")

# Initialize divisions
afc_east <- c("NE", "NYJ", "MIA", "BUF")
afc_west <- c("DEN", "KC", "LV", "LAC")
afc_north <- c("BAL", "PIT", "CIN", "CLE")
afc_south <- c("TEN", "IND", "HOU", "JAX")

nfc_east <- c("WAS", "NYG", "PHI", "DAL")
nfc_west <- c("SEA", "ARI", "LA", "SF")
nfc_north <- c("GB", "MIN", "CHI", "DET")
nfc_south <- c("NO", "TB", "ATL", "CAR")

# Add in divisions and conferences
teams_colors_logos <- nflfastR::teams_colors_logos %>% 
  dplyr::as_tibble() %>% 
  dplyr::mutate(
    division = dplyr::case_when(
      team_abbr %in% afc_east ~ "AFC EAST",
      team_abbr %in% afc_west ~ "AFC WEST",
      team_abbr %in% afc_north ~ "AFC NORTH",
      team_abbr %in% afc_south ~ "AFC SOUTH",
      team_abbr %in% nfc_east ~ "NFC EAST",
      team_abbr %in% nfc_west ~ "NFC WEST",
      team_abbr %in% nfc_north ~ "NFC NORTH",
      team_abbr %in% nfc_south ~ "NFC SOUTH"),
    conference = ifelse(stringr::str_detect(division, "AFC"), "AFC", "NFC"))

# Return a team's conference
get_conf <- function(team) {
  conf <- teams_colors_logos$conference[teams_colors_logos$team_abbr == team]
  return(conf)
}
# Return a team's division
get_div <- function(team) {
  div <- teams_colors_logos$division[teams_colors_logos$team_abbr == team]
  return(div)
}

# Compute weights for pre-season prior
prior_weight <- function(team_abbr) {
  last_game <- game_results %>% 
    dplyr::filter(team == team_abbr, !is.na(score_diff)) %>%
    dplyr::pull(game_number) %>%
    max()
  
  w <- 1.95 * max(c(0, last_game), na.rm = T)/
    (max(c(1, 16), na.rm = T))
  
  w <- min(c(w, 1))
  return(w)
}

# Function to weight epa based on current win probability
weight_epa_wp <- function(win_prob) {
  return(1/4*sin(2*pi*(win_prob-0.25))+1)
}

# Return a team's ESPN logo
ESPN_logo_url <- function(x) {
  ifelse(is.na(x), NA, paste0('https://a.espncdn.com/i/teamlogos/nfl/500/',x,'.png'))
}

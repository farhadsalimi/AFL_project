# make a clean environment
rm(list=ls(all=TRUE))

# load packages----
library(tidyverse)



# import datsets ----
results <- read_csv("afl_match_results.csv")
player_stats <- read_csv("afl_player_stats.csv")
odds <- read_csv("afl_odds.csv")



# wrangle results dataset----
results <- 
  results %>%
  # filter for regular season (as advised in the email)
  filter(Round.Type == "Regular") %>%
  # lowercase all column names
  rename_all(tolower) 

# make a dataset for home teams
results_home <- 
  results %>%
  #select variables of interest
  select(date, home.team:away.points, margin, season) %>%
  #rename appropriately
  rename(
    team = home.team,
    scored.points = home.points,
    scored.goals = home.goals,
    scored.behinds = home.behinds,
    received.points = away.points,
    received.goals = away.goals,
    received.behinds = away.behinds,
    opposition = away.team
    ) %>%
  # mutate status and result variables
  mutate(
    status = "Home",
    result = ifelse(margin < 0, "l", ifelse(margin == 0, "d", "w"))
  )

# make a datset for away teams
results_away <- 
  results %>%
  select(date, home.team:away.points, margin, season) %>%
  rename(
    team = away.team,
    scored.points = away.points,
    scored.goals = away.goals,
    scored.behinds = away.behinds,
    received.points = home.points,
    received.goals = home.goals,
    received.behinds = home.behinds,
    opposition = home.team
  ) %>%
  mutate(
    status = "Away",
    margin = -margin,
    result = ifelse(margin < 0, "l", ifelse(margin == 0, "d", "w"))
  )

# bind home and away datasets
results_long <- bind_rows(results_away, results_home)

# make a dataset showing the proportion of win for each team, opposition, status
win_prop <- 
  results_long %>%
  # group by team, opposition, status and result
  group_by(team, opposition, status, result) %>%
  # find number of each event
  summarise(n = n()) %>%
  # find the proportion of each event
  mutate(win_prop = n / sum(n)) %>%
  # filter fot wins
  filter(result == "w") %>%
  # remove unwanted columns
  select(-result, -n)

# wrangle player stats dataset----
player_stats <-
  player_stats %>%
  # remove columns with advanced stats due to high missing rate (as advised in the email)
  select(-CCL:-T5) 

team_stats <- 
  player_stats %>% 
  # group by team, season, opposition, and status
  group_by(Date, Team, Season, Opposition, Status) %>% 
  # calculate the average team level stats for each season, opposition, and venue
  summarise_at(vars(GA:SC), mean, na.rm = TRUE) %>% 
  # ungroup
  ungroup() %>%
  # lowercase all column names
  rename_all(tolower)


# wrangle odds dataset----
odds <- 
  odds %>% 
  # separate match details into three sections by "/"
  separate(match_details, into = c("sec_1", "sec_2", "sec_3"), sep = "/", fill = "left") %>%
  # remove section1 and2
  select(-sec_1:-sec_2) %>%
  # separate section 2 by "v" to get team names
  separate(sec_3, into = c("team1", "team2"), sep = "v")

# harmonising team1, team2, and team_name codings 
odds <-
  odds %>%
  # cound the number of words in the team_name column
  mutate(
    team_name_word_count = str_count(team_name, "\\S+")
    ) %>%
  # if there are more than 1 word then remove the last word (e.g. remove 'Lions' in 'Brisbane Lions')
  mutate(
    team_name = if_else(team_name_word_count > 1, word(team_name, 1, -2), team_name)
  ) %>%
  # remove white spaces from the start and end of 'team_name', 'team_1' and 'team_2'
  mutate(
    team_name = str_trim(team_name),
    team1 = str_trim(team1),
    team2 = str_trim(team2)) %>%
  # some extra recoding to make sure that all names are right and the same
  mutate(
    team_name = recode(
      team_name, 
      "St" = "St Kilda",
      "Port" = "Port Adelaide",
      "Greater Western Sydney" = "GWS",
      "Greater Western" = "GWS",
      "Wetsern" = "Western",
      "North" = "North Melbourne")) %>%
  mutate(
    team1 = recode(
      team1,
      "Greater Western Sydney" = "GWS",
      "Western Bulldogs" = "Western",
      "P Adelaide" = "Port Adelaide")
    ) %>%
  mutate(
    team2 = recode(
      team2,
      "Greater Western Sydney" = "GWS",
      "Western Bulldogs" = "Western",
      )) %>%
  #"remove that count of team_name"
  select(-team_name_word_count)

# opposition team
odds <-
  odds %>%
  # if team_name is team1 then opposition is team2  and if team_name is team2 then opposition is team1
  mutate(
    opposition = if_else (team_name == team1, team2, team1)
  ) %>%
  #"remove team1 and team2"
  select(-team1:-team2) %>%
  # remove AFL from the season variable
  separate(season, into = c("s1", "season")) %>%
  # remove "s1"
  select(-s1) %>%
  #rename team_name to team
  rename(team = team_name)

# make season numeric
odds <- 
  odds %>%
  mutate(season = as.numeric(season))


  
  


# make an aggregated dataset for the modelling ----
# you need to add the last match performance and last match results, performance and odds

just_results <- 
  results_long %>%
  # select the variables of interest
  select(date, season, team, opposition, status, result) %>%
  # group by team, opposition and status(home or away)
  group_by(team, opposition, status) %>%
  # arrange by date
  arrange(date) %>%
  # make a value showing the match rank
  mutate(rank = row_number()) %>%
  # ungroup
  ungroup()

# be careful not to put the same match performance!!!!
# make a dataframe of last match results
last_results <- 
  results_long %>%
  group_by(team, opposition, status) %>%
  arrange(date) %>%
  mutate(rank = row_number() + 1) %>%
  select(-season) %>%
  rename(
    last_date = date,
    last_result = result)

# make a datfarme with results of each match and last match results
main_df <- 
  just_results %>% 
  left_join(last_results, by = c("team", "opposition", "status", "rank")) %>% 
  select(-season)

# join performance of last match
main_df <- main_df %>%
  left_join(team_stats, by = c("team", "opposition", "status", "last_date" = "date")) %>%
  select(-season)

# join odds
main_df <- main_df %>% left_join(odds)

# join the win_prop
main_df <- 
  main_df %>%
  left_join(win_prop)

# save the main_df
write_rds(main_df, path = "afl_main.rds")

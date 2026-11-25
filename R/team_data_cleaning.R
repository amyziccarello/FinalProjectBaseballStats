install.packages(c("baseballr", "tidyverse", "lubridate", "pROC"))

library(baseballr)
library(tidyverse)
library(lubridate)
library(pROC)

# Retrieve all five seasons
schedule_2021<-mlb_schedule(season = 2021)
schedule_2022<-mlb_schedule(season = 2022)
schedule_2023<-mlb_schedule(season = 2023)
schedule_2024<-mlb_schedule(season = 2024)
schedule_2025<-mlb_schedule(season = 2025)

# Combine into one dataset
schedule_raw <- bind_rows(schedule_2021,schedule_2022,schedule_2023,schedule_2024,schedule_2025)

#filter out spring training, post seaon, cancelled games
'''
games <- schedule_raw %>%  filter(
    game_type == "R", status_abstract_game_state == "Final") %>%
  transmute(game_pk,date = as.Date(official_date),season = as.integer(season),
            home_team = teams_home_team_name, away_team = teams_away_team_name,
            home_score = teams_home_score,  away_score = teams_away_score,
            home_win = as.integer(teams_home_is_winner), away_win = as.integer(teams_away_is_winner),
            venue = venue_name)
'''
games <- schedule_raw %>% filter(game_type == "R", status_abstract_game_state == "Final") %>% transmute(
    game_pk,date = as.Date(official_date), season = as.integer(season),
    home_team = teams_home_team_name, away_team = teams_away_team_name,
    home_score = teams_home_score, away_score = teams_away_score,
    home_win = as.integer(teams_home_is_winner), away_win = as.integer(teams_away_is_winner),
    venue = venue_name) %>% filter(!is.na(home_score),
    !is.na(away_score), !is.na(home_win),!is.na(away_win)) %>% distinct(game_pk, .keep_all = TRUE)
#check duplicates
games %>% count(game_pk) %>% filter(n > 1)
colSums(is.na(games))
games %>% count(season)

#format so its two row per game so each row is one observation for either home or away
home_games <- games %>% transmute(game_pk, date, season,
    team = home_team, opponent = away_team,
    is_home = 1, runs_scored = home_score,
    runs_allowed = away_score, win = home_win, venue)

away_games <- games %>% transmute(game_pk, date,season,
    team = away_team, opponent = home_team,
    is_home = 0, runs_scored = away_score,
    runs_allowed = home_score, win = away_win, venue)

# Combine both datasets
team_games <- bind_rows(home_games, away_games) %>% arrange(date, game_pk, team)


#historic data prior to game
team_games <- team_games %>% group_by(team) %>% arrange(date, game_pk, .by_group = TRUE) %>% mutate(
    games_played_prior = row_number() - 1, 
    team_win_pct_prior = lag(cummean(win)), 
    team_runs_avg_prior = lag(cummean(runs_scored)), 
    team_runs_allowed_avg_prior = lag(cummean(runs_allowed))) %>% ungroup()

#opponent priors stats
opponent_stats <- team_games %>%  select(game_pk,team, opp_win_pct_prior = team_win_pct_prior,
    opp_runs_avg_prior = team_runs_avg_prior, opp_runs_allowed_avg_prior = team_runs_allowed_avg_prior)

''' 
#this line causes errors with the join?
#team_games <- team_games %>% left_join(opponent_stats, by = c("game_pk", "opponent" = "team"))

# Check for duplicate game/team combinations
team_games %>% count(game_pk, team) %>% filter(n > 1)
# Check for duplicate game/opponent combinations
team_games %>% count(game_pk, opponent) %>% filter(n > 1)
# Check opponent_stats specifically
opponent_stats %>% count(game_pk, team) %>% filter(n > 1)
#all return 492 duplicate rows

games %>%  count(game_pk) %>% filter(n > 1) %>% arrange(desc(n))
games %>% filter(game_pk %in% (games %>% count(game_pk) %>% filter(n > 1) %>% pull(game_pk))) %>% arrange(game_pk) %>% head(20)
schedule_raw %>% count(game_type)
'''
#retry join
team_games <- team_games %>%  left_join(opponent_stats, by = c("game_pk", "opponent" = "team"), relationship = "many-to-one")

'''
#checks on removed data
team_games %>%count(game_pk) %>% count(n)
team_games %>% summarise(missing_team_win_pct = sum(is.na(team_win_pct_prior)),
    missing_opp_win_pct = sum(is.na(opp_win_pct_prior)),
    missing_team_runs = sum(is.na(team_runs_avg_prior)),
    missing_opp_runs = sum(is.na(opp_runs_avg_prior)))
glimpse(team_games)
head(team_games, 10)
team_games %>%filter(is.na(team_win_pct_prior)) %>% count(team, sort = TRUE)
team_games %>%  filter(is.na(team_win_pct_prior)) %>% select(date, team, opponent, season) %>% arrange(team, date)
'''

model_data <- team_games %>% drop_na(team_win_pct_prior, opp_win_pct_prior,
    team_runs_avg_prior, team_runs_allowed_avg_prior, opp_runs_avg_prior)
train_data <- model_data %>%  filter(season <= 2024)
test_data <- model_data %>% filter(season == 2025)

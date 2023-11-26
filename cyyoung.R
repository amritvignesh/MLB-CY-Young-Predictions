library(dplyr)
library(baseballr)
library(rvest)
library(tidyverse)
library(gt)
library(vip)

pitcher_stats_2023 <- bref_daily_pitcher("2023-01-01", "2023-12-31")

pitcher_stats_train = data.frame()
for (year in 2008:2022) {
  start_date <- paste0(year, "-01-01")
  end_date <- paste0(year, "-12-31")
  add_train <- bref_daily_pitcher(start_date, end_date)
  pitcher_stats_train <- rbind(pitcher_stats_train, add_train)
}

pitcher_stats_train <- pitcher_stats_train %>%
  mutate(team_games = ifelse(season == 2020, 60, 162)) %>%
  filter(IP/team_games >= 1.0)

both_leagues <- pitcher_stats_train %>%
  filter(Level == "Maj-AL,Maj-NL")

both_leagues$date[both_leagues$bbref_id == "sabatc.01"] <- "2008-07-02"
both_leagues$date[both_leagues$bbref_id == "blantjo01"] <- "2008-07-09"
both_leagues$date[both_leagues$bbref_id == "hernali01"] <- "2008-07-30"
both_leagues$date[both_leagues$bbref_id == "leecl02"] <- "2009-07-26"
both_leagues$date[both_leagues$bbref_id == "pennybr01"] <- "2009-08-21"
both_leagues$date[both_leagues$bbref_id == "harenda01"] <- "2010-07-21"
both_leagues$date[both_leagues$bbref_id == "jacksed01" & both_leagues$season == 2010] <- "2010-07-28"
both_leagues$date[both_leagues$bbref_id == "saundjo01" & both_leagues$season == 2010] <- "2010-07-23"
both_leagues$date[both_leagues$bbref_id == "westbja01"] <- "2010-07-26"
both_leagues$date[both_leagues$bbref_id == "jacksed01" & both_leagues$season == 2011] <- "2011-07-24"
both_leagues$date[both_leagues$bbref_id == "jimenub01"] <- "2011-07-30"
both_leagues$date[both_leagues$bbref_id == "greinza01" & both_leagues$season == 2012] <- "2012-07-24"
both_leagues$date[both_leagues$bbref_id == "sanchan01"] <- "2012-07-22"
both_leagues$date[both_leagues$bbref_id == "guthrje01"] <- "2012-07-18"
both_leagues$date[both_leagues$bbref_id == "saundjo01" & both_leagues$season == 2012] <- "2012-08-20"
both_leagues$date[both_leagues$bbref_id == "dempsry01"] <- "2012-07-25"
both_leagues$date[both_leagues$bbref_id == "beckejo02"] <- "2012-08-19"
both_leagues$date[both_leagues$bbref_id == "feldmsc01"] <- "2013-06-26"
both_leagues$date[both_leagues$bbref_id == "samarje01"] <- "2014-06-28"
both_leagues$date[both_leagues$bbref_id == "peavyja01"] <- "2014-07-22"
both_leagues$date[both_leagues$bbref_id == "mccarbr01"] <- "2014-07-03"
both_leagues$date[both_leagues$bbref_id == "lackejo01"] <- "2014-07-26"
both_leagues$date[both_leagues$bbref_id == "cosarja01"] <- "2014-07-26"
both_leagues$date[both_leagues$bbref_id == "hammeja01"] <- "2014-07-04"
both_leagues$date[both_leagues$bbref_id == "hamelco01" & both_leagues$season == 2015] <- "2015-07-25"
both_leagues$date[both_leagues$bbref_id == "cuetojo01"] <- "2015-07-25"
both_leagues$date[both_leagues$bbref_id == "fiersmi01"] <- "2015-07-29"
both_leagues$date[both_leagues$bbref_id == "happja01"] <- "2015-07-30"
both_leagues$date[both_leagues$bbref_id == "moorema02"] <- "2016-07-27"
both_leagues$date[both_leagues$bbref_id == "shielja02"] <- "2016-05-31"
both_leagues$date[both_leagues$bbref_id == "pomerdr01"] <- "2016-07-07"
both_leagues$date[both_leagues$bbref_id == "liriafr01"] <- "2016-07-31"
both_leagues$date[both_leagues$bbref_id == "novaiv01"] <- "2016-07-29"
both_leagues$date[both_leagues$bbref_id == "quintjo01"] <- "2017-07-08"
both_leagues$date[both_leagues$bbref_id == "darviyu01"] <- "2017-07-26"
both_leagues$date[both_leagues$bbref_id == "leakemi01" & both_leagues$season == 2017] <- "2017-08-26"
both_leagues$date[both_leagues$bbref_id == "hellije01"] <- "2017-07-22"
both_leagues$date[both_leagues$bbref_id == "hamelco01" & both_leagues$season == 2018] <- "2018-07-23"
both_leagues$date[both_leagues$bbref_id == "gausmke01"] <- "2018-07-28"
both_leagues$date[both_leagues$bbref_id == "bauertr01"] <- "2019-07-28"
both_leagues$date[both_leagues$bbref_id == "greinza01" & both_leagues$season == 2019] <- "2019-07-31"
both_leagues$date[both_leagues$bbref_id == "leakemi01" & both_leagues$season == 2019] <- "2019-07-30"
both_leagues$date[both_leagues$bbref_id == "stromma01"] <- "2019-07-24"
both_leagues$date[both_leagues$bbref_id == "roarkta01"] <- "2019-07-30"
both_leagues$date[both_leagues$bbref_id == "gibsoky01"] <- "2021-07-24"
both_leagues$date[both_leagues$bbref_id == "anderty01"] <- "2021-07-20"
both_leagues$date[both_leagues$bbref_id == "montgjo01"] <- "2022-07-31"

split_leagues = data.frame()
for (i in 1:nrow(both_leagues)) {
  Sys.sleep(3)
  id = both_leagues[["bbref_id"]][i]
  season = both_leagues[["season"]][i]
  start_date = paste0(season, "-01-01")
  end_date = paste0(season, "-12-31")
  change = both_leagues[["date"]][i]
  before <- bref_daily_pitcher(start_date, change) %>%
    filter(bbref_id == id)
  after <- bref_daily_pitcher(format((as.Date(change) + 1), "%Y-%m-%d"), end_date) %>%
    filter(bbref_id == id)
  split_leagues = rbind(split_leagues, before, after)
  cat("Starting", id, "/n")
}


split_leagues <- split_leagues %>%
  mutate(team_games = ifelse(season == 2020, 60, 162))

pitcher_stats_train <- rbind(pitcher_stats_train, split_leagues)

pitcher_stats_train <- pitcher_stats_train %>%
  filter(Level != "Maj-AL,Maj-NL")

al_pitchers <- pitcher_stats_train %>%
  filter(Level == "Maj-AL") %>%
  select(Name, season, Team, W, IP, ERA, WHIP, BAbip, SO_perc, uBB_perc)

nl_pitchers <- pitcher_stats_train %>%
  filter(Level == "Maj-NL") %>%
  select(Name, season, Team, W, IP, ERA, WHIP, BAbip, SO_perc, uBB_perc)

alcy <- mlb_awards_recipient(award_id = "ALCY") %>%
  filter(season >= 2008) %>%
  mutate(season = as.numeric(season)) %>%
  select(player_name_first_last, season, award_name)
nlcy <- mlb_awards_recipient(award_id = "NLCY") %>%
  filter(season >= 2008) %>%
  mutate(season = as.numeric(season)) %>%
  select(player_name_first_last, season, award_name)

al_pitchers <- left_join(al_pitchers, alcy, by = c("Name" = "player_name_first_last", "season")) %>%
  mutate(award = ifelse(is.na(award_name), 0, 1))

nl_pitchers <- left_join(nl_pitchers, nlcy, by = c("Name" = "player_name_first_last", "season")) %>%
  mutate(award = ifelse(is.na(award_name), 0, 1))

alcy_reg <- glm(award ~ W + IP + ERA + WHIP + BAbip + SO_perc + uBB_perc, data = al_pitchers, family = binomial)
nlcy_reg <- glm(award ~ W + IP + ERA + WHIP + BAbip + SO_perc + uBB_perc, data = nl_pitchers, family = binomial)

vip(nlcy_reg)
al_pitchers <- al_pitchers %>%
  ungroup() %>%
  mutate(prediction = predict(alcy_reg, al_pitchers, type = "response")) %>%
  group_by(season) %>%
  mutate(award_prob = prediction/sum(prediction)) %>%
  mutate(award_won = ifelse(award == 1, "WON", "")) %>%
  ungroup() 

nl_pitchers <- nl_pitchers %>%
  ungroup() %>%
  mutate(prediction = predict(nlcy_reg, nl_pitchers, type = "response")) %>%
  group_by(season) %>%
  mutate(award_prob = prediction/sum(prediction)) %>%
  mutate(award_won = ifelse(award == 1, "WON", "")) %>%
  ungroup() 

pitcher_stats_test <- bref_daily_pitcher("2023-01-01", "2023-12-31") %>%
  mutate(team_games = 162) %>%
  filter(IP/team_games >= 1.0)

both_leagues_test <- pitcher_stats_test %>%
  filter(Level == "Maj-AL,Maj-NL")

both_leagues_test$date[both_leagues_test$bbref_id == "montgjo01"] <- "2023-07-28"
both_leagues_test$date[both_leagues_test$bbref_id == "lynnla01"] <- "2023-07-26"
both_leagues_test$date[both_leagues_test$bbref_id == "verlaju01"] <- "2023-07-30"


split_leagues_test = data.frame()
for (i in 1:nrow(both_leagues_test)) {
  Sys.sleep(3)
  id = both_leagues_test[["bbref_id"]][i]
  start_date = "2023-01-01"
  end_date = "2023-12-31"
  change = both_leagues_test[["date"]][i]
  before <- bref_daily_pitcher(start_date, change) %>%
    filter(bbref_id == id)
  after <- bref_daily_pitcher(format((as.Date(change) + 1), "%Y-%m-%d"), end_date) %>%
    filter(bbref_id == id)
  split_leagues_test = rbind(split_leagues_test, before, after)
  cat("Starting", id, "/n")
}


split_leagues_test <- split_leagues_test %>%
  mutate(team_games = 162)

pitcher_stats_test <- rbind(pitcher_stats_test, split_leagues_test)

pitcher_stats_test <- pitcher_stats_test %>%
  filter(Level != "Maj-AL,Maj-NL")

al_pitchers_test <- pitcher_stats_test %>%
  filter(Level == "Maj-AL") %>%
  select(Name, season, Team, W, IP, ERA, WHIP, BAbip, SO_perc, uBB_perc)

nl_pitchers_test <- pitcher_stats_test %>%
  filter(Level == "Maj-NL") %>%
  select(Name, season, Team, W, IP, ERA, WHIP, BAbip, SO_perc, uBB_perc)

al_pitchers_test <- al_pitchers_test %>%
  ungroup() %>%
  mutate(prediction = predict(alcy_reg, al_pitchers_test, type = "response")) %>%
  group_by(season) %>%
  mutate(award_prob = prediction/sum(prediction)) %>%
  ungroup() 

nl_pitchers_test <- nl_pitchers_test %>%
  ungroup() %>%
  mutate(prediction = predict(nlcy_reg, nl_pitchers_test, type = "response")) %>%
  group_by(season) %>%
  mutate(award_prob = prediction/sum(prediction)) %>%
  ungroup() 

al_pitchers_test <- al_pitchers_test %>%
  arrange(-award_prob) %>%
  mutate(award_prob = round(award_prob, 3)) %>%
  mutate(nominee = ifelse(Name == "Gerrit Cole", "NOMINEE", ifelse(Name == "Kevin Gausman", "NOMINEE", ifelse(Name == "Sonny Gray", "NOMINEE", "")))) %>%
  select(Name, Team, award_prob, nominee) 

nl_pitchers_test <- nl_pitchers_test %>%
  arrange(-award_prob) %>%
  mutate(award_prob = round(award_prob, 3)) %>%
  mutate(nominee = ifelse(Name == "Blake Snell", "NOMINEE", ifelse(Name == "Zac Gallen", "NOMINEE", ifelse(Name == "Logan Webb", "NOMINEE", "")))) %>%
  select(Name, Team, award_prob, nominee) 

al_pitchers_test %>% filter(row_number() <= 10) %>% gt() %>% 
  cols_align(
    align = "center",
    columns = c(Name, Team, award_prob, nominee)
  ) %>%
  data_color(
    columns = award_prob,
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_label(
    Name = md("**Player**"),
    Team = md("**Team**"),
    award_prob = md("**ALCY Probability**"),
    nominee = md("**Nominee Status**")
  ) %>%
  tab_header(
    title = md("**2023 ALCY Award Probability**"),
    subtitle = "Including All Players Regardless of Nominee Status, Based on Data from 2008 to 2022"
  )

nl_pitchers_test %>% filter(row_number() <= 10) %>% gt() %>% 
  cols_align(
    align = "center",
    columns = c(Name, Team, award_prob, nominee)
  ) %>%
  data_color(
    columns = award_prob,
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_label(
    Name = md("**Player**"),
    Team = md("**Team**"),
    award_prob = md("**NLCY Probability**"),
    nominee = md("**Nominee Status**")
  ) %>%
  tab_header(
    title = md("**2023 NLCY Award Probability**"),
    subtitle = "Including All Players Regardless of Nominee Status, Based on Data from 2008 to 2022"
  )

alcy_nominees <- al_pitchers_test %>%
  filter(nominee == "NOMINEE") %>%
  mutate(award_prob_actual = round(award_prob/sum(award_prob), 3)) %>%
  select(Name, Team, award_prob_actual)

nlcy_nominees <- nl_pitchers_test %>%
  filter(nominee == "NOMINEE") %>%
  mutate(award_prob_actual = round(award_prob/sum(award_prob), 3)) %>%
  select(Name, Team, award_prob_actual)

alcy_nominees %>% gt() %>% 
  cols_align(
    align = "center",
    columns = c(Name, Team, award_prob_actual)
  ) %>%
  data_color(
    columns = award_prob_actual,
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_label(
    Name = md("**Player**"),
    Team = md("**Team**"),
    award_prob_actual = md("**ALCY Probability**")
  ) %>%
  tab_header(
    title = md("**2023 ALCY Award Probability**"),
    subtitle = "ONLY NOMINEES, Based on Data from 2008 to 2022"
  )

nlcy_nominees %>% gt() %>% 
  cols_align(
    align = "center",
    columns = c(Name, Team, award_prob_actual)
  ) %>%
  data_color(
    columns = award_prob_actual,
    colors = scales::col_numeric(
      palette = paletteer::paletteer_d(
        palette = "ggsci::blue_material"
      ) %>% as.character(),
      domain = NULL
    )
  ) %>%
  cols_label(
    Name = md("**Player**"),
    Team = md("**Team**"),
    award_prob_actual = md("**NLCY Probability**")
  ) %>%
  tab_header(
    title = md("**2023 NLCY Award Probability**"),
    subtitle = "Including All Players Regardless of Nominee Status, Based on Data from 2008 to 2022"
  )

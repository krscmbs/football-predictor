###packages###

# install.packages("devtools")
library("devtools")
# devtools::install_github("dashee87/footballR")
# install.packages("dplyr")
library("footballR")
library("dplyr")
# install.packages("skellam")
# install.packages("ggplot2")
# install.packages("purrr")
# install.packages("tidyr")
library(skellam)
library(ggplot2)
library(purrr)
library(tidyr)
# devtools::install_github("phillc73/abettor")
# install.packages("RCurl")
library(abettor)
library(RCurl)
library(readr)
library(tidyverse)

options(stringsAsFactors = FALSE)
options(max.print = 1000000)
options(scipen = 999, digits = 20)

###load data###

data <- read.csv("results.csv")
data <- as_tibble(data)
head(data)

rank <- read.csv("fifa_ranking.csv")
rank <- as_tibble(rank)
head(rank)

###create importance variable###

#see tournaments

cbind(table(data$tournament))

#create variable

data$importance <- "Minor Cup"
data$importance[grepl("qualification", data$tournament)] <- "Qualifier"
data$importance[data$tournament=="Friendly"] <- "Friendly"
data$importance[data$tournament %in% c("AFC Asian Cup", "UEFA Euro", "FIFA World Cup", 
                                       "Oceania Nations Cup", "Gold Cup", "Copa América", 
                                       "Africa Cup Of Nations", "Confederations Cup")] <- "Tournament"
data$importance[grepl("Nations League", data$tournament)] <- "Nations League"

#check

cbind(table(data$importance))

###date filters####

# data16 <- filter(data, data$date>="2016-01-01")
# head(data16)

data14 <- filter(data, data$date>="2018-06-14")
head(data14)

###append fifa ranking data###

max(rank$rank_date)
rank <- rank %>% 
  mutate(rank_date = as.Date(rank_date)) %>% 
  filter(rank_date >= '2022-10-06')

rank$country <- with(rank, if_else(country_full == "USA", "United States",
                           if_else(country_full == "IR Iran", "Iran",
                           if_else(country_full == "Côte d'Ivoire", "Ivory Coast",
                           if_else(country_full == "Curaçao", "Curacao",
                           if_else(country_full == "Korea Republic", "South Korea",
                           if_else(country_full == "Congo DR", "DR Congo",
                           if_else(country_full == "Cabo Verde", "Cape Verde",
                           if_else(country_full == "Kyrgyz Republic", "Kyrgyzstan",
                           if_else(country_full == "Korea DPR", "North Korea",
                           if_else(country_full == "St. Kitts and Nevis", "Saint Kitts and Nevis",
                           if_else(country_full == "Swaziland", "Eswatini",
                           if_else(country_full == "St. Vincent / Grenadines", "Saint Vincent and Grenadines",
                           if_else(country_full == "St. Lucia", "Saint Lucia",
                           if_else(country_full == "São Tomé and Pr?ncipe", "Sao Tome and Principe",
                           if_else(country_full == "Timor-Leste", "East Timor",
                           if_else(country_full == "Guyana", "French Guiana", country_full)))))))))))))))))

home_team_rank <- rank %>%
  dplyr::select(country, rank) %>%
  rename(home_team_rank = rank)
away_team_rank <- rank %>%
  dplyr::select(country, rank) %>%
  rename(away_team_rank = rank)
data14 <- left_join(data14, home_team_rank, by = c("home_team" = "country"))
data14 <- left_join(data14, away_team_rank, by = c("away_team" = "country"))

data14$home_team_rank <- with(data14, if_else(is.na(home_team_rank) == TRUE, 211, as.numeric(home_team_rank)))
data14$away_team_rank <- with(data14, if_else(is.na(away_team_rank) == TRUE, 211, as.numeric(away_team_rank)))

###recode neutral###

data14$neutral <- with(data14, if_else(neutral == TRUE, 1, 0))

check <- subset(data14, is.na(data14$home_team_rank) == TRUE)

###############model###############

#neutral model

poisson_model <- 
  rbind(
    data.frame(goals = data14$home_score,
               team = data14$home_team,
               rank = data14$home_team_rank,
               opponent = data14$away_team,
               importance = data14$importance,
               neutral = with(data14, if_else(neutral == 1, "Neutral", "Home"))
               ),
    data.frame(goals = data14$away_score,
               team = data14$away_team,
               rank = data14$away_team_rank,
               opponent = data14$home_team,
               importance = data14$importance,
               neutral = with(data14, if_else(neutral == 1, "Neutral", "Away"))
        )
    ) %>%
  glm(goals ~ team + opponent + importance + neutral, family = poisson(link = log), data = .)
summary(poisson_model)

#qatar home advantage model

qatar_model <-
  rbind(
    data.frame(goals = data14$home_score,
               team = data14$home_team,
               rank = data14$home_team_rank,
               opponent = data14$away_team,
               importance = data14$importance,
               home = 1),
    data.frame(goals = data14$away_score,
               team = data14$away_team,
               rank = data14$away_team_rank,
               opponent = data14$home_team,
               importance = data14$importance,
               home = 0)) %>%
  glm(goals ~ home + team + opponent + importance, family = poisson(link = log), data = .)
summary(qatar_model)

####predict goals scored###

#neutral

fixtures <- as_tibble(read_csv("fixtures.csv"))

fixtures$home_xg <- predict(poisson_model, 
                data.frame(team = fixtures$home_team, 
                           opponent = fixtures$away_team,
                           importance = fixtures$importance,
                           neutral = if_else(fixtures$neutral == 1, "Neutral", "Home")),
                type = "response")

fixtures$away_xg <- predict(poisson_model, 
                data.frame(team = fixtures$away_team, 
                           opponent = fixtures$home_team,
                           importance = fixtures$importance,
                           neutral = if_else(fixtures$neutral == 1, "Neutral", "Away")),
                type = "response")

write.csv(fixtures, file = "xG.csv", row.names = FALSE)

#russia home advantage

# mean(predict(qatar_model,
#         data.frame(home=1, team="Iran",
#                    opponent="Portugal",
#                    importance=c("Friendly", "Qualifier", "Tournament")),
#         type="response"))
# 
# mean(predict(qatar_model,
#         data.frame(home=0, team="Portugal",
#                    opponent="Iran",
#                    importance=c("Friendly", "Qualifier", "Tournament")),
#         type="response"))

###simulate matches###

###neutral

simulate_match <- function(poisson_model, home_team, away_team, max_goals = 8){
  home_goals_avg <- predict(poisson_model,
                            data.frame(team = home_team, 
                                       opponent = away_team,
                                       importance = c("Tournament"),
                                       neutral = "Neutral"), 
                            type = "response")
  away_goals_avg <- predict(poisson_model, 
                            data.frame(team = away_team, 
                                       opponent = home_team, 
                                       importance = c("Tournament"),
                                       neutral = "Neutral"),                             
                            type = "response")
  dpois(0:max_goals, home_goals_avg) %o% dpois(0:max_goals, away_goals_avg) 
}

probs <- data.frame(simulate_match(poisson_model, home_team, away_team, max_goals = 8))

fixtures$n <- c(1:48)
n <- 0
output_df <- data.frame()

for (i in 1:48) {
  n <- n + 1
  home_team <- (fixtures %>% filter(n == !!n) %>% select(home_team))[[1]]
  away_team <- (fixtures %>% filter(n == !!n) %>% select(away_team))[[1]]
  probs <- data.frame(simulate_match(poisson_model, home_team, away_team, max_goals = 8))
  colnames(probs) <- c(0:8)
  rownames(probs) <- c(0:8)
  home_win_prob <- sum(probs[lower.tri(probs)])
  away_win_prob <- sum(probs[upper.tri(probs)])
  draw_prob <- 1-home_win_prob-away_win_prob
  most_likely_df <- probs %>% 
    rownames_to_column('home_score') %>% 
    pivot_longer(!home_score, values_to = 'probs', names_to = 'away_score') %>% 
    arrange(desc(probs)) %>% 
    slice_head(n = 1)
  most_likely_result <- paste0(most_likely_df$home_score[[1]], '-', most_likely_df$away_score[[1]])
  most_likely_result_probs <- most_likely_df$probs[[1]]
  append <- data.frame(home_team, away_team, home_win_prob, draw_prob, away_win_prob, most_likely_result, most_likely_result_probs)
  output_df <- rbind(output_df, append)
}

fixtures <- fixtures %>% 
  select(-n) %>% 
  left_join(output_df, by = c('home_team', 'away_team'))

write.csv(fixtures, file = 'predictions.csv', row.names = FALSE)

##manually adjust qatar fixtures for home advantage

simulate_qatar <- function(russia_model, home_team, away_team, max_goals = 8){
  home_goals_avg <- predict(russia_model,
                            data.frame(home = 1,
                                       team = home_team,
                                       opponent = away_team,
                                       importance = c("Friendly", "Qualifier", "Tournament")),
                            type = "response")
  away_goals_avg <- predict(russia_model,
                            data.frame(home = 0,
                                       team = away_team,
                                       opponent = home_team,
                                       importance = c("Friendly", "Qualifier", "Tournament")),
                            type = "response")
  dpois(0:max_goals, home_goals_avg) %o% dpois(0:max_goals, away_goals_avg)
}

probsq <- simulate_qatar(qatar_model, "Qatar", "Senegal", max_goals = 8)

#russia win probability

sum(probsq[lower.tri(probsq)])

#draw probability

sum(diag(probsq))

#opponent win probability

sum(probsq[upper.tri(probsq)])

#most likely result

max(probsq)

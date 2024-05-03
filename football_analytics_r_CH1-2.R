rm(list = ls())
set.seed(3)
library(nflfastR)
library(tidyverse)

pbp_r <- load_pbp(2021)

pbp_r <- pbp_r |> filter(play_type == 'pass' & !is.na(air_yards))

pbp_r |>
  group_by(passer_id, passer) |>
  summarize(n = n(), adot = mean(air_yards)) |>
  filter(n >= 100 & !is.na(passer)) |>
  arrange(-adot) |>
  print(n=Inf)

library(ggthemes)

pbp_r1 <- load_pbp(2016:2022)

pbp_r1_p <- pbp_r1 |> filter(play_type == 'pass' & !is.na(air_yards))

pbp_r1_p <- 
  pbp_r1_p |> 
  mutate(
    pass_length_air_yards = ifelse(air_yards >= 20, "long", "short"),
    passing_yards = ifelse(is.na(passing_yards), 0, passing_yards)
  )

pbp_r1_p |>
  pull(passing_yards) |>
  summary()

pbp_r1_p |>
  filter(pass_length_air_yards == "short") |>
  pull(passing_yards) |>
  summary()

pbp_r1_p |>
  filter(pass_length_air_yards == "long") |>
  pull(passing_yards) |>
  summary()

pbp_r1_p |>
  filter(pass_length_air_yards == "short") |>
  pull(epa) |>
  summary()
  
pbp_r1_p |>
  filter(pass_length_air_yards == "long") |>
  pull(epa) |>
  summary()  

ggplot(pbp_r1, aes(x = passing_yards)) + 
  geom_histogram()
ggplot(pbp_r1, aes(x = epa)) + 
  geom_histogram()

pbp_r1_p |>
  filter(pass_length_air_yards == "long") |>
  ggplot(aes(passing_yards)) +
  geom_histogram(binwidth = 1) +
  ylab("Count") +
  xlab("epa") +
  theme_bw()

ggplot(pbp_r1_p, aes(x = pass_length_air_yards, y = passing_yards)) + 
  geom_boxplot() +
  theme_bw() + 
  xlab("Pass length in yards (long >= 20, short <= 20") +
  ylab("Yards gained/lost during a passing play")

ggplot(pbp_r1_p, aes(x = pass_length_air_yards, y = epa)) + 
  geom_boxplot() +
  theme_bw() + 
  xlab("Pass length in yards (long >= 20, short <= 20") +
  ylab("EPA for the play")

#grouping by player name and id to get mean of season passing yards
#n = attempts in the season
#.group drop removes the groups from resulting data frame
pbp_r1_p_s <- 
  pbp_r1_p |>
  group_by(passer_player_name, passer_player_id, season) |>
  summarize(
    ypa = mean(passing_yards, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )
  
#sorting the above output to look at

pbp_r1_p_s |> 
  arrange(-ypa) |>
  print()

#saving new dataframe with n = at least 100

pbp_r1_p_100 <- 
  pbp_r1_p |>
  group_by(passer_id, passer, season) |>
  summarize(
    n = n(), 
    ypa = mean(passing_yards),
    .groups = "drop"
    ) |>
  filter(n >= 100) |>
  arrange(-ypa)

pbp_r1_p_100 |>
  print(n=20)


#creating data frames for ypa for each player for each season and for the previous season and merging together
#to get better insights
air_yards_r1 <- 
  pbp_r1_p |>
  select(passer_id, passer, season, pass_length_air_yards, passing_yards) |>
  arrange(passer_id, season, pass_length_air_yards) |>
  group_by(passer_id, passer, pass_length_air_yards, season) |>
  summarize(n = n(),
            ypa = mean(passing_yards),
            .groups = "drop") |>
  filter((n >= 100 & pass_length_air_yards == 'short') |
           (n >= 30 & pass_length_air_yards == 'long')) |>
  select(-n)
air_yards_r1

air_yards_lag_r1 <-
  air_yards_r1 |>
  mutate(season = season + 1) |>
  rename(ypa_last = ypa)
air_yards_lag_r1

pbp_r1_p_s_pl <- 
  air_yards_r1 |>
  inner_join(air_yards_lag_r1, by = c("passer_id", "pass_length_air_yards",
                                      "season", "passer"))
#searching the df for brady and rodgers
pbp_r1_p_s_pl |>
  filter(passer %in% c("T.Brady", "A.Rodgers")) |>
  print(n = Inf)

#next 2 code lines do the same thing -- give some info about the df
glimpse(pbp_r1_p_s_pl)

pbp_r1_p_s_pl |>
  glimpse()

#getting # of unique qb's
pbp_r1_p_s_pl |>
  distinct(passer_id) |>
  nrow()

#creating scatterplot using geom_point()
scatter_ypa_r <- 
  ggplot(pbp_r1_p_s_pl, aes(x = ypa_last, y = ypa)) + 
  geom_point() + 
  facet_grid(cols = vars(pass_length_air_yards)) + 
  labs(
    x = "Yards per Attempt, Year n",
    y = "Yards per Attempt, Year n+1"
    )
  theme_bw() +
    theme(strip.background = element_blank())
print(scatter_ypa_r)

#adding geom_smoooth() to previous plot

scatter_ypa_r + 
  geom_smooth(method = "lm")

#looking at the numerical values for what the graphs are showing by looking at the correlation
pbp_r1_p_s_pl |>
  filter(!is.na(ypa) & !is.na(ypa_last)) |>
  group_by(pass_length_air_yards) |>
  summarize(correlation = cor(ypa, ypa_last))


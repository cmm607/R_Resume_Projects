rm(list = ls())
set.seed(3)
library(nflfastR)
library(tidyverse)
library(broom)

#loading the data and filtering for qbs w/ at least 10 passing plays in a given week to see frequencies
#of various toushdowns, first filtering out na values for pass_id
pbp_r <- load_pbp(2016:2022)
pbp_r_pass <- 
  pbp_r |>
  filter(!is.na(passer_id))

#replacing na values for td passes w/ 0 and summarizing by the season,
#passer_id, passer to calc number of passes per week and number of td passes per week
#then filtering to exclude players w/ fwer than 10 passes for each week
#then calc number of tds per qb per week
#lastly, will save total_line by using an aggregate funxtion like mean
pbp_r_pass_td_y <-
  pbp_r_pass |>
  mutate(
    pass_touchdown = ifelse(is.na(pass_touchdown), 0,
                            pass_touchdown)) |>
  group_by(season, week, passer_id, passer) |>
  summarize(
    n_passes = n(),
    pass_td_y = sum(pass_touchdown),
    total_line = mean(total_line)
  ) |>
  filter(n_passes >= 10)
pbp_r_pass_td_y |>
  group_by(pass_td_y) |>
  summarize(n = n())

print(pbp_r_pass_td_y)

pbp_r_pass_td_y |>
  ungroup() |>
  select(-passer, -passer_id) |>
  summary()

#checking if Poisson distribution is reasonable by looking at a bar graph of the grequencies
#and comparing this with a Poisson distribution of the same mean - lambda
pass_td_y_mean_r <-
  pbp_r_pass_td_y |>
  pull(pass_td_y) |>
  mean()

plot_pos_r <-
  tibble(x = seq(0, 7)) |>
  mutate(expected = dpois(
    x = x,
    lambda = pass_td_y_mean_r
  ))

ggplot() +
  geom_histogram(
    data = pbp_r_pass_td_y,
    aes(
      x = pass_td_y,
      y = after_stat(count / sum(count))
    ),
    binwidth = 0.5
  ) +
  geom_line(
    data = plot_pos_r, aes(x =x, y= expected),
    color = "red", linewidth = 1
  ) +
  theme_bw() +
  xlab("Touchdown passes per player per game for 2016 to 2022") +
  ylab("Probability")

#filter passing attempts >= 10 per week
pbp_r_pass_td_y_geq10 <-
  pbp_r_pass_td_y |>
  filter(n_passes >=10)

#taking avg td passes for each qb for previous season and current season up to current game
x_r <- tibble()

for (season_idx in seq(2017, 2022)) {
  for (week_idx in seq(1, 22)) {
    week_calc_r <-
      pbp_r_pass_td_y_geq10 |>
      filter((season == (season_idx - 1)) |
               (season == season_idx & week < week_idx)) |>
      group_by(passer_id, passer) |>
      summarize(
        n_games = n(),
        pass_td_rate = mean(pass_td_y),
        .groups = "keep"
      ) |>
      mutate(season = season_idx, week = week_idx)
    
    x_r <- bind_rows(x_r, week_calc_r)
  }
}
x_r |>
  filter(passer == "P.Mahomes")|>
  tail()

#now creating response variable which is the pbp_r_geq10 with the added game total
pbp_r_pass_td_y_geq10 <-
  pbp_r_pass_td_y_geq10 |>
  inner_join(x_r,
             by= c("season", "week", "passer_id", "passer"))

#plotting passing tds in each game for each passer
weekly_passing_id_r_plot <-
  pbp_r_pass_td_y_geq10 |>
  ggplot(aes(x = week, y = pass_td_y, group = passer_id)) +
  geom_line(alpha = 0.25) +
  facet_wrap(vars(season), nrow = 3) +
  theme_bw() +
  theme(strip.background = element_blank()) +
  ylab("Total passing tds") +
  xlab("Week of season")
weekly_passing_id_r_plot

#adding Poisson regression line to graph
weekly_passing_id_r_plot +
  geom_smooth(method = "glm", method.args = list("family" = "poisson"),
              se=FALSE,
              linewidth = 0.5, color = 'blue',
              alpha = 0.25)

#use poisson regression to model since we are assuming poisson distribution
pass_fit_r <-
  glm(pass_td_y ~ pass_td_rate + total_line,
      data = pbp_r_pass_td_y_geq10,
      family = "poisson")

pbp_r_pass_td_y_geq10 <-
  pbp_r_pass_td_y_geq10 |>
  ungroup() |>
  mutate(exp_pass_td = predict(pass_fit_r, type = "response"))

summary(pass_fit_r) |>
  print()

#poisson coefficients are on exponential scale, accessing model's parameters and taking exponential
tidy(pass_fit_r, exponentiate = TRUE, conf.int = TRUE)

#looking at mahomes from super bowl
pbp_r_pass_td_y_geq10 |>
  filter(passer == "P.Mahomes",
         season == 2022, week ==22) |>
  select(-pass_td_y, -n_passes, -passer_id, -week, -season, -n_games)

#dpois() function gives the PMF (probability mass function) and ppois() gives the CDF (cumulative density function)
pbp_r_pass_td_y_geq10 <-
  pbp_r_pass_td_y_geq10 |>
  mutate(
    p_0_td = dpois(x = 0,
                   lambda = exp_pass_td),
    p_1_td = dpois(x = 1,
                   lambda = exp_pass_td),
    p_2_td = dpois(x = 2,
                   lambda = exp_pass_td),
    p_g2_td = ppois(q = 2,
                    lambda = exp_pass_td,
                    lower.tail = FALSE)
  )

#looking at outputs for mahomes going into super bowl
pbp_r_pass_td_y_geq10 |>
  filter(passer == "P.Mahomes", season == 2022, week == 22) |>
  select(-pass_td_y, -n_games, -n_passes, -passer_id, -week, -season)

#looking more deeply at poisson regression coefficients
x <- rpois(n = 10, lambda = 1)
print(x)
print(mean(x))

#fitting glm with global intercept and looking at coefficient on model scale and exponential scale
glm_out_r <-
  glm(x ~ 1, family = "poisson")
print(tidy(glm_out_r))
print(tidy(glm_out_r, exponentiate = TRUE))

#looking at ravens td passes since qb got hurt week 13 expect to see decline in avg or expected passes
bal_td_r <-
  pbp_r |>
  filter(posteam == "BAL" & season == 2022) |>
  group_by(game_id, week) |>
  summarize(
    td_per_game = sum(touchdown, na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(week = week - 1)
ggplot(bal_td_r, aes(x = week, y = td_per_game)) +
  geom_point() +
  theme_bw() +
  stat_smooth(method = "glm", formula = "y ~ x",
              method.args = list(family = "poisson")) +
  xlab("Week") +
  ylab("Touchdowns per game") +
  scale_y_continuous(breaks = seq(0, 6)) +
  scale_x_continuous(breaks = seq(1, 20, by = 2))

#now building model to look at coefficients
glm_bal_td_r <-
  glm(td_per_game ~ week,
      data = bal_td_r,
      family = "poisson")
print(tidy(glm_bal_td_r))
print(tidy(glm_bal_td_r, exponentiate = TRUE))

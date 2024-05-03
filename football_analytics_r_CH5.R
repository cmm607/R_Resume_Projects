rm(list = ls())
set.seed(3)
library(nflfastR)
library(tidyverse)
library(broom)

#loading the data and filtering for pass plays with a passer and a pass depth
pbp_r <- load_pbp(2016:2022)
pbp_r_pass <- 
  pbp_r |>
  filter(play_type == "pass" & !is.na(passer_id) &
           !is.na(air_yards))

#restricting air yards to be greater than 0 and less than or equal to 20
#to ensure a large enough sample size
#going to summarize the data to get completion percentage

pass_pct_r <- 
  pbp_r_pass |>
  filter(0 < air_yards & air_yards <= 20) |>
  group_by(air_yards) |>
  summarize(comp_pct = mean(complete_pass),
            .groups = 'drop')

#graphing air yards and completion percentage with scatterplot and setting a smoothed best fit line
pass_pct_r |>
  ggplot(aes(x = air_yards, y = comp_pct)) +
  geom_point() +
  stat_smooth(method = "lm") +
  theme_bw() +
  ylab("Percent completion") +
  xlab("Air Yards")

#applying logistic regression to predict binary data
complete_ay_r <-
  glm(complete_pass ~ air_yards,
      data = pbp_r_pass,
      family = "binomial")
summary(complete_ay_r)

#plotting the above
ggplot(data = pbp_r_pass,
       aes(x=air_yards, y=complete_pass)) +
  geom_jitter(height = 0.05, width = 0,
              alpha = 0.05) +
  stat_smooth(method = 'glm',
              method.args=list(family="binomial")) +
  theme_bw() +
  ylab("Completed pass (1=yes, 0=no)") +
  xlab("air yards")

#glm's have diff residuals so cant take right from the model, have to manually calc them

#creating a new column that gets values by extracting the predicted model fits via the prefict()
# then subracting that value from the complete_pass to calc CPOE (completion percentage over expected)
pbp_r_pass <-
  pbp_r_pass |>
  mutate(exp_completion = predict(complete_ay_r, type = "resp"),
         cpoe = complete_pass - exp_completion)

#looking at leaders in CPOE since 2016 vs leaders in actual completion percentage,
#only looking at passes with air_yards, qbs w/ 100 or more attempts
#calculating the mean CPOE and mean completion percentage and arranging by compl to look at
pbp_r_pass |>
  group_by(season, passer_id, passer) |>
  summarize(n = n(),
            cpoe = mean(cpoe, na.rm =TRUE),
            compl = mean(complete_pass, na.rm =TRUE),
            .groups = "drop") |>
  filter(n >= 100) |>
  arrange(-cpoe) |>
  print(n = 20)

#adding more variables to the model - down, distance to go, distance to endzone, pass location, 
#whether qb was hit

#removing missing data
pbp_r_pass_no_miss <-
  pbp_r_pass |>
  mutate(down = factor(down),
         qb_hit = factor(qb_hit)) |>
  filter(complete.cases(down, qb_hit, complete_pass,
                        ydstogo, yardline_100, air_yards,
                        pass_location, qb_hit))
#running model and saving outputs
complete_more_r <-
  pbp_r_pass_no_miss |>
  glm(formula = complete_pass ~ down * ydstogo + yardline_100 +
        air_yards + pass_location + qb_hit,
      family = "binomial")

#calc CPOE
pbp_r_pass_no_miss <-
  pbp_r_pass_no_miss |>
  mutate(exp_completion = predict(complete_more_r, type = "resp"),
         cpoe = complete_pass - exp_completion)

#summarizing the data
cpoe_more_r <-
  pbp_r_pass_no_miss |>
  group_by(season, passer_id, passer) |>
  summarize(n = n(),
            cpoe = mean(cpoe, na.rm = TRUE),
            compl = mean(complete_pass),
            exp_completion = mean(exp_completion),
            .groups = "drop") |>
  filter(n > 100)

#print top 20 entries
cpoe_more_r |>
  arrange(-cpoe) |>
  print(n = 20)

#calc the lag between current CPOE and last year's CPOE

#create current df
cpoe_now_r <-
  cpoe_more_r |>
  select(-n)
print(cpoe_now_r)
#creating last year's df and adding 1 to season
cpoe_last_r <-
  cpoe_more_r |>
  select(-n) |>
  mutate(season = season + 1) |>
  rename(cpoe_last = cpoe,
         compl_last = compl,
         exp_completion_last = exp_completion)
print(cpoe_last_r)

#merge together
cpoe_lag_r <-
  cpoe_now_r |>
  inner_join(cpoe_last_r,
             by = c("passer_id", "passer", "season")) |>
  ungroup()
print(cpoe_lag_r)
#select the 2 passing column and examine correlation
cpoe_lag_r |>
  select(compl_last, compl) |>
  cor(use = "complete.obs")

cpoe_lag_r |>
  select(cpoe_last, cpoe) |>
  cor(use = "complete.obs")

#looking at correlation for expected completions
cpoe_lag_r |>
  select(exp_completion_last, exp_completion) |>
  cor(use="complete.obs")

#first look at odds ratios
complete_ay_r |>
  tidy(exponentiate = TRUE, conf.int = TRUE)
#care about odds being diff from 1 since 1:1 implies predictor doesn't change the outcom of an event

pbp_r_pass |>
  summarize(comp_pct = mean(complete_pass)) |>
  mutate(odds = comp_pct / (1 - comp_pct),
         log_odds = log(odds))

complete_global_r <-
  glm(complete_pass ~ 1,
      data = pbp_r_pass,
      family = "binomial")
complete_global_r |>
  tidy()

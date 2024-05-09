rm(list = ls())
set.seed(3)
library(nflfastR)
library(tidyverse)

demo_data_r <- tibble(down = c("first", "second"),
                      ydstogo = c(10, 5))

model.matrix(~ ydstogo + down, data = demo_data_r)

model.matrix(~ ydstogo + down -1, data = demo_data_r)


pbp_r <- load_pbp(2016:2022)

#filtering only for run data and removing plays that were not regular downs
pbp_r_run <-
  pbp_r |>
  filter(play_type == "run" & !is.na(rusher_id) &
           !is.na(down) & !is.na(run_location)) |>
  mutate(rushing_yards = ifelse(is.na(rushing_yards),
       0,
       rushing_yards)
         )
#changing down to be integer
pbp_r_run <-
  pbp_r_run |>
  mutate(down = as.character(down))

#plotting histogram of rushing yds by down
ggplot(pbp_r_run, aes(x = rushing_yards)) + 
  geom_histogram(binwidth = 1) +
  facet_wrap(vars(down), ncol = 2,
             labeller = label_both) +
  theme_bw() +
  theme(strip.background = element_blank())

#now accounting for yds to go == 10 since most rushes on 4th and 3rd down are in short yard situations
pbp_r_run |>
  filter(ydstogo == 10) |>
  ggplot(aes(x = down, y = rushing_yards)) +
  geom_boxplot()

#looking at yards to endzone now
ggplot(pbp_r_run, aes(x = yardline_100, y = rushing_yards)) +
  geom_point(alpha = 0.25) + 
  stat_smooth(method = "lm") +
  theme_bw()

#now going to bin and average and plot with scatterplot w/ geom_point
pbp_r_run |>
  group_by(yardline_100) |>
  summarize(rushing_yards_mean = mean(rushing_yards)) |>
  ggplot(aes(x = yardline_100, y = rushing_yards_mean)) +
  geom_point() + 
  stat_smooth(method = "lm") +
  theme_bw()

#now going to look at run location
ggplot(pbp_r_run, aes(run_location, rushing_yards)) +
  geom_boxplot() +
  theme_bw()

#looking at score differential using binning and summarize
pbp_r_run |>
  group_by(score_differential) |>
  summarize(rushing_yards_mean = mean(rushing_yards)) |>
  ggplot(aes(score_differential, rushing_yards_mean)) +
  geom_point() +
  stat_smooth(method = "lm") +
  theme_bw()

#creating multiple linear regression with an interaction of down:ydstogo which creates a contrast
pbp_r_run <-
  pbp_r_run |>
  mutate(down = as.character(down))

expected_yards_r <-
  lm(rushing_yards ~ 1 + down + ydstogo + down:ydstogo  +
       yardline_100 + run_location + score_differential,
     data = pbp_r_run
     )
pbp_r_run <-
  pbp_r_run |>
  mutate(ryoe = resid(expected_yards_r))
summary(expected_yards_r)

#using package to make a table from the regression output
library(broom)
library(kableExtra)
expected_yards_r |>
  tidy(conf.int = TRUE) |>
  kbl(format = "pipe", digits = 2) |>
  kable_styling()

#creating summary table for ryoe totals, means, and yards per carry 
#and then saving only data from rushers with more than 50 carries
ryoe_r <-
  pbp_r_run |>
  group_by(season, rusher_id, rusher) |>
  summarize(
    n = n(), ryoe_total = sum(ryoe), ryoe_per = mean(ryoe),
    yards_per_carry = mean(rushing_yards) 
      ) |>
  filter(n > 50)

#sorting by total ryoe
ryoe_r |>
  arrange(-ryoe_total) |>
  print()

#sorting by ryoe_per
ryoe_r |>
  filter(n > 50) |>
  arrange(-ryoe_per) |>
  print()

#redoing the analysis from ch 3 to see if RYOE is a better metric than yards per carry

#creating current df
ryoe_now_r <-
  ryoe_r |>
  select(-n, -ryoe_total)

#creating last years df and adding 1 to season
ryoe_last_r <-
  ryoe_r |>
  select(-n, -ryoe_total) |>
  mutate(season = season + 1) |>
  rename(ryoe_per_last = ryoe_per,
         yards_per_carry_last = yards_per_carry)
ryoe_r
ryoe_now_r
ryoe_last_r

#merging together
ryoe_lag_r <-
  ryoe_now_r |>
  inner_join(ryoe_last_r,
             by = c("rusher_id", "rusher", "season")) |>
  ungroup()
ryoe_lag_r

#selecting the columns and examining the correlation
ryoe_lag_r |>
  select(yards_per_carry, yards_per_carry_last) |>
  cor(use = "complete.obs")

ryoe_lag_r |>
  select(ryoe_per, ryoe_per_last) |>
  cor(use = "complete.obs")

#doing internal diagnosis of relationships
par(mfrow = c(2, 2))
plot(expected_yards_r)
#THIS CHAPTER ALSO EXPLAINS THE QQ RESIDUALS AND WHAT THE GRAPHS MEAN FROM THE CODE ABOVE

#looking at what happens to the graphs if we remove plays less than 15 yds or greater than 90 yds
expected_yards_filter <-
  pbp_r_run |>
  filter(rushing_yards > 15 & rushing_yards < 90) |>
  lm(formula = rushing_yards ~ 1 + down + ydstogo + down:ydstogo +
       yardline_100 + run_location + score_differential)
par(mfrow = c(2, 2))
plot(expected_yards_filter)
summary(expected_yards_filter)


### Exercises 
#Question 1.
#Change the carries threshold from 50 carries to 100 carries. Do you still see the same stability differences 
#that you found in this chapter?

#As seen in the code below, I do not see the same stability difference. Here I see that the yards per carry metric
#is actually more stable than the rushing yard over expected when accounting for a threshold of 100 carries.

#creating summary table for ryoe totals, means, and yards per carry 
#and then saving only data from rushers with more than 100 carries
ryoe_r_Q1 <-
  pbp_r_run |>
  group_by(season, rusher_id, rusher) |>
  summarize(
    n = n(), ryoe_total = sum(ryoe), ryoe_per = mean(ryoe),
    yards_per_carry = mean(rushing_yards) 
  ) |>
  filter(n > 100)

#sorting by total ryoe
ryoe_r_Q1 |>
  arrange(-ryoe_total) |>
  print()

#sorting by ryoe_per
ryoe_r_Q1 |>
  filter(n > 100) |>
  arrange(-ryoe_per) |>
  print()

#redoing the analysis from ch 3 to see if RYOE is a better metric than yards per carry

#creating current df
ryoe_now_r_Q1 <-
  ryoe_r_Q1 |>
  select(-n, -ryoe_total)

#creating last years df and adding 1 to season
ryoe_last_r_Q1 <-
  ryoe_r_Q1 |>
  select(-n, -ryoe_total) |>
  mutate(season = season + 1) |>
  rename(ryoe_per_last = ryoe_per,
         yards_per_carry_last = yards_per_carry)
ryoe_r_Q1
ryoe_now_r_Q1
ryoe_last_r_Q1

#merging together
ryoe_lag_r_Q1 <-
  ryoe_now_r_Q1 |>
  inner_join(ryoe_last_r_Q1,
             by = c("rusher_id", "rusher", "season")) |>
  ungroup()
ryoe_lag_r_Q1

#selecting the columns and examining the correlation
ryoe_lag_r_Q1 |>
  select(yards_per_carry, yards_per_carry_last) |>
  cor(use = "complete.obs")

ryoe_lag_r_Q1 |>
  select(ryoe_per, ryoe_per_last) |>
  cor(use = "complete.obs")

#Question 2. Inspect James Conner's RYOE values for his career relative to Bell's. What do you notice about the 
#metric for both backs?
# I notice that James Conner has a better RYOE total than Leveon Bell. That is good to know considering it was 
# major controversy when the Steelers moved off from Bell in favor of James Conner. This now makes sense as
# the following years Bell had declining RYOE while Conner had ups and downs but overall better RYOE compared to
#Bell.

filter(ryoe_r, rusher=='J.Conner')
filter(ryoe_r, rusher=='L.Bell')


rm(list = ls())
set.seed(3)
library(nflfastR)
library(tidyverse)

pbp_r <- load_pbp(2016:2022)

#filtering for running plays and plays w/o rushers out and then making missing rushigng yards to 0
pbp_r_run <- 
  pbp_r |>
  filter(play_type == "run" & !is.na(rusher_id)) |>
  mutate(rushing_yards = ifelse(is.na(rushing_yards), 0, rushing_yards))

#plotting a scatterplot (geom_point) of the data prior to building model and adding a smoothed line
#to see if it is positive or negative slope
ggplot(pbp_r_run, aes(x = ydstogo, y = rushing_yards)) +
  geom_point() + 
  theme_bw() + 
  stat_smooth(method = "lm")


#average over each yds per carry value gained in each bin
#first create ypc (yards per carry) 
pbp_r_run_avg <- 
  pbp_r_run |>
  group_by(ydstogo) |>
  summarize(ypc = mean(rushing_yards))

#plotting histogram
ggplot(pbp_r_run_avg, aes(x = ydstogo, y = ypc)) +
  geom_point() + 
  theme_bw() +
  stat_smooth(method = "lm")

#creating simple linear regression model
yards_to_go_r <- 
  lm(rushing_yards ~ ydstogo, data = pbp_r_run)
summary(yards_to_go_r)

#creating an RYOE (rush yds over expected) column in the data
pbp_r_run <-
  pbp_r_run |>
  mutate(ryoe = resid(yards_to_go_r))

#grouping by seasons, rusher, rusher_id
#summarizing n = n() - number of carries a rusher has
# sum of ryoe - total ryoe
#mean of ryoe - ryoe per carry
#mean of rushing yards is ypc
#arrange by total ryoe from greatest to least
#then filtering to include only players with at least 50 carries

ryoe_r <- 
  pbp_r_run |>
  group_by(season, rusher_id, rusher) |>
  summarize(
    n=n(),
    ryoe_total = sum(ryoe),
    ryoe_per = mean(ryoe),
    yards_per_carry = mean(rushing_yards)
  ) |>
  arrange(-ryoe_total) |>
  filter(n > 50)
print(ryoe_r)

ryoe_r |>
  arrange(-ryoe_per)

#comparing ryoe per carry to traditional yards per carry
#creating current df to work with
ryoe_now_r <-
  ryoe_r |>
  select(-n, -ryoe_total)

#creating last seaon df and add 1 to season
ryoe_last_r <-
  ryoe_r |>
  select(-n, -ryoe_total) |>
  mutate(season = season +1) |>
  rename(ryoe_per_last = ryoe_per,
         yards_per_carry_last = yards_per_carry)

#joining the 2 together
ryoe_lag_r <-
  ryoe_now_r |>
  inner_join(ryoe_last_r,
             by = c("rusher_id", "rusher", "season")) |>
  ungroup()

#selecting the two yds per carries collumns and examining correlation
ryoe_lag_r |>
  select(yards_per_carry, yards_per_carry_last) |>
  cor(use = "complete.obs")

ryoe_lag_r |>
  select(ryoe_per, ryoe_per_last) |>
  cor(use = "complete.obs")

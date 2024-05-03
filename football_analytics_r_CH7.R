<<<<<<< HEAD
rm(list = ls())
set.seed(3)
library(nflfastR)
library(tidyverse)
library(broom)
library(janitor)
library(rvest)
library(htmlTable)
library(zoo)

#web_scraping from pro football reference for draft data
draft_r <- tibble()

#looping over years 2000-2022
for (i in seq(from = 2000, to = 2022)) {
  url <- paste0(
    "https://www.pro-football-reference.com/years/",
    i,
    "/draft.htm"
  )
  web_data <-
    read_html(url) |>
    html_nodes(xpath = '//*[@id="drafts"]') |>
    html_table()
  
  web_df <-
    web_data[[1]]
  
  web_df_clean <-
    web_df |>
    janitor::row_to_names(row_number = 1) |>
    janitor::clean_names(case = "none") |>
    mutate(Season = i) |>
    filter(Tm != "Tm") #remove any extra column headers
  
  draft_r <-
    bind_rows(
      draft_r,
      web_df_clean
    )
}

#renaming teams to reflect those that moved like the chargers
draft_r <-
  draft_r |>
  mutate(Tm = case_when(Tm == "SDG" ~ "LAC",
                        Tm == "OAK" ~ "LVR",
                        Tm == "STL" ~ "LAR",
                        TRUE ~ Tm),
         DrAV = ifelse(is.na(DrAV), 0, DrAV))
write_csv(draft_r, "draft_data_r.csv")
draft_r <- read_csv("draft_data_r.csv")

#selecting the data we'll need for analysis
draft_r_use <-
  draft_r |>
  select(Season, Pick, Tm, Player, Pos, wAV, DrAV)
print(draft_r_use)

#looking at values of draft picks DrAV Draft approximite value
#selecting years prior to 2019

draft_r_use_pre2019 <-
  draft_r_use |>
  mutate(DrAV = as.numeric(DrAV),
         wAV = as.numeric(wAV),
         Pick = as.integer(Pick)) |>
  filter(Season <= 2019)

ggplot(draft_r_use_pre2019, aes(Pick, DrAV)) +
  geom_point(alpha = 0.2) +
  stat_smooth() +
  theme_bw()

#going to smooth out value for each pick, first calc avg value for each,
#replace nan values, then calc 6 pick moving avg
draft_chart_r <-
  draft_r_use_pre2019 |>
  group_by(Pick) |>
  summarize(mean_DrAV = mean(DrAV, na.rm = TRUE)) |>
  mutate(mean_DrAV = ifelse(is.na(mean_DrAV),
                            0, mean_DrAV)) |>
  mutate(
    roll_DrAV = 
      rollapply(mean_DrAV,
                width = 13,
                FUN = mean,
                na.rm = TRUE,
                fill = "extend",
                partial = TRUE)
  )

ggplot(draft_chart_r, aes(Pick, roll_DrAV)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  ylab("Rolling avg (\u00B1 6) DrAV") +
  xlab("Draft Pick")

#from here can fit a model to the data to quantify the results
DrAV_pick_fit_r <-
  draft_chart_r |>
  lm(formula = log(roll_DrAV + 1) ~ Pick)
summary(DrAV_pick_fit_r)

#merging back into draft_chart)r and looking at tio if data
draft_chart_r <-
  draft_chart_r |>
  mutate(
    fitted_DrAV =
      pmax(
        0,
        exp(predict(DrAV_pick_fit_r)) -1
      )
  )
draft_chart_r |>
  head()

#now have an estimate for worth of each draft pick, looking at what the model
# would have said about the trade between jets and colts 
library(kableExtra)
future_pick <- 
  tibble(
    Pick = "Future 2nd round",
    Value = "14.8 (discounted at rate of 25%)"
  )
team <- tibble("Receiving team" = c("Jets", rep("Colts", 4)))

tbl_1 <-
  draft_chart_r |>
  filter(Pick %in% c(3, 6, 37, 49)) |>
  select(Pick, fitted_DrAV) |>
  rename(Value = fitted_DrAV) |>
  mutate(
    Pick = as.character(Pick),
    Value = as.character(round(Value, 1))
  ) |>
  bind_rows(future_pick)

team |>
  bind_cols(tbl_1) |>
  kbl(format = "pipe") |>
  kable_styling()
#above we can see jets got "fleeced" by losing the trade by 50+ DrAV

#using the data to see actual DrAV for the players
results_trade <-
  tibble(
    Team = c("Jets", rep("Colts", 5)),
    Pick = c(
      3, 6, 37,
      "49-traded for 52",
      "49-traded for 169",
      "52 in 2019"
    ),
    Player = c(
      "Sam Darnold",
      "Quenton Nelson",
      "Braden Smith",
      "Kemoko Turay",
      "Jordan Wilkins",
      "Rock Ya-Sin"
    ),
    "DrAV" = c(25, 55, 32, 5, 8, 11)
  )
results_trade |>
  kbl(format = "pipe") |>
  kable_styling()

#seeing if some teams are better than others at drafting
draft_r_use_pre2019 <-
  draft_r_use_pre2019 |>
  left_join(draft_chart_r |> select(Pick, fitted_DrAV),
            by = "Pick")
draft_r_use_pre2019 |>
  group_by(Tm) |>
  summarize(
    total_picks = n(),
    DrAV_OE = mean(DrAV - fitted_DrAV, na.rm = TRUE),
    DrAV_sigma = sd(DrAV - fitted_DrAV, na.rm = TRUE)
  ) |>
  arrange(-DrAV_OE) |>
  print(n= Inf)

#checking statistical difference in output above
draft_r_use_pre2019 |>
  group_by(Tm) |>
  summarize(
    total_picks = n(),
    DrAV_OE = mean(DrAV - fitted_DrAV, na.rm =TRUE),
    DrAV_sigma = sd(DrAV - fitted_DrAV, na.rm = TRUE)
  ) |>
  mutate(
    se = DrAV_sigma / sqrt(total_picks),
    lower_bound = DrAV_OE - 1.96 * se,
    upper_bound = DrAV_OE + 1.96 * se
  ) |>
  arrange(-DrAV_OE) |>
  print(n = Inf)
=======
rm(list = ls())
set.seed(3)
library(nflfastR)
library(tidyverse)
library(broom)
library(janitor)
library(rvest)
library(htmlTable)
library(zoo)

#web_scraping from pro football reference for draft data
draft_r <- tibble()

#looping over years 2000-2022
for (i in seq(from = 2000, to = 2022)) {
  url <- paste0(
    "https://www.pro-football-reference.com/years/",
    i,
    "/draft.htm"
  )
  web_data <-
    read_html(url) |>
    html_nodes(xpath = '//*[@id="drafts"]') |>
    html_table()
  
  web_df <-
    web_data[[1]]
  
  web_df_clean <-
    web_df |>
    janitor::row_to_names(row_number = 1) |>
    janitor::clean_names(case = "none") |>
    mutate(Season = i) |>
    filter(Tm != "Tm") #remove any extra column headers
  
  draft_r <-
    bind_rows(
      draft_r,
      web_df_clean
    )
}

#renaming teams to reflect those that moved like the chargers
draft_r <-
  draft_r |>
  mutate(Tm = case_when(Tm == "SDG" ~ "LAC",
                        Tm == "OAK" ~ "LVR",
                        Tm == "STL" ~ "LAR",
                        TRUE ~ Tm),
         DrAV = ifelse(is.na(DrAV), 0, DrAV))
write_csv(draft_r, "draft_data_r.csv")
draft_r <- read_csv("draft_data_r.csv")

#selecting the data we'll need for analysis
draft_r_use <-
  draft_r |>
  select(Season, Pick, Tm, Player, Pos, wAV, DrAV)
print(draft_r_use)

#looking at values of draft picks DrAV Draft approximite value
#selecting years prior to 2019

draft_r_use_pre2019 <-
  draft_r_use |>
  mutate(DrAV = as.numeric(DrAV),
         wAV = as.numeric(wAV),
         Pick = as.integer(Pick)) |>
  filter(Season <= 2019)

ggplot(draft_r_use_pre2019, aes(Pick, DrAV)) +
  geom_point(alpha = 0.2) +
  stat_smooth() +
  theme_bw()

#going to smooth out value for each pick, first calc avg value for each,
#replace nan values, then calc 6 pick moving avg
draft_chart_r <-
  draft_r_use_pre2019 |>
  group_by(Pick) |>
  summarize(mean_DrAV = mean(DrAV, na.rm = TRUE)) |>
  mutate(mean_DrAV = ifelse(is.na(mean_DrAV),
                            0, mean_DrAV)) |>
  mutate(
    roll_DrAV = 
      rollapply(mean_DrAV,
                width = 13,
                FUN = mean,
                na.rm = TRUE,
                fill = "extend",
                partial = TRUE)
  )

ggplot(draft_chart_r, aes(Pick, roll_DrAV)) +
  geom_point() +
  geom_smooth() +
  theme_bw() +
  ylab("Rolling avg (\u00B1 6) DrAV") +
  xlab("Draft Pick")

#from here can fit a model to the data to quantify the results
DrAV_pick_fit_r <-
  draft_chart_r |>
  lm(formula = log(roll_DrAV + 1) ~ Pick)
summary(DrAV_pick_fit_r)

#merging back into draft_chart)r and looking at tio if data
draft_chart_r <-
  draft_chart_r |>
  mutate(
    fitted_DrAV =
      pmax(
        0,
        exp(predict(DrAV_pick_fit_r)) -1
      )
  )
draft_chart_r |>
  head()

#now have an estimate for worth of each draft pick, looking at what the model
# would have said about the trade between jets and colts 
library(kableExtra)
future_pick <- 
  tibble(
    Pick = "Future 2nd round",
    Value = "14.8 (discounted at rate of 25%)"
  )
team <- tibble("Receiving team" = c("Jets", rep("Colts", 4)))

tbl_1 <-
  draft_chart_r |>
  filter(Pick %in% c(3, 6, 37, 49)) |>
  select(Pick, fitted_DrAV) |>
  rename(Value = fitted_DrAV) |>
  mutate(
    Pick = as.character(Pick),
    Value = as.character(round(Value, 1))
  ) |>
  bind_rows(future_pick)

team |>
  bind_cols(tbl_1) |>
  kbl(format = "pipe") |>
  kable_styling()
#above we can see jets got "fleeced" by losing the trade by 50+ DrAV

#using the data to see actual DrAV for the players
results_trade <-
  tibble(
    Team = c("Jets", rep("Colts", 5)),
    Pick = c(
      3, 6, 37,
      "49-traded for 52",
      "49-traded for 169",
      "52 in 2019"
    ),
    Player = c(
      "Sam Darnold",
      "Quenton Nelson",
      "Braden Smith",
      "Kemoko Turay",
      "Jordan Wilkins",
      "Rock Ya-Sin"
    ),
    "DrAV" = c(25, 55, 32, 5, 8, 11)
  )
results_trade |>
  kbl(format = "pipe") |>
  kable_styling()

#seeing if some teams are better than others at drafting
draft_r_use_pre2019 <-
  draft_r_use_pre2019 |>
  left_join(draft_chart_r |> select(Pick, fitted_DrAV),
            by = "Pick")
draft_r_use_pre2019 |>
  group_by(Tm) |>
  summarize(
    total_picks = n(),
    DrAV_OE = mean(DrAV - fitted_DrAV, na.rm = TRUE),
    DrAV_sigma = sd(DrAV - fitted_DrAV, na.rm = TRUE)
  ) |>
  arrange(-DrAV_OE) |>
  print(n= Inf)

#checking statistical difference in output above
draft_r_use_pre2019 |>
  group_by(Tm) |>
  summarize(
    total_picks = n(),
    DrAV_OE = mean(DrAV - fitted_DrAV, na.rm =TRUE),
    DrAV_sigma = sd(DrAV - fitted_DrAV, na.rm = TRUE)
  ) |>
  mutate(
    se = DrAV_sigma / sqrt(total_picks),
    lower_bound = DrAV_OE - 1.96 * se,
    upper_bound = DrAV_OE + 1.96 * se
  ) |>
  arrange(-DrAV_OE) |>
  print(n = Inf)
>>>>>>> 8e23923ad6a187b5f0a409b173a99e0e0b779d87

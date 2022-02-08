library(tidyverse)
library(modelr)
library(ggthemes)

## SHARE OF SHOTS THAT WERE 3PT

# From 2002 to 2007
nba_data_historical <- rename(nba_data_historical, "share_3pt" = "3PAr")
threept_shooting <- nba_data_historical %>%
  filter(year_id %in% 2002:2007, type == "RS",
         team_id %in% nba_data_historical$team_id[nba_data_historical$year_id 
                                                  == 2007]) %>%
  group_by(team_id) %>%
  summarize(share_3pt = mean(as.numeric(share_3pt)))


team_records <- rename(team_records, "win_loss_perc" = "W/L%")
team_records_data <- team_records %>%
  filter(Season %in% 2002:2007, 
         Team %in% team_records$Team[team_records$Season 
                                     == 2007]) %>%
  group_by(Team) %>%
  summarize(win_loss_perc = mean(as.numeric(win_loss_perc)))

threept_success_table <- data.frame(
  team = team_records_data$Team,
  threept_shooting = threept_shooting$share_3pt, 
  team_success = team_records_data$win_loss_perc
)


threept_success_reg <- lm(data = threept_success_table, 
                          team_success ~ threept_shooting)
summary(threept_success_reg)


threept_success_table <- threept_success_table %>%
  add_residuals(threept_success_reg)

ggplot(data = threept_success_table, 
       aes(threept_shooting, team_success)) +
  geom_point() +
  labs(x = "Proportion of shots from three-point range", 
       y = "Percentage of games won", 
       title = "The association between three-point shooting and team success: 2002-2007", 
       caption = "Source: FiveThirtyEight, Kaggle") +
  theme_stata() +
  theme(panel.grid = element_blank(), 
        plot.title = element_text(hjust = 0.5), 
        axis.title.y = element_text(vjust = 2))

## 3PT FG PERCENTAGE, LINEAR REGRESSION, 2002-2007
three_fg <- nba_data_historical %>%
  filter(type == "RS", G > 10, 
         three_fg > 10, 
         year_id %in% 2002:2007) %>%
  group_by(team_id) %>%
  summarize(three_fg = mean(as.numeric(three_fg)))

seasons2002_2017 <- data.frame(
  team = three_fg[, 1],
  three_fg = three_fg[, 2], 
  success = success_time(2002:2007)[, 2]
)

model2002_2017 <- lm(success ~ three_fg, seasons2002_2017)
summary(model2002_2017)

ggplot(data = seasons2002_2017, aes(three_fg, success)) +
  geom_point() +
  theme_stata() +
  labs(title = "The association between 3PT% and team success: 2002-2007", 
       x = "Three-point field goal percentage", 
       y = "Percentage of games won", 
       caption = "Source: FiveThirtyEight, Kaggle") +
  theme(panel.grid = element_blank(), 
        plot.title = element_text(hjust = 0.5), 
        plot.caption = element_text(hjust = -0.05),
        axis.title.y = element_text(vjust = 2), 
        axis.text.y = element_text(hjust = 0.5))

## 3PT FG PERCENTAGE, LINEAR REGRESSION, 2015-2017
three_fg1 <- nba_data_historical %>%
  filter(type == "RS", G > 10,
         share_3pt > 10, 
         year_id %in% 2015:2017) %>%
  group_by(team_id) %>%
  summarize(three_fg = mean(as.numeric(three_fg)))

seasons2015_2017 <- data.frame(
  team = three_fg1[, 1], 
  three_fg = three_fg1[, 2], 
  success = success_time(2015:2017)[, 2]
)

model2015_2017 <- lm(success ~ three_fg, seasons2015_2017) 
summary(model2015_2017)

ggplot(seasons2015_2017, aes(three_fg, success)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_stata() +
  labs(title = "The association between 3PT% and team success: 2015-2017", 
       x = "Three-point field goal percentage", 
       y = "Percentage of games won", 
       caption = "Source: FiveThirtyEight, Kaggle") +
  theme(panel.grid = element_blank(), 
        plot.title = element_text(hjust = 0.5), 
        plot.caption = element_text(hjust = -0.05),
        axis.title.y = element_text(vjust = 2), 
        axis.text.y = element_text(hjust = 0.5))

## RESIDUAL PLOT
seasons2015_2017 <- seasons2015_2017 %>%
  add_residuals(model2015_2017)

ggplot(seasons2015_2017[seasons2015_2017$resid > 0, ], 
       aes(reorder(team_id, resid), resid)) +
  geom_bar(stat = "identity", alpha = 0.9) +
  theme_stata() +
  labs(x = "Team", 
       y = "Residual", 
       title = "Teams that outperform projected success based on 3PT% — 2015 to 2017", 
       caption = "Source: FiveThirtyEight, Kaggle") +
  theme(panel.grid = element_blank(), 
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = -0.05), 
        axis.title.y = element_text(vjust = 2), 
        axis.text.y = element_text(hjust = 0.5))

seasons2015_2017_volume <- data.frame(
  team = volume_3pt[, 1], 
  share_3pt = volume_3pt[, 2], 
  success = success_time(2015:2017)[, 2]
)

summary(lm(seasons2015_2017_volume$success ~ seasons2015_2017_volume$share_3pt))

volume_3pt <- nba_data_historical %>%
  filter(type == "RS", G > 10,
         share_3pt > 10, 
         year_id %in% 2015:2017) %>%
  group_by(team_id) %>%
  summarize(share_3pt = mean(as.numeric(share_3pt)))

ggplot(seasons2015_2017_volume, aes(share_3pt, success)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_stata() +
  labs(
    title = "The association between three-point shooting and team success: 2015-2017", 
    x = "Proportion of shots from three-point range", 
    y = "Percentage of games won", 
    caption = "Source: FiveThirtyEight, Kaggle") +
  theme(panel.grid = element_blank(), 
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = -0.05), 
        axis.title.y = element_text(vjust = 2), 
        axis.text.y = element_text(hjust = 0.5))

arrange(volume_3pt, desc(share_3pt))

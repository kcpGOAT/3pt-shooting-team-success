library(tidyverse)
library(ggthemes)
history_3pt <- nba_data_historical %>%
  filter(G > 10, year_id > 1979) %>%
  group_by(year_id) %>%
  summarize(share_3pt = mean(share_3pt, na.rm = TRUE))

ggplot(history_3pt, aes(year_id, share_3pt)) +
  geom_line() +
  labs(x = "Year", 
       y = "Proportion of shots from three-point range",
       title = "The growth of three-point shooting in the NBA",
       caption = "Source: FiveThirtyEight") +
  theme_stata() +
  theme(panel.grid.major = element_blank(), 
        plot.title = element_text(hjust = 0.5), 
        plot.caption = element_text(hjust = -0.05),
        axis.title.y = element_text(vjust = 2),
        axis.title.x = element_text(vjust = -0.5),
        axis.text.y = element_text(hjust = 0.5))

######

hist_3pa <- Seasons_Stats %>%
  filter(Year > 1979, G > 10) %>%
  group_by(Tm, Year) %>%
  summarize(threept_attempts = sum(`3PA`)/82) %>%
  group_by(Year) %>%
  summarize(threept_attempts = mean(threept_attempts))

ggplot(hist_3pa, aes(Year, threept_attempts)) +
  geom_line() +
  labs(y = "Average team three-point attempts", 
       title = "The increase in the number of three-point shots per game", 
       caption = "Source: Kaggle") +
  theme_stata() +
  xlim(1980, 2020) +
  ylim(0, 30) +
  theme(panel.grid.major = element_blank(), 
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = -0.05),
        axis.title.y = element_text(vjust = 2),
        axis.title.x = element_text(vjust = -0.5),
        axis.text.y = element_text(hjust = 0.5))

#####

hist_efficiency <- Seasons_Stats %>%
  filter(Year >= 1980) %>%
  group_by(Year) %>%
  summarize(three_fg = sum(`3P`)/sum(`3PA`))
  
ggplot(hist_efficiency, aes(Year, three_fg)) +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = 1996) +
  theme_stata() +
  labs(y = "3FG%", 
       title = "NBA three-point efficiency: 1980-2017", 
       caption = "Source: Kaggle") +
  theme(panel.grid.major = element_blank(), 
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = -0.05),
        axis.title.y = element_text(vjust = 2),
        axis.title.x = element_text(vjust = -0.5),
        axis.text.y = element_text(hjust = 0.5))

## 3PT% - 2015-2017
  
threefg_2017 <- Seasons_Stats %>%
  filter(Year %in% 2015:2017, Tm != "TOT") %>%
  group_by(Tm) %>%
  summarize(threefg = sum(`3P`)/sum(`3PA`))

seasons2015_2017_new <- data.frame(
  team = threefg_2017[, 1], 
  threefg = threefg_2017[, 2], 
  success = success_time(2015:2017)[, 2]
)

summary(lm(seasons2015_2017_new$success ~ seasons2015_2017_new$threefg))

ggplot(seasons2015_2017_new, aes(threefg, success)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_stata() +
  labs(title = "The association between 3PT% and team success: 2015-2017", 
       x = "Three-point field goal percentage", 
       y = "Percentage of games won", 
       caption = "Source: Kaggle") +
  theme(panel.grid = element_blank(), 
        plot.title = element_text(hjust = 0.5), 
        plot.caption = element_text(hjust = -0.05),
        axis.title.y = element_text(vjust = 2), 
        axis.text.y = element_text(hjust = 0.5))

## volume - 2015-2017
volume_2017 <- Seasons_Stats %>%
  filter(Year %in% 2015:2017, Tm != "TOT") %>%
  group_by(Tm) %>%
  summarize(volume = sum(`3PA`)/sum(`FGA`))

seasons2015_2017_new_volume <- data.frame(
  team = volume_2017[, 1], 
  volume = volume_2017[, 2], 
  success = success_time(2015:2017)[, 2]
)

ggplot(seasons2015_2017_new_volume, aes(volume, success)) +
  geom_point() +
  theme_stata() +
  labs(title = "The association between three-point shooting and team success: 2015-2017", 
       x = "Proportion of shots from three-point range", 
       y = "Percentage of games won", 
       caption = "Source: Kaggle") +
  theme(panel.grid = element_blank(), 
        plot.title = element_text(hjust = 0.5), 
        plot.caption = element_text(hjust = -0.05),
        axis.title.y = element_text(vjust = 2), 
        axis.text.y = element_text(hjust = 0.5))
summary(lm(seasons2015_2017_new_volume$success ~ 
             seasons2015_2017_new_volume$volume))

## volume - 2002-2007
new_success_time <- function(time_range) {
  teams <- sort(unique(team_records$Team[team_records$Season %in% time_range]))
  
  success <- team_records %>%
    filter(Season %in% time_range) %>%
    group_by(Team, Season) %>%
    summarize(success = round(sum(W)/(sum(L) + sum(W)), digits = 2)) %>%
    arrange(Team)
  success[, 3]
}

volume_2007 <- Seasons_Stats %>%
  filter(Year %in% 2002:2007, Tm != "TOT") %>%
  group_by(Tm, Year) %>%
  summarize(volume = sum(`3PA`)/sum(`FGA`))

seasons2002_2007_new_volume <- data.frame(
  team = volume_2007[, 1],
  year = volume_2007[, 2],
  volume = volume_2007[, 3], 
  success = new_success_time(2002:2007)
)

ggplot(seasons2002_2007_new_volume, aes(volume, success)) +
  geom_point() +
  theme_stata() +
  facet_wrap(~Year) +
  labs(title = "The association between three-point shooting and team success: 2002-2007", 
       x = "Proportion of shots from three-point range", 
       y = "Percentage of games won", 
       caption = "Source: Kaggle") +
  theme(panel.grid = element_blank(), 
        plot.title = element_text(hjust = 0.5), 
        plot.caption = element_text(hjust = -0.05),
        axis.title.y = element_text(vjust = 2), 
        axis.text.y = element_text(hjust = 0.5))

volume_2007_whole <- Seasons_Stats %>%
  filter(Year %in% 2002:2007, Tm != "TOT") %>%
  group_by(Tm) %>%
  summarize(volume = sum(`3PA`)/sum(`FGA`))

seasons2002_2007_volume <- data.frame(
  team = volume_2007_whole[, 1],
  volume = volume_2007_whole[, 2], 
  success = success_time(2002:2007)[, 2]
)
summary(lm(seasons2002_2007_volume$success ~
             seasons2002_2007_volume$volume))

## threefg - 2002-2007
threefg_2007 <- Seasons_Stats %>%
  filter(Year %in% 2002:2007, Tm != "TOT") %>%
  group_by(Tm, Year) %>%
  summarize(threefg = sum(`3P`)/sum(`3PA`))

threefg_2007_seasons <- data.frame(
  team = threefg_2007[, 1],
  year = threefg_2007[, 2],
  threefg = threefg_2007[, 3], 
  success = new_success_time(2002:2007)
)

ggplot(threefg_2007_seasons, aes(threefg, success)) +
  geom_point() +
  theme_stata() +
  facet_wrap(~Year) +
  labs(title = "The association between 3PT% and team success: 2002-2007", 
       x = "Three-point field goal percentage", 
       y = "Percentage of games won", 
       caption = "Source: Kaggle") +
  theme(panel.grid = element_blank(), 
        plot.title = element_text(hjust = 0.5), 
        plot.caption = element_text(hjust = -0.05),
        axis.title.y = element_text(vjust = 2), 
        axis.text.y = element_text(hjust = 0.5))

threefg_2007_whole <- Seasons_Stats %>%
  filter(Year %in% 2002:2007, Tm != "TOT") %>%
  group_by(Tm) %>%
  summarize(threefg = sum(`3P`)/sum(`3PA`))

seasons2002_2007_threefg <- data.frame(
  team = threefg_2007_whole[, 1],
  threefg = threefg_2007_whole[, 2], 
  success = success_time(2002:2007)[, 2]
)
summary(lm(seasons2002_2007_threefg$success ~
             seasons2002_2007_threefg$threefg))


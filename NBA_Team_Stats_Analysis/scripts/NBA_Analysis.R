# Load the Necessary Libraries
library(tidyverse)
library(here)
library(lubridate)
library(aod)

# Read in the csv file as a dataframe and cleans up some of the formatting
data <- read_csv(here("data", "nba.games.stats.csv"))
data <- data %>%
  mutate(Season = ifelse(month(Date) > 9,
                         paste(year(Date), "-", year(Date) + 1),
                         paste(year(Date) - 1, "-", year(Date))),
         X1 = NULL)

# Sets the team colors to make bars more distinguishable when creating bar graphs
primary_team_colors <- c("#E03A3E", "#007A33", "#000000", "#CE1141", "#1D1160",
                         "#860038", "#00538C", "#0E2240", "#C8102E", "#1D428A",
                         "#CE1141", "#002D62", "#C8102E", "#552583", "#5D76A9",
                         "#98002E", "#00471B", "#0C2340", "#0C2340", "#006BB6",
                         "#007AC1", "#0077C0", "#006BB6", "#1D1160", "#E03A3E",
                         "#5A2D81", "#C4CED4", "#CE1141", "#002B5C", "#002B5C")
secondary_team_colors <- c("#C1D32F", "#BA9653", "#FFFFFF", "#000000", "#00788C",
                           "#041E42", "#002B5E", "#FEC524", "#1D42BA", "#FFC72C",
                           "#000000", "#FDBB30", "#1D428A", "#FDB927", "#12173F",
                           "#F9A01B", "#EEE1C6", "#236192", "#C8102E", "#F58426",
                           "#EF3B24", "#C4CED4", "#ED174C", "#E56020", "#000000",
                           "#63727A", "#000000", "#000000", "#00471B", "#E31837")

# Filters dataframe to show only wins for each team and then aggregates by the
# number of wins for each team, for each season
season_wins <- data %>%
  filter(WINorLOSS == "W") %>%
  count(Team, Season) %>%
  rename(Wins = "n")

# Creates a barplot showing the number of wins for each team during each season
season_wins_plot <- ggplot(season_wins) +
  geom_col(aes(Team, Wins), fill = rep(primary_team_colors, 4), color = 'black') +
  facet_wrap(~Season) +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()) +
  geom_text(aes(Team, Wins, label = Team),
            color = rep(secondary_team_colors, each = 4),
            angle = 90, size = 3, hjust = 1.25, fontface = "bold") +
  ggtitle("Number of Regular Season Wins for Each NBA Team, 2014-2018")

season_wins_plot


# Comparing 3-point pct. with Free Throw Pct.
three_pt_vs_free_throw <- ggplot(data) +
  geom_point(aes(X3PointShots., FreeThrows.)) +
  xlab("3 Point Percentage") +
  ylab("Free Throw Percentage") +
  ggtitle("3-Point Percentage vs. Free Throw Percentage")

three_pt_vs_free_throw

# Calculating Correlation Coefficient
cor(data$X3PointShots., data$FreeThrows., method = "pearson")
cor.test(data$X3PointShots., data$FreeThrows., method = "pearson")

# Comparing the League Average in different metrics across a whole season,
# for each season
league_avgs <- data %>%
  group_by(Game) %>%
  summarise(Points = mean(TeamPoints),
            FieldGoalPct = mean(FieldGoals.),
            ThreePointPct = mean(X3PointShots.),
            FreeThrowPct = mean(FreeThrows.),
            Assists = mean(Assists),
            Turnovers = mean(Turnovers),
            Fouls = mean(TotalFouls)) %>%
  gather(variable, value, Points:Fouls, factor_key = TRUE)

league_avgs_plot <- ggplot(league_avgs) +
  geom_line(aes(Game, value)) +
  facet_wrap(~variable, scales = "free_y") +
  ggtitle("NBA League Averages Across different team metrics over the course of
          a season")
league_avgs_plot

# Season Field Goal Pct Averages, Home vs Away, Across 4 Different Seasons
season_fgpct <- data %>%
  group_by(Season, Team, Home) %>%
  summarise(FieldGoalPct = sum(FieldGoals)/sum(FieldGoalsAttempted))

season_fgpct_plot <- ggplot(season_fgpct) +
  geom_col(aes(Team, FieldGoalPct, alpha = Home),
           position = "dodge",
           fill = rep(primary_team_colors, 4, each = 2)) +
  facet_wrap(~Season, scales = "free_y") +
  ylab("Season Field Goal Percentage") +
  ggtitle(label = "Season Field Goal Percentages for Each Team",
          subtitle = "(Home vs. Away, Across 4 Different Seasons)") +
  geom_text(aes(Team, FieldGoalPct, label = Team, group = Home),
            color = rep(secondary_team_colors, 4, each = 2),
            angle = 90, size = 1.75, hjust = 1.25, vjust = 0.25,
            position = position_dodge(width = 0.9),
            fontface = "bold") +
  scale_alpha_manual(values = c(0.5, 1)) +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank())

season_fgpct_plot

# Does the same as above but with league percentages instead of team percentages
league_fgpct <- data %>% 
  group_by(Season, Home) %>% 
  summarise(FieldGoalPct = sum(FieldGoals)/sum(FieldGoalsAttempted),
            FieldGoals = sum(FieldGoals),
            FieldGoalsAttempted = sum(FieldGoalsAttempted))

league_fgpct_plot <- ggplot(league_fgpct) +
  geom_col(aes(Home, FieldGoalPct, fill = Home)) +
  facet_wrap(~Season) +
  scale_fill_manual(values = c("#FF0000", "#00FF00")) +
  theme(axis.title.x = element_blank()) +
  geom_text(aes(Home, FieldGoalPct, label = round(FieldGoalPct, 3)),
            vjust = 1)

league_fgpct_plot

# Using 2-proportion t-test to calculate p-value and confidence intervals for
# percentage differences between Home and Away Games
league_fgpct <- league_fgpct %>%
  pivot_wider(names_from = Home,
              values_from =c(FieldGoalPct, FieldGoals, FieldGoalsAttempted))

res_2014_2015 <- prop.test(x = c(league_fgpct$FieldGoals_Home[[1]],
                                 league_fgpct$FieldGoals_Away[[1]]),
                           n = c(league_fgpct$FieldGoalsAttempted_Home[[1]],
                                 league_fgpct$FieldGoalsAttempted_Away[[1]]))
res_2015_2016 <- prop.test(x = c(league_fgpct$FieldGoals_Home[[2]],
                                 league_fgpct$FieldGoals_Away[[2]]),
                           n = c(league_fgpct$FieldGoalsAttempted_Home[[2]],
                                 league_fgpct$FieldGoalsAttempted_Away[[2]]))
res_2016_2017 <- prop.test(x = c(league_fgpct$FieldGoals_Home[[3]],
                                 league_fgpct$FieldGoals_Away[[3]]),
                           n = c(league_fgpct$FieldGoalsAttempted_Home[[3]],
                                 league_fgpct$FieldGoalsAttempted_Away[[3]]))
res_2017_2018 <- prop.test(x = c(league_fgpct$FieldGoals_Home[[4]],
                                 league_fgpct$FieldGoals_Away[[4]]),
                           n = c(league_fgpct$FieldGoalsAttempted_Home[[4]],
                                 league_fgpct$FieldGoalsAttempted_Away[[4]]))
overall_res <- prop.test(x = c(sum(league_fgpct$FieldGoals_Home),
                               sum(league_fgpct$FieldGoals_Away)),
                         n = c(sum(league_fgpct$FieldGoalsAttempted_Home),
                               sum(league_fgpct$FieldGoalsAttempted_Away)))

res_2014_2015
res_2015_2016
res_2016_2017
res_2017_2018
overall_res

# Comparing the percentage difference between seasons
over_the_years_comparison <- ggplot(league_fgpct, aes(group = 1)) +
  geom_line(aes(Season, FieldGoalPct_Home), color = 'green') +
  geom_point(aes(Season, FieldGoalPct_Home)) +
  geom_line(aes(Season, FieldGoalPct_Away), color = 'red') +
  geom_point(aes(Season, FieldGoalPct_Away))

over_the_years_comparison

# Creating a Logistic Regression Model to Predict the Likelihood of Winning,
# given the individual stats for a team
data$WINorLOSS <- factor(data$WINorLOSS)
data$Home <- factor(data$Home)
mylogit <- glm(WINorLOSS ~ Home + FieldGoals. + X3PointShots. + FreeThrows +
                 TotalRebounds + Assists + Steals + Blocks + Turnovers +
                 TotalFouls, data = data, family = "binomial")
summary(mylogit)

# Tests whether the effect of playing at Home or Away is statistically significant
wald.test(b = coef(mylogit), Sigma = vcov(mylogit), Terms = 2)

# Tests the probability of winning at Home vs Away, keeping everything else equal
newdata <- with(data, data.frame(FieldGoals. = mean(FieldGoals.),
                                 X3PointShots. = mean(X3PointShots.),
                                 FreeThrows = mean(FreeThrows),
                                 TotalRebounds = mean(TotalRebounds),
                                 Assists = mean(Assists),
                                 Steals = mean(Steals),
                                 Blocks = mean(Blocks),
                                 Turnovers = mean(Turnovers),
                                 TotalFouls = mean(TotalFouls),
                                 Home = factor(c("Home", "Away"))))
newdata$PredProb <- predict(mylogit, newdata = newdata, type = "response")

# Tests the probability of Winning At Home vs. Away, at different values
# for field goal percentage
newdata <- with(data, data.frame(FieldGoals. = rep(seq(from = .20, to = .80,
                                                       length.out = 1000)),
                                 X3PointShots. = mean(X3PointShots.),
                                 FreeThrows = mean(FreeThrows),
                                 TotalRebounds = mean(TotalRebounds),
                                 Assists = mean(Assists),
                                 Steals = mean(Steals),
                                 Blocks = mean(Blocks),
                                 Turnovers = mean(Turnovers),
                                 TotalFouls = mean(TotalFouls),
                                 Home = factor(rep(c("Home", "Away"),
                                                   each = 1000))))
newdata <- cbind(newdata, predict(mylogit, newdata = newdata, type = "link",
                                  se = TRUE))
newdata <- within(newdata, {
  PredProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})

# Constructing a graph of the probability of winning at different field goal
# percentages, stratifying based on Home vs. Away
ggplot(newdata, aes(x = FieldGoals., y = PredProb)) + 
  geom_ribbon(aes(ymin = LL, ymax = UL, fill = Home), alpha = 0.2) +
  geom_line(aes(colour = Home), size = 1) +
  xlim(0.4, 0.5) +
  xlab("Field Goal Percentage") +
  ylab("Predicted Probability") +
  ggtitle("Probablility of Winning an NBA Game given different field goal
          percentages")

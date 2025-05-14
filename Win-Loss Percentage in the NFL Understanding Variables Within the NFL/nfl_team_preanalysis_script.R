#
rm(list = ls())

install.packages("readr")
library("readr")


str(nfl_R)




# Perform multivariable regression
model <- lm(win_loss_perc ~ points_scored_per_game + points_opp_per_game + total_yards_per_game + 
              plays_offense_per_game + yds_per_play_offense + turnovers_per_game + 
              fumbles_lost_per_game + first_downs_per_game + pass_cmp_per_game + 
              pass_att_per_game + pass_yds_per_game + pass_td_per_game + 
              pass_int_per_game + pass_net_yds_per_att + pass_fd_per_game + 
              rush_att_per_game + rush_yards_per_game + rush_td_per_game + 
              rush_yds_per_att + rush_fd_per_game + penalties_per_game + 
              penalties_yds_per_game + pen_fd_per_game + score_pct + turnover_pct,
            data = nfl_R)
summary(nfl_R)
summary(model)

model2 <- lm(win_loss_perc ~ total_yards_per_game + 
                        plays_offense_per_game + yds_per_play_offense + turnovers_per_game + 
                        fumbles_lost_per_game + first_downs_per_game + pass_cmp_per_game + 
                        pass_att_per_game + pass_yds_per_game + pass_td_per_game + 
                        pass_int_per_game + pass_net_yds_per_att + pass_fd_per_game + 
                        rush_att_per_game + rush_yards_per_game + rush_td_per_game + 
                        rush_yds_per_att + rush_fd_per_game + penalties_per_game + 
                        penalties_yds_per_game + pen_fd_per_game + score_pct + turnover_pct,
                      data = nfl_R)
summary(model2)

model3 <- lm(win_loss_perc ~ total_yards_per_game + 
                        plays_offense_per_game + yds_per_play_offense + turnovers_per_game + 
                        fumbles_lost_per_game + first_downs_per_game + pass_cmp_per_game + 
                        pass_att_per_game + pass_yds_per_game + 
                        pass_int_per_game + pass_net_yds_per_att + pass_fd_per_game + 
                        rush_att_per_game + rush_yards_per_game +
                        rush_yds_per_att + rush_fd_per_game + penalties_per_game + 
                        penalties_yds_per_game + pen_fd_per_game + score_pct + turnover_pct,
                      data = nfl_R)
summary(model3)

model4 <- lm(win_loss_perc ~ total_yards_per_game +
                                  plays_offense_per_game + yds_per_play_offense + turnovers_per_game + 
                                  fumbles_lost_per_game + first_downs_per_game + pass_cmp_per_game + 
                                  pass_att_per_game + pass_yds_per_game + 
                                  pass_int_per_game + pass_net_yds_per_att + pass_fd_per_game + 
                                  rush_att_per_game + rush_yards_per_game +
                                  rush_yds_per_att + rush_fd_per_game + penalties_per_game + 
                                  penalties_yds_per_game + pen_fd_per_game + turnover_pct,
                                data = nfl_R)
summary(model4)

# Fit linear regression model
model5 <- lm(win_loss_perc ~ total_yards_per_game, data = nfl_R)

# Summary of the regression model
summary(model5)

# Scatter plot
plot(nfl_R$total_yards_per_game, nfl_R$win_loss_perc, 
     main = "Winning Percentage \nvs Total Yards Per Game", 
     xlab = "Total Yards Per Game", ylab = "Winning Percentage")

# Add regression line
abline(model5, col = "red", lwd = 2)


model6 <- lm(win_loss_perc ~ first_downs_per_game , data = nfl_R)
summary(model6)

# Scatter plot
plot(nfl_R$first_downs_per_game, nfl_R$win_loss_perc, 
     main = "Winning Percentage \nvs First Downs Per Game", 
     xlab = "First Downs", ylab = "Winning Percentage")

# Add regression line
abline(model6, col = "red", lwd = 2)

model7 <- lm(win_loss_perc ~ total_yards_per_game + first_downs_per_game, data = nfl_R)
summary(model7)

summary(model4)

model8<- lm(win_loss_perc ~ turnovers_per_game, data = nfl_R)
summary(model8)


# Scatter plot
plot(nfl_R$turnovers_per_game, nfl_R$win_loss_perc, 
     main = "Winning Percentage \nvs Turnovers Per Game", 
     xlab = "Turnovers Per Game", ylab = "Winning Percentage")

# Add regression line
abline(model8, col = "red", lwd = 2)

model9<- lm(win_loss_perc ~ turnover_pct, data = nfl_R)
summary(model9)


# Scatter plot
plot(nfl_R$turnover_pct, nfl_R$win_loss_perc, 
     main = "Winning Percentage \nvs turnover_pct", 
     xlab = "Turnover Percentage", ylab = "Winning Percentage")

# Add regression line
abline(model9, col = "red", lwd = 2)

summary(model8)
summary(model9)


model10<- lm(win_loss_perc ~ turnovers_per_game + turnover_pct, data = nfl_R)
summary(model8)
summary(model9)
summary(model10)

model11<- lm(win_loss_perc ~ penalties_per_game, data = nfl_R)
summary(model11)


# Scatter plot
plot(nfl_R$penalties_per_game, nfl_R$win_loss_perc, 
     main = "Winning Percentage \nvs Penalties Per Game", 
     xlab = "Penalties Per Game", ylab = "Winning Percentage")

# Add regression line
abline(model11, col = "red", lwd = 2)

model12<- lm(win_loss_perc ~ penalties_yds_per_game, data = nfl_R)
summary(model12)


# Scatter plot
plot(nfl_R$penalties_yds_per_game, nfl_R$win_loss_perc, 
     main = "Winning Percentage \nvs Penalty Yards Per Game", 
     xlab = "Penalty Yards Per Game", ylab = "Winning Percentage")

# Add regression line
abline(model12, col = "red", lwd = 2)
summary(model11)
summary(model12)

model13<- lm(win_loss_perc ~ penalties_per_game + penalties_yds_per_game, data = nfl_R)
summary(model13)

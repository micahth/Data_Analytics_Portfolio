#rm(list = ls())

install.packages("readr")
library("readr")

dim(nfl_R)
  
str(nfl_R)
#split into training and verification data

install.packages("caret")
library(caret)
set.seed(123)

trainIndex <- createDataPartition(nfl_R$win_loss_perc, p = 0.8, 
                                  list = FALSE, 
                                  times = 1)
training_data <- nfl_R[trainIndex, ]
testing_data <- nfl_R[-trainIndex, ]
summary(training_data)
print(training_data)
summary(testing_data)
print(testing_data)


#regression on training data

model <- lm(win_loss_perc ~ points_scored_per_game + points_opp_per_game + total_yards_per_game + 
              plays_offense_per_game + yds_per_play_offense + turnovers_per_game + 
              fumbles_lost_per_game + first_downs_per_game + pass_cmp_per_game + 
              pass_att_per_game + pass_yds_per_game + pass_td_per_game 
              + pass_net_yds_per_att + pass_fd_per_game + 
              rush_att_per_game + rush_td_per_game + 
              rush_yds_per_att + rush_fd_per_game + penalties_per_game + 
              penalties_yds_per_game + score_pct + turnover_pct,
            data = training_data)


summary(model)




coefficients <- coef(model)


equation <- paste("win_loss_perc =", paste0(coefficients[-1], "* ", names(coefficients)[-1], collapse = " + "), "+", coefficients[1])


print(equation)

# Make predictions on testing data
predictions <- predict(model, newdata = testing_data)

# Calculate absolute differences
abs_diff <- abs(predictions - testing_data$win_loss_perc)

# Calculate MAD
mad_score <- mean(abs_diff)

# Print MAD score
print(mad_score)





#model_no_points
model_no_points <- lm(win_loss_perc ~ total_yards_per_game + 
               plays_offense_per_game + yds_per_play_offense + turnovers_per_game + 
               fumbles_lost_per_game + first_downs_per_game + pass_cmp_per_game + 
               pass_att_per_game + pass_yds_per_game + pass_td_per_game 
             + pass_net_yds_per_att + pass_fd_per_game + 
               rush_att_per_game + rush_td_per_game + 
               rush_yds_per_att + rush_fd_per_game + penalties_per_game + 
               penalties_yds_per_game + score_pct + turnover_pct,
             data = training_data)


summary(model_no_points)

coefficients_no_points <- coef(model_no_points)


equation_no_points <- paste("win_loss_perc =", paste0(coefficients_no_points[-1], "* ", names(coefficients_no_points)[-1], collapse = " + "), "+", coefficients[1])


print(equation_no_points)

# Make testing data predictions 
Predictions_no_points <- predict(model_no_points, newdata = testing_data)

# Calculate absolute differences
abs_diff_no_points <- abs(Predictions_no_points - testing_data$win_loss_perc)

# Calculate MAD
mad_score_no_points<- mean(abs_diff_no_points)

# Print MAD score
print(mad_score_no_points)



#run backwards and forwards step regression on training data

# Backward stepwise regression
backward_model <- step(model, direction = "backward")

# Forward stepwise 
empty_model <- lm(win_loss_perc ~ 1, data = training_data)

forward_model <- step(empty_model, direction = "forward", Trace = TRUE, scope = list(lower=empty_model, upper=backward_model) )

# Print summaries of the models
summary(backward_model)
summary(forward_model)

#test data (MAD, MSE, RMSE)

# Generate predictions on the test data using the backward stepwise model
backward_predictions <- predict(backward_model, newdata = testing_data)

# Generate predictions on the test data using the forward stepwise model
forward_predictions <- predict(forward_model, newdata = testing_data)

# Calculate Mean Absolute Deviation (MAD)
backward_mad <- mean(abs(backward_predictions - testing_data$win_loss_perc))
forward_mad <- mean(abs(forward_predictions - testing_data$win_loss_perc))

# Calculate Mean Squared Error (MSE)
backward_mse <- mean((backward_predictions - testing_data$win_loss_perc)^2)
forward_mse <- mean((forward_predictions - testing_data$win_loss_perc)^2)

# Calculate Root Mean Squared Error (RMSE)
backward_rmse <- sqrt(backward_mse)
forward_rmse <- sqrt(forward_mse)

# Print the results
cat("Backward Stepwise Regression:\n")
cat("Bakward MAD:", backward_mad, "\n")
cat("Backward MSE:", backward_mse, "\n")
cat("Backward RMSE:", backward_rmse, "\n\n")


cat("Forward Stepwise Regression:\n")
cat("Forward MAD:", forward_mad, "\n")
cat("Forward MSE:", forward_mse, "\n")
cat("Forward RMSE:", forward_rmse, "\n")

# Backward no points stepwise regression
backward_model_no_points <- step(model_no_points, direction = "backward")

# Forward no points stepwise 
empty_model_no_points <- lm(win_loss_perc ~ 1, data = training_data)

forward_model_no_points <- step(empty_model_no_points, direction = "forward", Trace = TRUE, scope = list(lower=empty_model_no_points, upper=backward_model_no_points) )

# Print summaries
summary(backward_model_no_points)
summary(forward_model_no_points)

#test data (MAD)

# Generate predictions on the test data using the backward stepwise model
backward_predictions_no_points <- predict(backward_model_no_points, newdata = testing_data)


# Generate predictions on test data using the forward stepwise  no points model
forward_predictions_no_points <- predict(forward_model_no_points, newdata = testing_data)

# Calculate Mean Absolute Deviation (MAD)
backward_mad_no_points <- mean(abs(backward_predictions_no_points - testing_data$win_loss_perc))
forward_mad_no_points <- mean(abs(forward_predictions_no_points - testing_data$win_loss_perc))


# Print

cat("Badkward No Points MAD:", backward_mad_no_points, "\n")



cat("Forward No Points MAD:", forward_mad_no_points, "\n")






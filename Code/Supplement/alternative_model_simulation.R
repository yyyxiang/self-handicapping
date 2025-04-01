library(tidyverse)

alternative_model_fitted_params <- read.csv('alternative_model_fitted_params.csv', header = T, stringsAsFactors = T)

c_seq <- seq(0, 1, by = 0.1) * 20 # competence is defined as the max number of correct answers
gamma_seq <- c(.5, 1)
d <- 8
prior_c_dist <- rep(1/length(c_seq), length(c_seq))

prob_success <- function(c, gamma, d, k = signaling_model_fitted_params$k, b = signaling_model_fitted_params$b) { # k controls steepness of the curve
  1 / (1 + exp(-k * (gamma * c - d - b)))
}

prediction_df <- NULL
for (mdl in c('self-deception', 'hide-incompetence')) {
  fitted_params <- alternative_model_fitted_params %>% filter(model == mdl)
  for (exp in 1:2) {
    if (exp == 1) {
      inverse_temp <- fitted_params$exp1_inverse_temp
    } else {
      inverse_temp <- fitted_params$exp2_inverse_temp
    }
    
    optimal_gamma_df <- NULL
    for (true_c in c_seq) {
      if (mdl == 'self-deception') {
        values_df <- data.frame(exp_index = paste0('exp', exp), true_c = true_c, true_accuracy = round(true_c/20, 1), gamma = gamma_seq, 
                                value = -fitted_params$alpha * (gamma_seq - fitted_params$beta)^2)
      } else if (mdl == 'hide-incompetence') {
        values_df <- data.frame(exp_index = paste0('exp', exp), true_c = true_c, true_accuracy = round(true_c/20, 1), gamma = gamma_seq, 
                                value = -fitted_params$alpha * (gamma_seq - true_c/20 - fitted_params$beta)^2)
      }
      values_df$prob <- exp(values_df$value * inverse_temp) / sum(exp(values_df$value * inverse_temp))
      optimal_gamma_df <- rbind(optimal_gamma_df, values_df)
    }
    
    actor_prediction <- optimal_gamma_df %>% 
      filter(gamma == 0.5) %>% 
      mutate(probability_10 = prob*100) %>%  
      dplyr::rename(accuracy = true_accuracy) %>% 
      select(exp_index, accuracy, probability_10) %>% 
      mutate(block = 'actor') %>% mutate(model = mdl)

    prediction_df <- rbind(prediction_df, actor_prediction) 
  }
}

prediction_df <- prediction_df %>% arrange(model, exp_index)
write.csv(prediction_df,'alternative_model_prediction.csv', row.names = FALSE)

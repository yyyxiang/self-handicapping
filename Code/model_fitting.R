library(tidyverse)
library(optimx)

c_seq <- seq(0, 1, by = 0.1) * 20 # competence is defined as the max number of correct answers
gamma_seq <- c(.5, 1)
d <- 8
prob_success <- function(c, gamma, d, k, b) { # k controls steepness of the curve, b is the x value of the sigmoid midpoint
  1 / (1 + exp(-k * (gamma * c - d - b)))
}

prior_c_dist <- rep(1/length(c_seq), length(c_seq))
dat <- read.csv('./../Data/data.csv', header = T, stringsAsFactors = T)

fit_model <- function(params) {
  total_loss <- 0
  
  for (exp in 1:2) {
    
    w <- ifelse(exp == 1, 1, 0)
    # w = 1: the actor's goal is to maximize perceived competence
    # w = 0: the actor's goal is to pass
    
    k <- params[1]
    b <- params[2]
    inverse_temp <- ifelse(exp == 1, params[3], params[4])
    
    optimal_gamma_df <- NULL
    for (true_c in c_seq) {
      values_df <- NULL
      for (gamma in gamma_seq) {
        # naive observer
        prior_c <- prior_c_dist
        df <- data.frame(c = c_seq, gamma = gamma, prior_c = prior_c) %>% 
          rowwise() %>% 
          mutate(success_likelihood = prob_success(c, gamma, d, k, b)) %>% 
          ungroup() %>% 
          mutate(failure_likelihood = 1 - success_likelihood)
        if (sum(df$success_likelihood) == 0 | sum(df$failure_likelihood) == 0) {
          return(Inf)
        } 
        df <- df %>% 
          mutate(success_posterior = prior_c * success_likelihood,
                 failure_posterior = prior_c * failure_likelihood) %>% # apply Bayes' rule
          mutate(success_posterior = success_posterior / sum(success_posterior),
                 failure_posterior = failure_posterior / sum(failure_posterior)) # normalize
        failure_expectation <- sum(df$c * df$failure_posterior) # E[c|s=1, gamma]
        success_expectation <- sum(df$c * df$success_posterior) # E[c|s=0, gamma]
        c_hat <- df$success_likelihood[df$c == true_c] * success_expectation + df$failure_likelihood[df$c == true_c] * failure_expectation
        
        # actor
        performance <- df$success_likelihood[df$c == true_c]
        value = w * c_hat + (1-w) * performance
        values_df <- rbind(values_df, data.frame(w = w, true_c = true_c, true_accuracy = round(true_c/20, 1), gamma = gamma, value = value))
      }
      values_df$prob <- exp(values_df$value * inverse_temp) / sum(exp(values_df$value * inverse_temp))
      optimal_gamma_df <- rbind(optimal_gamma_df, values_df)
    }
    
    prediction_df <- optimal_gamma_df %>% 
      filter(gamma == 0.5) %>% 
      mutate(prediction = prob*100) %>%  
      dplyr::rename(accuracy = true_accuracy) %>% 
      select(-value, -prob, -gamma)
    
    loss <- dat %>% 
      filter(block == 'actor' & exp_index == paste0('exp', exp)) %>% 
      group_by(accuracy) %>% 
      dplyr::summarize(p = mean(probability_10)) %>% 
      merge(prediction_df) %>% 
      mutate(loss = (p - prediction)^2) %>%
      pull(loss) %>% 
      sum()
    
    total_loss <- total_loss + loss
  }
  
  return(total_loss)
}

fit = optim(par = c(1, 1, 1, 1),
            fn = fit_model,
            method = 'Nelder-Mead')
fitted_params <- data.frame(k = fit$par[1], b = fit$par[2], exp1_inverse_temp = fit$par[3], exp2_inverse_temp = fit$par[4])
write.csv(fitted_params,'fitted_params.csv', row.names = FALSE)

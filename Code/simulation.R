library(tidyverse)

fitted_params <- read.csv('fitted_params.csv', header = T, stringsAsFactors = T)

c_seq <- seq(0, 1, by = 0.1) * 20 # competence is defined as the max number of correct answers
gamma_seq <- c(.5, 1)
d <- 8
prior_c_dist <- rep(1/length(c_seq), length(c_seq))

prob_success <- function(c, gamma, d, k = fitted_params$k, b = fitted_params$b) { # k controls steepness of the curve
  1 / (1 + exp(-k * (gamma * c - d - b)))
}

prediction_df <- NULL
for (exp in 1:2) {
  if (exp == 1) {
    w <- 1 # the actor's goal is to maximize perceived competence
    inverse_temp <- fitted_params$exp1_inverse_temp
  } else {
    w <- 0 # the actor's goal is to pass
    inverse_temp <- fitted_params$exp2_inverse_temp
  }
  
  naive_df <- NULL
  optimal_gamma_df <- NULL
  for (true_c in c_seq) {
    values_df <- NULL
    for (gamma in gamma_seq) {
      # naive observer
      prior_c <- prior_c_dist
      df <- data.frame(c = c_seq, gamma = gamma, prior_c = prior_c) %>% 
        rowwise() %>% 
        mutate(success_likelihood = prob_success(c, gamma, d)) %>% 
        ungroup() %>% 
        mutate(failure_likelihood = 1 - success_likelihood) %>% 
        mutate(success_posterior = prior_c * success_likelihood,
               failure_posterior = prior_c * failure_likelihood) %>% # apply Bayes' rule
        mutate(success_posterior = success_posterior / sum(success_posterior),
               failure_posterior = failure_posterior / sum(failure_posterior)) # normalize
      
      failure_posterior_str <- paste(df$failure_posterior, collapse = ';')
      success_posterior_str <- paste(df$success_posterior, collapse = ';')
      failure_expectation <- sum(df$c * df$failure_posterior) # E[c|s=1, gamma]
      success_expectation <- sum(df$c * df$success_posterior) # E[c|s=0, gamma]
      
      if (true_c == c_seq[1]) { # naive observer's evaluations don't depend on true_c, so we're saving it only once
        naive_df <- rbind(naive_df, 
                          data.frame(exp_index = paste0('exp', exp), gamma = gamma, observer = 'naive', block = 'observer',
                                     fail = failure_expectation,pass = success_expectation,
                                     posterior_fail = failure_posterior_str, posterior_pass = success_posterior_str))
      }
      
      # actor
      c_hat <- df$success_likelihood[df$c == true_c] * success_expectation + df$failure_likelihood[df$c == true_c] * failure_expectation
      performance <- df$success_likelihood[df$c == true_c]
      value = w * c_hat + (1-w) * performance
      values_df <- rbind(values_df, data.frame(exp_index = paste0('exp', exp), true_c = true_c, true_accuracy = round(true_c/20, 1), gamma = gamma, value = value))
    }
    values_df$prob <- exp(values_df$value * inverse_temp) / sum(exp(values_df$value * inverse_temp))
    optimal_gamma_df <- rbind(optimal_gamma_df, values_df)
  }
  
  actor_prediction <- optimal_gamma_df %>% 
    filter(gamma == 0.5) %>% 
    mutate(probability_10 = prob*100) %>%  
    dplyr::rename(accuracy = true_accuracy) %>% 
    select(exp_index, accuracy, probability_10) %>% 
    mutate(block = 'actor')
  
  # sophisticated observer
  sophisticated_df <- NULL
  updated_c <- optimal_gamma_df %>% 
    mutate(prior_c = rep(prior_c, each = length(gamma_seq))) %>% 
    mutate(posterior = prior_c * prob) %>% 
    group_by(gamma) %>% 
    mutate(posterior = posterior/sum(posterior)) # normalize
  
  for (gamma in gamma_seq) {
    prior_c <- updated_c$posterior[updated_c$gamma == gamma] # P(c|gamma) becomes the new prior
    df <- data.frame(c = c_seq, gamma = gamma, prior_c = prior_c) %>% 
      rowwise() %>% 
      mutate(success_likelihood = prob_success(c, gamma, d)) %>% 
      ungroup() %>% 
      mutate(failure_likelihood = 1 - success_likelihood) %>% 
      mutate(success_posterior = prior_c * success_likelihood,
             failure_posterior = prior_c * failure_likelihood) %>%
      mutate(success_posterior = success_posterior / sum(success_posterior),
             failure_posterior = failure_posterior / sum(failure_posterior))
    
    failure_posterior_str <- paste(df$failure_posterior, collapse = ';')
    success_posterior_str <- paste(df$success_posterior, collapse = ';')
    failure_expectation <- sum(df$c * df$failure_posterior)
    success_expectation <- sum(df$c * df$success_posterior)
    sophisticated_df <- rbind(sophisticated_df, 
                              data.frame(exp_index = paste0('exp', exp), gamma = gamma, observer = 'sophisticated', block = 'observer',
                                         fail = failure_expectation,pass = success_expectation,
                                         posterior_fail = failure_posterior_str, posterior_pass = success_posterior_str))
  }
  observer_prediction <- rbind(naive_df, sophisticated_df) %>% 
    mutate(answers = case_when(gamma == 0.5 ~ 10,
                               T ~ 20)) %>% 
    mutate(pass = as.character(pass),
           fail = as.character(fail)) %>% 
    pivot_longer(cols = c(pass, fail, posterior_pass, posterior_fail), names_to = 'key', values_to = 'value') %>% 
    mutate(outcome = paste0(ifelse(grepl('pass', key), 'pass', 'fail'), answers),
           value_type = ifelse(grepl('posterior', key), 'posterior', 'evaluation')) %>% 
    select(-answers, -key) %>% 
    pivot_wider(names_from = value_type, values_from = value) %>%
    mutate(evaluation = as.numeric(evaluation),
           evaluation = evaluation / 20 * 100) %>%
    select(exp_index, block, observer, outcome, evaluation, posterior)
  
  exp_df <- dplyr::bind_rows(actor_prediction, observer_prediction)
  prediction_df <- rbind(prediction_df, exp_df)
}

prediction_df <- prediction_df %>% arrange(exp_index, block, observer, outcome)
write.csv(prediction_df,'model_prediction.csv', row.names = FALSE)

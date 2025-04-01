library(tidyverse)
library(optimx)

c_seq <- seq(0, 1, by = 0.1) * 20 # competence is defined as the max number of correct answers
gamma_seq <- c(.5, 1)
dat <- read.csv('./../../Data/data.csv', header = T, stringsAsFactors = T)

fit_model <- function(params, model) {
  total_loss <- 0
  
  for (exp in 1:2) {
    alpha <- params[1]
    beta <- params[2]
    inverse_temp <- ifelse(exp == 1, params[3], params[4])
    
    optimal_gamma_df <- NULL
    for (true_c in c_seq) {
      if (model == 'self-deception') {
        values_df <- data.frame(true_c = true_c, true_accuracy = round(true_c/20, 1), gamma = gamma_seq, 
                                value = -alpha * (gamma_seq - beta)^2)
      } else if (model == 'hide-incompetence') {
        values_df <- data.frame(true_c = true_c, true_accuracy = round(true_c/20, 1), gamma = gamma_seq, 
                                value = -alpha * (gamma_seq - true_c/20 - beta)^2)
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

fitted_params <- NULL
for (mdl in c('self-deception', 'hide-incompetence')) {
  fit = optim(par = c(1, 1, 1, 1),
              fn = function(x) fit_model(x, model = mdl),
              method = 'Nelder-Mead')
  fitted_params <- rbind(fitted_params, 
                         data.frame(model = mdl, alpha = fit$par[1], beta = fit$par[2], exp1_inverse_temp = fit$par[3], exp2_inverse_temp = fit$par[4]))
}
write.csv(fitted_params,'alternative_model_fitted_params.csv', row.names = FALSE)

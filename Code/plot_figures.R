library(tidyverse)
library(patchwork)
library(LaplacesDemon) # for KLD
source('helper.R')

########## New experiments ##########
dat <- read.csv('./../Data/data.csv', header = T, stringsAsFactors = T)
actor_dat <- dat %>% filter(block == 'actor')
observer_dat <- dat %>% filter(block == 'observer')

simulation <- read.csv('model_prediction.csv', header = T, stringsAsFactors = T)
actor_prediction <- simulation %>% filter(block == 'actor')
observer_prediction <- simulation %>% filter(block == 'observer')

pdf('./../Figures/fig_observer.pdf', onefile = T, width = 10, height = 5)
p1_1 <- observer_plot(observer_dat, observer_prediction, 1) + labs(title = 'Experiment 1 \n Goal = Perceived Competence') + theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')
p1_2 <- observer_plot(observer_dat, observer_prediction, 2) + labs(title = 'Experiment 2 \n Goal = Success', y = NULL) + theme(plot.title = element_text(hjust = 0.5))
p1_1 + p1_2
dev.off()

pdf('./../Figures/fig_actor.pdf', onefile = T, width = 9, height = 4)
p2_1 <- actor_plot(actor_dat, actor_prediction, 1) + labs(title = 'Experiment 1 \n Goal = Perceived Competence') + theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')
p2_2 <- actor_plot(actor_dat, actor_prediction, 2) + labs(title = 'Experiment 2 \n Goal = Success', y = NULL) + theme(plot.title = element_text(hjust = 0.5))
p2_1 + p2_2
dev.off()

pdf('./../Figures/fig_evaluation_change.pdf', onefile = T, width = 12, height = 4)
p3_1 <- evaluation_change_plot(observer_dat, observer_prediction, 1) + labs(title = 'Experiment 1 \n Goal = Perceived Competence') + theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')
p3_2 <- evaluation_change_plot(observer_dat, observer_prediction, 2) + labs(title = 'Experiment 2 \n Goal = Success', y = NULL) + theme(plot.title = element_text(hjust = 0.5))
p3_1 + p3_2
dev.off()

posterior_df <- observer_prediction %>%
  select(-evaluation) %>%
  pivot_wider(names_from = 'observer', values_from = 'posterior') %>%
  rowwise() %>%
  mutate(divergence = KLD(parse_posterior(naive), parse_posterior(sophisticated))$mean.sum.KLD) %>% 
  ungroup()

pdf('./../figures/fig_divergence.pdf', onefile = T, width = 12, height = 4)
p4_1 <- kl_divergence_plot(posterior_df, 1) + labs(title = 'Experiment 1 \n Goal = Perceived Competence') + theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')
p4_2 <- kl_divergence_plot(posterior_df, 2) + labs(title = 'Experiment 2 \n Goal = Success', y = NULL) + theme(plot.title = element_text(hjust = 0.5))
p4_1 + p4_2
dev.off()

########## Past experiments ##########
##### Luginbuhl & Palmer, 1991: naive observer ratings #####
luginbuhl1991_dat <- data.frame(exp_index = c(rep('exp1', 6), rep('exp2', 6)), 
                                outcome = rep(c('A', 'C', 'F'), 4),
                                self_handicap = rep(c(rep('yes', 3), rep('no', 3)), 2),
                                evaluation = c(95.64, 89.39, 85.45,
                                               91.29, 79.43, 70.82,
                                               96.58, 89.08, 82.08,
                                               93.53, 81.83, 74.44)) %>%  # scale [0, 100]
  group_by(outcome, self_handicap) %>% 
  dplyr::summarize(evaluation = mean(evaluation)) %>% 
  ungroup()

c_seq <- seq(0, 100, by = 1)
gamma_seq <- c(.8, 1)
grade_seq <- c('A', 'C', 'F')
d_seq <- c(95, 75, 55) # as indicated in the paper
prior_c_dist <- rep(1/length(c_seq), length(c_seq))

prob_success <- function(c, gamma, d, k = 0.3, b = -3) {
  1 / (1 + exp(-k * (gamma * c - d - b)))
}

naive_df <- NULL
for (i in 1:length(grade_seq)) {
  for (gamma in gamma_seq) {
    # naive observer
    prior_c <- prior_c_dist
    df <- data.frame(c = c_seq, gamma = gamma, prior_c = prior_c) %>% 
      rowwise() %>% 
      mutate(success_likelihood = prob_success(c, gamma, d_seq[i])) %>% 
      ungroup() %>% 
      mutate(failure_likelihood = 1 - success_likelihood) %>% 
      mutate(success_posterior = prior_c * success_likelihood) %>% # apply Bayes' rule
      mutate(success_posterior = success_posterior / sum(success_posterior)) # normalize
    success_expectation <- sum(df$c * df$success_posterior) # E[c|s=0, gamma, d]
    naive_df <- rbind(naive_df, data.frame(outcome = grade_seq[i], self_handicap = ifelse(gamma == 1, 'no', 'yes'), 
                                           evaluation = success_expectation))
  }
}

p1_1 <- luginbuhl1991_plot(luginbuhl1991_dat) + labs(title = 'Data', x = 'Outcome', y = 'Evaluation') + theme(legend.position = 'none')
p1_2 <- luginbuhl1991_plot(naive_df) + labs(title = 'Model', x = 'Outcome', y = NULL)

##### Tice, 1991 #####
tice1991_dat <- data.frame(exp_index = c(rep('exp1', 4), rep('exp2', 4)),
                           competence = rep(c('high', 'low'), 4),
                           meaningful_outcome = rep(c(rep('failure', 2), rep('success', 2)), 2),
                           dv = c(309.7, 213.6, 196.6, 366.7, # number of seconds practiced
                                  3.3, 2.7, 2.5, 3.7)) # number of tape (smaller numbers indicate more distraction from the tape)
tice1991_dat <- tice1991_dat %>% 
  group_by(exp_index) %>% 
  mutate(handicap_factor = dv/(max(dv))) %>% # convert number of seconds practiced and number of tape to percentage
  ungroup() %>% 
  group_by(competence, meaningful_outcome) %>% 
  dplyr::summarize(handicap_factor = mean(handicap_factor)) %>% 
  ungroup()

c_seq <- seq(0, 10, by = 1)
low_competence <- 2
high_competence <- 9
gamma_seq <- c(0.6, 1)
d <- 3
prior_c_dist <- rep(1/length(c_seq), length(c_seq))
w <- 1
inverse_temp <- 15

prob_success <- function(c, gamma, d, k = 2, b = 0) {
  1 / (1 + exp(-k * (gamma * c - d - b)))
}

optimal_gamma_df <- NULL
for (i in 1:2) {
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
      if (i == 1) { # failure is meaning
        failure_expectation <- sum(df$c * df$failure_posterior) # E[c|s=1, gamma]
        # when failure is meaningful, success_expectation = prior mean
        success_expectation <- sum(df$c * df$prior_c)
      } else { # success is meaningful
        # when success is meaningful, failure_expectation = prior mean
        failure_expectation <- sum(df$c * df$prior_c)
        success_expectation <- sum(df$c * df$success_posterior) # E[c|s=0, gamma]  
      }
      
      # actor
      c_hat <- df$success_likelihood[df$c == true_c] * success_expectation + df$failure_likelihood[df$c == true_c] * failure_expectation
      performance <- df$success_likelihood[df$c == true_c]
      value = w * c_hat + (1-w) * performance
      values_df <- rbind(values_df, data.frame(meaningful_outcome = ifelse(i == 1, 'failure', 'success'), competence = true_c, gamma = gamma, value = value))
    }
    values_df$prob <- exp(values_df$value * inverse_temp) / sum(exp(values_df$value * inverse_temp))
    optimal_gamma_df <- rbind(optimal_gamma_df, values_df)
  }
}

actor_df <- optimal_gamma_df %>% 
  mutate(competence = case_when(competence == low_competence ~ 'low',
                                competence == high_competence ~ 'high')) %>% 
  drop_na(competence) %>% 
  group_by(meaningful_outcome, competence) %>% 
  dplyr::summarize(handicap_factor = sum(gamma * prob)) %>% 
  ungroup()

p2_1 <- tice1991_plot(tice1991_dat) + labs(x = 'Condition', y = 'Handicap factor') + theme(legend.position = 'none')
p2_2 <- tice1991_plot(actor_df) + labs(x = 'Condition', y = NULL) 

##### Rhodewalt et al., 1995 #####
rhodewalt1995_dat <- data.frame(excuse = as_factor(rep(c('low_effort', 'anxiety', 'medication'), 4)),
                                self_handicap = c(rep('no', 3), rep('yes', 3), rep('no', 3), rep('yes', 3)),
                                publicity = c(rep('public', 6), rep('private', 6)),
                                evaluation = c(1.7, 1.9, 1.7,
                                               -0.1, 1.7, 1.3,
                                               2.0, 2.1, 2.3,
                                               0.0, 0.4, 0.3)) %>% # scale [-5, 5]
  mutate(evaluation = evaluation + 5) # so that competence >= 0

c_seq <- seq(0, 10, by = 1)
gamma_seq <- c(.5, 1)
d <- 5
prior_c_dist <- rep(1/length(c_seq), length(c_seq))
w <- 1
inverse_temp <- 0.5
excuse <- c('low_effort', 'anxiety', 'medication')

prob_success <- function(c, gamma, d, k = 1, b = 0) { 
  1 / (1 + exp(-k * (gamma * c - d - b)))
}

sophisticated_df <- NULL
for (j in 1:2) { # publicity index
  for (i in 1:3) { # excuse index
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
        failure_expectation <- sum(df$c * df$failure_posterior) # E[c|s=1, gamma]
        success_expectation <- sum(df$c * df$success_posterior) # E[c|s=0, gamma]
        
        # actor
        c_hat <- df$success_likelihood[df$c == true_c] * success_expectation + df$failure_likelihood[df$c == true_c] * failure_expectation
        performance <- df$success_likelihood[df$c == true_c]
        value = w * c_hat + (1-w) * performance
        values_df <- rbind(values_df, data.frame(true_c = true_c, gamma = gamma, value = value))
      }
      values_df$prob <- exp(values_df$value * inverse_temp) / sum(exp(values_df$value * inverse_temp))
      optimal_gamma_df <- rbind(optimal_gamma_df, values_df)
    }
    
    # sophisticated observer
    updated_c <- optimal_gamma_df %>% 
      mutate(prior_c = rep(prior_c, each = length(gamma_seq))) %>% 
      mutate(posterior = prior_c * prob) %>% 
      group_by(gamma) %>% 
      mutate(posterior = posterior/sum(posterior)) %>%  # normalize
      ungroup()
    
    for (gamma in gamma_seq) {
      if (i != 1 & j == 1) { # anxiety or medication in public evaluation condition
        prior_c <- prior_c_dist
      } else {
        prior_c <- updated_c$posterior[updated_c$gamma == gamma]
      }
      sophisticated_df <- rbind(sophisticated_df, data.frame(excuse = excuse[i], 
                                                             self_handicap = ifelse(gamma == 1, 'no', 'yes'),
                                                             publicity = ifelse(j == 1, 'public', 'private'),
                                                             evaluation = sum(prior_c * c_seq)))
    }
  }
}
sophisticated_df <- sophisticated_df %>% mutate(excuse = as_factor(excuse)) 

p3_1 <- rhodewalt1995_plot(rhodewalt1995_dat) + labs(x = 'Excuse', y = 'Evaluation') + theme(legend.position = 'none')
p3_2 <- rhodewalt1995_plot(sophisticated_df) + labs(x = 'Excuse', y = NULL)

pdf('./../Figures/fig_previous_studies.pdf', onefile = T, width = 7, height = 9)
(p1_1 | p1_2) /
  (p2_1 | p2_2) /
  (p3_1 | p3_2)
dev.off()

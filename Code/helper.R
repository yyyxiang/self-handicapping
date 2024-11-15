actor_plot <- function(data, simulation, exp) {
  
  p <- data %>% 
    filter(exp_index == paste0('exp',exp)) %>% 
    mutate(accuracy = as_factor(accuracy)) %>% 
    ggplot(aes(accuracy, probability_10)) +
    geom_hline(yintercept = 50, linetype = 'dashed', color = 'gray') +
    geom_line(data = simulation %>% filter(exp_index == paste0('exp',exp)) %>% mutate(accuracy = as_factor(accuracy)), 
              linetype = 'dashed', 
              group = 1) +
    geom_point(data = simulation %>% filter(exp_index == paste0('exp',exp)) %>% mutate(accuracy = as_factor(accuracy)), 
               aes(shape = 'Model'),
               size = 2,
               fill = 'white') +
    stat_summary(fun.data = 'mean_cl_boot', geom = 'errorbar', width = 0) +
    stat_summary(fun = 'mean', geom = 'point', size = 2, aes(shape = 'Data')) +
    stat_summary(fun = 'mean', geom = 'line', group = 1) +
    scale_shape_manual(values = c('Data' = 16, 'Model' = 24), name = NULL) +
    scale_y_continuous(breaks = c(0, 25, 50, 75, 100), limits = c(0, 100)) +
    labs(x = 'Actor overall accuracy', y = 'Probability of self-handicapping') +
    theme_classic()
  return (p)
}

observer_plot <- function(data, simulation, exp) {
  p <- data %>% 
    filter(exp_index == paste0('exp',exp)) %>% 
    ggplot(aes(x = observer, y = evaluation, color = outcome, group = outcome)) +
    geom_line(data = simulation %>% 
                filter(exp_index == paste0('exp',exp)),
              linetype = 'dashed') +
    geom_point(data = simulation %>% 
                 filter(exp_index == paste0('exp',exp)),
               aes(shape = 'Model'),
               size = 3,
               fill = 'white') +
    stat_summary(fun.data = 'mean_cl_boot', geom = 'errorbar', width = 0) +
    stat_summary(fun = 'mean', geom = 'point', size = 3, aes(shape = 'Data')) +
    stat_summary(fun = 'mean', geom = 'line') +
    geom_text(data = . %>% 
                filter(observer == 'sophisticated') %>% 
                group_by(outcome) %>% 
                dplyr::summarize(evaluation = mean(evaluation)),
              aes(x = 2.3, label = str_to_title(outcome)),
              show.legend = F) +
    scale_color_brewer(palette = 'Set1') +
    scale_shape_manual(values = c('Data' = 16, 'Model' = 24), name = NULL) +
    scale_x_discrete(labels = c('naive' = 'Naive', 'sophisticated' = 'Sophisticated')) +
    guides(color = 'none') +
    ylim(0, 100) +
    labs(x = 'Observer Type', y = 'Evaluation') +
    theme_classic() +
    theme(
      axis.title = element_text(size = 16),    # axis titles font size
      axis.text = element_text(size = 14),     # axis text font size
      legend.text = element_text(size = 14)    # legend text font size
    )
  return (p)
}

evaluation_change_plot <- function(data, simulation, exp) {

  data <- data %>%
    pivot_wider(names_from = observer, values_from = evaluation) %>%
    mutate(diff = sophisticated - naive)
  simulation <- simulation %>%
    pivot_wider(names_from = observer, values_from = evaluation) %>%
    mutate(diff = sophisticated - naive)

  p <- data %>%
    filter(exp_index == paste0('exp',exp)) %>%
    ggplot(aes(outcome, diff, color = outcome)) +
    geom_point(data = simulation %>% filter(exp_index == paste0('exp',exp)), 
               aes(shape = 'Model'),
               size = 2,
               fill = 'white') +
    stat_summary(fun.data = 'mean_cl_boot', geom = 'errorbar', width = 0) +
    stat_summary(fun = 'mean', geom = 'point', size = 3, aes(shape = 'Data')) +
    scale_color_brewer(palette = 'Set1') +
    scale_shape_manual(values = c('Data' = 16, 'Model' = 24), name = NULL) +
    guides(color = 'none') +
    scale_x_discrete(labels = str_to_title(data$outcome)) +
    coord_cartesian(ylim = c(-10, 10)) +
    geom_hline(yintercept = 0, alpha = 0.5, linetype = 'dashed') +
    labs(x = 'Outcome', y = 'Evaluation change') +
    theme_classic() +
    theme(
      axis.title = element_text(size = 16),    # axis titles font size
      axis.text = element_text(size = 14),     # axis text font size
      legend.text = element_text(size = 14)    # legend text font size
    )
  return (p)
}

luginbuhl1991_plot <- function(data) {
  data %>% 
    ggplot(aes(outcome, evaluation, color = self_handicap)) + 
    geom_point(position = position_dodge2(width = 0.5), size = 3) +
    scale_color_discrete(name = 'Self-handicapped', labels = c('Yes', 'No'), limits = c('yes', 'no')) +
    ylim(0, 100) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5))
}

tice1991_plot <- function(data) {
  data %>% 
    ggplot(aes(meaningful_outcome, handicap_factor, color = competence)) +
    geom_point(position = position_dodge2(width = 0.5), size = 3) +
    scale_x_discrete(labels = c('failure' = 'Failure \nmeaningful', 'success' = 'Success \nmeaningful')) +
    scale_color_manual(name = 'Competence', labels = c('Low', 'High'), limits = c('low', 'high'), values = c('#B47EB2', '#2E958C')) +
    ylim(0, 1) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5))
}

rhodewalt1995_plot <- function(data) {
  data %>% 
    ggplot(aes(excuse, evaluation, color = self_handicap)) +
    geom_point(position = position_dodge2(width = 0.5), size = 3) +
    scale_x_discrete(labels = c('low_effort' = 'Low effort', 'anxiety' = 'Anxiety', 'medication' = 'Medication')) +
    scale_color_discrete(name = 'Self-handicapped', labels = c('Yes', 'No'), limits = c('yes', 'no')) +
    ylim(0, 10) + 
    facet_wrap(~publicity, nrow = 2, labeller = as_labeller(c('public' = 'Public evaluation', 'private' = 'Private evaluation'))) +
    theme_classic() +
    theme(plot.title = element_text(hjust = 0.5))
}

library(tidyverse)
library(patchwork)
source('helper.R')

dat <- read.csv('./../../Data/data.csv', header = T, stringsAsFactors = T)
actor_dat <- dat %>% filter(block == 'actor')

for (mdl in c('ambiguity-seeking', 'hide-incompetence')) {
  simulation <- read.csv('alternative_model_prediction.csv', header = T, stringsAsFactors = T) %>% 
    filter(model == mdl)
  
  pdf(paste0('./../../Figures/Supplement/', mdl, '_fig_actor.pdf'), onefile = T, width = 9, height = 4)
  p2_1 <- actor_plot(actor_dat, actor_prediction, 1) + labs(title = 'Experiment 1 \n Goal = Perceived Competence') + theme(plot.title = element_text(hjust = 0.5), legend.position = 'none')
  p2_2 <- actor_plot(actor_dat, actor_prediction, 2) + labs(title = 'Experiment 2 \n Goal = Success', y = NULL) + theme(plot.title = element_text(hjust = 0.5))
  print(p2_1 + p2_2)
  dev.off()
}

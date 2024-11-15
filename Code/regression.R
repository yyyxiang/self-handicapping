library(tidyverse)
library(brms)

dat <- read.csv('./../Data/data.csv', header = T, stringsAsFactors = T)
actor_dat <- dat %>% filter(block == 'actor')
observer_dat <- dat %>% filter(block == 'observer')

# hypothesis a) with the same outcome (success or failure), a naive observer 
# will perceive self-handicapped actors as more competent compared to actors 
# who did not self-handicap.

naive_observer <- observer_dat %>% 
  filter(observer == 'naive') %>% 
  separate(outcome, c('outcome', 'answers'), sep = 4) %>%
  mutate(answers = as_factor(answers),
         outcome = factor(outcome, levels = c('fail', 'pass'), ordered = T))

mdl1_exp1 <- brm(formula = evaluation ~ 1 + outcome + answers + (1 + outcome + answers|subject),
                 data = naive_observer %>% filter(exp_index == 'exp1'),
                 file = 'cache/mdl1_exp1',
                 control = list(adapt_delta = 0.9),
                 seed = 1)
summary(mdl1_exp1)

mdl1_exp2 <- brm(formula = evaluation ~ 1 + outcome + answers + (1 + outcome + answers|subject),
                 data = naive_observer %>% filter(exp_index == 'exp2'),
                 file = 'cache/mdl1_exp2',
                 control = list(adapt_delta = 0.9),
                 seed = 1)
summary(mdl1_exp2)

# hypothesis b) actors are more likely to self-handicap when they are very 
# incompetent or very competent, but not when they are just good enough for the task.

mdl2_exp1 <- brm(formula = probability_10 ~ 1 + I(accuracy^2) + accuracy + (1 + I(accuracy^2) + accuracy|subject),
                 data = actor_dat %>% filter(exp_index == 'exp1'), 
                 file = 'cache/mdl2_exp1',
                 control = list(adapt_delta = 0.9),
                 seed = 1)
summary(mdl2_exp1)

mdl2_exp2 <- brm(formula = probability_10 ~ 1 + I(accuracy^2) + accuracy + (1 + I(accuracy^2) + accuracy|subject),
                 data = actor_dat %>% filter(exp_index == 'exp2'), 
                 file = 'cache/mdl2_exp2',
                 control = list(adapt_delta = 0.9),
                 seed = 1)
summary(mdl2_exp2)

# hypothesis c) sophisticated observers judge actors who self-handicapped and failed 
# as less competent than previously thought as naive observers.

mdl3_exp1 <- brm(formula = evaluation ~ 1 + observer + (1|subject), 
                 data = observer_dat %>% filter(outcome == 'fail10' & exp_index == 'exp1'),
                 file = 'cache/mdl3_exp1',
                 seed = 1)
summary(mdl3_exp1)

mdl3_exp2 <- brm(formula = evaluation ~ 1 + observer + (1|subject), 
                 data = observer_dat %>% filter(outcome == 'fail10' & exp_index == 'exp2'),
                 file = 'cache/mdl3_exp2',
                 seed = 1)
summary(mdl3_exp2)


# supplement: sophisticated observers' evaluations of other actors 

# pass10 actor
supplement_pass10_exp1 <- brm(formula = evaluation ~ 1 + observer + (1|subject), 
                              data = observer_dat %>% filter(outcome == 'pass10' & exp_index == 'exp1'),
                              file = 'cache/supplement_pass10_exp1',
                              seed = 1)
summary(supplement_pass10_exp1)

supplement_pass10_exp2 <- brm(formula = evaluation ~ 1 + observer + (1|subject), 
                              data = observer_dat %>% filter(outcome == 'pass10' & exp_index == 'exp2'),
                              file = 'cache/supplement_pass10_exp2',
                              seed = 1)
summary(supplement_pass10_exp2)

# pass20 actor
supplement_pass20_exp1 <- brm(formula = evaluation ~ 1 + observer + (1|subject), 
                              data = observer_dat %>% filter(outcome == 'pass20' & exp_index == 'exp1'),
                              file = 'cache/supplement_pass20_exp1',
                              seed = 1)
summary(supplement_pass20_exp1)

supplement_pass20_exp2 <- brm(formula = evaluation ~ 1 + observer + (1|subject), 
                              data = observer_dat %>% filter(outcome == 'pass20' & exp_index == 'exp2'),
                              file = 'cache/supplement_pass20_exp2',
                              seed = 1)
summary(supplement_pass20_exp2)

# fail20 actor
supplement_fail20_exp1 <- brm(formula = evaluation ~ 1 + observer + (1|subject), 
                              data = observer_dat %>% filter(outcome == 'fail20' & exp_index == 'exp1'),
                              file = 'cache/supplement_fail20_exp1',
                              seed = 1)
summary(supplement_fail20_exp1)

supplement_fail20_exp2 <- brm(formula = evaluation ~ 1 + observer + (1|subject), 
                              data = observer_dat %>% filter(outcome == 'fail20' & exp_index == 'exp2'),
                              file = 'cache/supplement_fail20_exp2',
                              seed = 1)
summary(supplement_fail20_exp2)

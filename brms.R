## ---- load-packages-load-data---------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(brms)

d_red <- read_csv("processed-data/data-to-model.csv")


## ---- MCMC-model, eval = FALSE--------------------------------------------------------------------------------------------------------------------------
## d_for_m2 <- filter(d_red, !is.na(gender_female) & !is.na(pre_interest)) # # if there are missing vals in the fixed predictors, MCMCglmm gives a warning
## 
## d_for_m2 <- fill(d_for_m2, pre_interest, post_interest)
## 
## d_for_m2 <- d_for_m2 %>%
##   arrange(participant_ID, response_date, within_date_signal_number) %>%
##   group_by(participant_ID) %>%
##   mutate(sig_num = row_number())
## 
## # brms - not run
## 
## library(brms)
## m3 <- brm(mvbind(rm_engagement, post_interest) ~
##             gender_female +
##             pre_interest +
##             (1|p|participant_ID) +
##             (1|q|program_ID),
##           data = d_for_m2, chains = 4, cores = 4, iter = 1000,
##           control = list(adapt_delta = .999,
##                          max_treedepth = 15))
## 


## -------------------------------------------------------------------------------------------------------------------------------------------------------
bf_1 <- bf(interest ~ gender_female + pre_interest + relevance + sig_num + (relevance + sig_num|p|participant_ID))
bf_2 <- bf(post_interest ~ gender_female + pre_interest + sig_num + (1|p|participant_ID))

m4 <- brm(bf_1 + bf_2,
          autocor = cor_ar(~ signal_number | participant_ID),
          data = d_for_m2, 
          chains = 4, cores = 4, iter = 2000)


## -------------------------------------------------------------------------------------------------------------------------------------------------------
# ggplot(d_for_m2, aes(x = sig_num, y = interest, group = as.factor(participant_ID), color = as.factor(participant_ID))) +
#   geom_point() +
#   geom_smooth(method = 'lm', se = FALSE) +
#   theme(legend.position = "none")

bf_2_1 <- bf(interest ~ gender_female + pre_interest + sig_num + (relevance + sig_num|p|participant_ID) + (1|program_ID))
bf_2_2 <- bf(post_interest ~ gender_female + pre_interest + (1|p|participant_ID) + (1|program_ID))

m5 <- brm(bf_2_1 + bf_2_2,
          autocor = cor_ar(~ signal_number | participant_ID),
          data = d_for_m2, 
          chains = 4, cores = 4, iter = 2000)
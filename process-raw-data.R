library(tidyverse)
# install.packages("devtools")
# install_github("jrosen48/jmRtools")
library(jmRtools)

# load("~/desktop/sandbox-01.Rdata")
# 
# l1 <- list(demographics, 
#            post_survey_data_partially_processed,
#            pre_survey_data_processed,
#            video,
#            pqa, 
#            pm,
#            esm)
# 
# l2 <- c("demographics", "post_survey_data_partially_processed", "pre_survey_data_processed",
#         "video", "pqa", "pm", "esm")
# 
# map2(l1, str_c("raw-data/", l2, ".csv"), write_csv)

## ---- processing-attendance-demo-esm-data--------------------------------

attendance <- read_csv("raw-data/attendance.csv")
demographics <- read_csv("raw-data/demographics.csv")
post_survey_data_partially_processed <- read_csv("raw-data/post_survey_data_partially_processed.csv")
pre_survey_data_processed <- read_csv("raw-data/pre_survey_data_processed.csv")
video <- read_csv("raw-data/video.csv")
pqa <- read_csv("raw-data/pqa.csv")
pm <- read_csv("raw-data/pm.csv")
esm <- read_csv("raw-data/esm.csv")

attendance <- rename(attendance, participant_ID = ParticipantID)
attendance <- mutate(attendance, prop_attend = DaysAttended / DaysScheduled,
                     participant_ID = as.integer(participant_ID))
attendance <- select(attendance, participant_ID, prop_attend)

demographics <- filter(demographics, participant_ID!= 7187)
demographics <- left_join(demographics, attendance)

esm$overall_engagement <- jmRtools::composite_mean_maker(esm, hard_working, concentrating, enjoy, interest)

## ---- joining-to-df------------------------------------------------------
df <- left_join(esm, pre_survey_data_processed, by = "participant_ID") # df & post-survey
df <- left_join(df, video, by = c("program_ID", "response_date", "sociedad_class", "signal_number")) # df & video
df <- left_join(df, demographics, by = c("participant_ID", "program_ID")) # df and demographics

## ---- proc-beep-actvariables, echo = F-----------------------------------
df$participant_ID <- as.factor(df$participant_ID)
df$program_ID <- as.factor(df$program_ID)
df$beep_ID <- as.factor(df$beep_ID)
df$beep_ID_new <- as.factor(df$beep_ID_new)

df$youth_activity_rc <- ifelse(df$youth_activity == "Off Task", "Not Focused", df$youth_activity)

df$youth_activity_rc <- ifelse(df$youth_activity_rc == "Student Presentation" | df$youth_activity_rc == "Problem Solving", "Creating Product", df$youth_activity_rc)

df$youth_activity_rc <- ifelse(df$youth_activity_rc == "Showing Video", "Program Staff Led", df$youth_activity_rc)

df$youth_activity_rc <- as.factor(df$youth_activity_rc)

df$youth_activity_rc <- forcats::fct_relevel(df$youth_activity_rc, "Not Focused")

df$relevance <- jmRtools::composite_mean_maker(df, use_outside, future_goals, important)

## ----proc-demographics---------------------------------------------------
df$urm <- ifelse(df$race %in% c("White", "Asian"), 0, 1)
df$race <- as.factor(df$race)
df$race <- fct_lump(df$race, n = 2)
df$race_other <- fct_relevel(df$race, "Other")
df$gender_female <- as.factor(df$gender) # female is comparison_group
df$gender_female <- ifelse(df$gender_female == "F", 1,
                           ifelse(df$gender_female == "M", 0, NA))

## ---- proc-pqa-data------------------------------------------------------
pqa <- mutate(pqa,
              active = active_part_1 + active_part_2,
              ho_thinking = ho_thinking_1 + ho_thinking_2 + ho_thinking_3,
              belonging = belonging_1 + belonging_2,
              agency = agency_1 + agency_2 + agency_3 + agency_4,
              youth_development_overall = active_part_1 + active_part_2 + ho_thinking_1 + ho_thinking_2 + ho_thinking_3 + belonging_1 + belonging_2 + agency_1 + agency_2 + agency_3 + agency_4,
              making_observations = stem_sb_8,
              data_modeling = stem_sb_2 + stem_sb_3 + stem_sb_9,
              interpreting_communicating = stem_sb_6,
              generating_data = stem_sb_4,
              asking_questions = stem_sb_1,
              stem_sb = stem_sb_1 + stem_sb_2 + stem_sb_3 + stem_sb_4 + stem_sb_5 + stem_sb_6 + stem_sb_7 + stem_sb_8 + stem_sb_9)

pqa$sociedad_class <- ifelse(pqa$eighth_math == 1, "8th Math",
                             ifelse(pqa$seventh_math == 1, "7th Math",
                                    ifelse(pqa$sixth_math == 1, "6th Math",
                                           ifelse(pqa$robotics == 1, "Robotics",
                                                  ifelse(pqa$dance == 1, "Dance", NA)))))

pqa <- rename(pqa,
              program_ID = SiteIDNumeric,
              response_date = resp_date,
              signal_number = signal)

pqa$program_ID <- as.character(pqa$program_ID)

df <- left_join(df, pqa, by = c("response_date", "program_ID", "signal_number", "sociedad_class"))

## ---- proc-vars-for-modeling---------------------------------------------
df <- df %>%
    mutate(dm_cog_eng = learning,
           dm_beh_eng = hard_working,
           dm_aff_eng = enjoy,
           dm_challenge = challenge,
           dm_competence = good_at) %>%
    rename(ssb_predict = stem_sb_1,
           ssb_model = stem_sb_2 ,
           ssb_analyze = stem_sb_3,
           ssb_measure = stem_sb_4,
           ssb_tools = stem_sb_5,
           ssb_precision = stem_sb_6,
           ssb_vocabulary = stem_sb_7,
           ssb_classification = stem_sb_8,
           ssb_symbols = stem_sb_9) %>%
    mutate(dm_ask = ssb_predict,
           dm_obs = ssb_classification,
           dm_gen = ifelse(ssb_measure == 1 | ssb_precision == 1, 1, 0),
           dm_mod = ssb_model,
           dm_com = ifelse(ssb_symbols == 1 | ssb_analyze == 1, 1, 0)) %>%
    mutate(ov_cog_eng = (important + future_goals) / 2,
           ov_beh_eng = (hard_working + concentrating) / 2,
           ov_aff_eng = (enjoy + interest) / 2) %>%
    mutate(dm_composite = dm_ask + dm_obs + dm_gen + dm_mod + dm_com,
           dm_composite_di = ifelse(dm_ask == 1 | dm_obs == 1 | dm_gen == 1 | dm_mod == 1 | dm_com == 1, 1, 0))

df$dm_overall_eng <- jmRtools::composite_mean_maker(df, dm_cog_eng, dm_beh_eng, dm_aff_eng)

df <- mutate(df, inquiry_based = ifelse(youth_activity_rc == "Creating Product" | youth_activity_rc == "Lab Activity", 1, 0),
             inquiry_based_three = ifelse(youth_activity_rc == "Creating Product" | youth_activity_rc == "Lab Activity", "inquiry-based",
                                          ifelse(youth_activity_rc == "Not Focused", "not-focused", "other")))

# d_red <- df %>%
#     group_by(participant_ID) %>%
#     mutate(rownum = row_number()) %>%
#     mutate(overall_pre_interest = ifelse(rownum == 1, overall_pre_interest, NA)) %>%
#     ungroup() %>%
#     select(-participant_ID)

dff <- select(df, 
              participant_ID, 
              program_ID, 
              rm_engagement = overall_engagement,
              gender_female,
              urm,
              pre_interest = overall_pre_interest,
              negative_affect)

dff$participant_ID <- as.integer(as.character(dff$participant_ID))

#dff$participant_ID <- as.integer(dff$participant_ID)
ps <- select(post_survey_data_partially_processed, participant_ID, overall_post_interest)

d <- left_join(dff, post_survey_data_partially_processed, by = "participant_ID")
d$program_ID <- as.integer(d$program_ID)
d <- left_join(d, pm) # matches program name
d <- rename(d, post_interest = overall_post_interest)

d_red <- d %>%
    group_by(participant_ID, program_ID) %>%
    mutate(rownum = row_number()) %>%
    mutate(post_interest = ifelse(rownum == 1, post_interest, NA))

write_csv(d_red, "processed-data/data-to-model.csv")

# Preparing/structuring data 
# ind <- distinct(d, participant_ID, post_interest) 
# 
# t <- ind %>% 
#     left_join(pre_survey_data_processed) %>% 
#     select(overall_pre_interest, post_interest)
# 
# rep <- select(d, participant_ID, rm_engagement)
# type <- c(rep("s", nrow(ind)), rep("r", nrow(rep)))
# 
# dd <- data.frame(y = c(ind$post_interest, rep$rm_engagement),
#                  type = as.factor(type),
#                  individual = as.factor(c(as.character(ind$participant_ID), as.character(rep$participant_ID))))
# d_red <- d %>% 
#     group_by(participant_ID, program_ID) %>% 
#     mutate(rownum = row_number()) %>% 
#     mutate(post_interest = ifelse(rownum == 1, post_interest, NA))
# 
# d_red <- filter(d_red, !is.na(pre_interest) & !is.na(gender_female))
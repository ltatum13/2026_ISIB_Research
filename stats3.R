# install and load in the lme4 and lmerTest packages
library("tidyr")
library("dplyr")
library("lme4")
library("lmerTest")

# import fNIRS and symptom data into R script
source("/Users/lailatatum/Library/Mobile Documents/com~apple~CloudDocs/Y2S3/BIOS4110/PROJECTS/r_proj/data_cleaning.R")

## do fNIRS scores in T1 predict the change in symptom scores?

## conduct linear regression on the current mixed model
# fNIRS score as the response variable and distress as the independent variable
lmm_distress <- lmer(score ~ distress * timepoint + (1 | id) + (1 | channel), data = symptom_fNIRS_data)
summary(lmm_distress)

# plot the scores for fNIRS channels and symptom survey
ggplot(symptom_fNIRS_data,
       aes(x = distress, y = score, color = id)) + 
  geom_point(alpha = 0.05) + 
  geom_jitter(width = 0.75, height = 0.75) +
  geom_line() +
  labs(title = "Distress Scores vs Channel Scores Across T1 and T3",
       x = "Distress Scores",
       y = "Channel Scores")

# fNIRS score as the response variable and fear as the independent variable
lmm_fear <- lmer(score ~ fear * timepoint + (1 | id) + (1 | channel), data = symptom_fNIRS_data)
summary(lmm_fear)

# plot the scores for fNIRS channels and symptom survey
ggplot(symptom_fNIRS_data,
       aes(x = fear, y = score, color = id)) + 
  geom_point(alpha = 0.05) +
  geom_jitter(width = 0.5, height = 0.5) +
  geom_smooth(method = "lm") + 
  labs(title = "Fear Scores vs Channel Scores Across T1 and T3",
       x = "Fear Scores",
       y = "Channel Scores")

# fNIRS score as the response variable and mania as the independent variable
lmm_mania <- lmer(score ~ mania + (1 | id) + (1 | channel), data = symptom_fNIRS_data)
summary(lmm_mania)

# plot the scores for fNIRS channels and symptom survey
ggplot(symptom_fNIRS_data,
       aes(x = mania, y = score, color = id)) + 
  geom_point(alpha = 0.05) +
  geom_jitter(width = 0.5, height = 0.5) +
  geom_smooth(method = "lm") + 
  labs(title = "Mania Scores vs Channel Scores Across T1 and T3",
       x = "Mania Scores",
       y = "Channel Scores")

# take the differences between each score per patient between T1 and T3
diff_data <- symptom_fNIRS_data %>%
  group_by(id, channel) %>%
  summarize(
    distress_diff = distress[timepoint == "T3"] - distress[timepoint == "T1"],
    fear_diff = fear[timepoint == "T3"] - fear[timepoint == "T1"],
    mania_diff = mania[timepoint == "T3"] - mania[timepoint == "T1"],
    channel_diff = score[timepoint == "T3"] - score[timepoint == "T1"],
    .groups = "drop"
  )

# conduct linear regression on the new mixed model
# fNIRS score as the response variable and distress as the independent variable
lmm_distress <- lmer(channel_diff ~ distress_diff + (1 | id), data = diff_data)
summary(lmm_distress)

# plot the scores for fNIRS channels and symptom survey
ggplot(diff_data,
       aes(x = distress_diff, y = channel_diff, color = id)) + 
  geom_point(alpha = 0.25) +
  geom_jitter(width = 0.5, height = 0.5) +
  geom_smooth(method = "lm") + 
  labs(title = "Distress Scores vs Channel Scores Across T1 and T3",
       x = "Distress Scores",
       y = "Channel Scores")

# fNIRS score as the response variable and fear as the independent variable
lmm_fear <- lmer(channel_diff ~ fear_diff + (1 | id), data = diff_data)
summary(lmm_fear)

# plot the scores for fNIRS channels and symptom survey
ggplot(diff_data,
       aes(x = fear_diff, y = channel_diff, color = id)) + 
  geom_point() +
  geom_jitter(width = 0.5, height = 0.5) +
  geom_smooth(method = "lm") + 
  labs(title = "Fear Scores vs Channel Scores Across T1 and T3",
       x = "Fear Scores",
       y = "Channel Scores")

# fNIRS score as the response variable and mania as the independent variable
lmm_mania <- lmer(channel_diff ~ mania_diff + (1 | id), data = diff_data)
summary(lmm_mania)

# plot the scores for fNIRS channels and symptom survey
ggplot(diff_data,
       aes(x = mania_diff, y = channel_diff, color = id)) + 
  geom_point() +
  geom_jitter(width = 0.5, height = 0.5) +
  geom_smooth(method = "lm") + 
  labs(title = "Mania Scores vs Channel Scores Across T1 and T3",
       x = "Mania Scores",
       y = "Channel Scores")

## import fNIRS and symptom data into R script
source("/Users/lailatatum/Library/Mobile Documents/com~apple~CloudDocs/Y2S3/BIOS4110/PROJECTS/r_proj/data_cleaning.R")

## create a new data frame with the differences between T3 and T1 for the symptom and fNIRS scores
# find the difference in symptom scores across T1 and T3 per patient
symp_score_diff <- data.frame(id = unique(symptom_data$id),
                              distress_diff = symptom_data$distress[symptom_data$timepoint == "T3"] - symptom_data$distress[symptom_data$timepoint == "T1"],
                              mania_diff = symptom_data$mania[symptom_data$timepoint == "T3"] - symptom_data$mania[symptom_data$timepoint == "T1"],
                              fear_diff = symptom_data$fear[symptom_data$timepoint == "T3"] - symptom_data$fear[symptom_data$timepoint == "T1"])

# find differences in fNIRS scores across T1 and T3 per patient
fNIRS_T1 <- fNIRS_data[fNIRS_data$timepoint == "T1", c("id", "channel", "score")]
fNIRS_T3 <- fNIRS_data[fNIRS_data$timepoint == "T3", c("id", "channel", "score")]

fNIRS_score_diff <- merge(fNIRS_T1, fNIRS_T3, by = c("id", "channel"), suffixes = c("_T1", "_T3"))
fNIRS_score_diff$score_diff <- fNIRS_score_diff$score_T3 - fNIRS_score_diff$score_T1

# take the average difference for each region per patient
for(i in 1:length(fNIRS_score_diff$id)) {
  fNIRS_score_diff$r1_avg_change[fNIRS_score_diff$id == fNIRS_score_diff$id[i]] <- mean(fNIRS_score_diff$score_diff[startsWith(fNIRS_score_diff$channel, "1x") & fNIRS_score_diff$id == fNIRS_score_diff$id[i]])
  fNIRS_score_diff$r2_avg_change[fNIRS_score_diff$id == fNIRS_score_diff$id[i]] <- mean(fNIRS_score_diff$score_diff[startsWith(fNIRS_score_diff$channel, "2x") & fNIRS_score_diff$id == fNIRS_score_diff$id[i]])
  fNIRS_score_diff$r3_avg_change[fNIRS_score_diff$id == fNIRS_score_diff$id[i]] <- mean(fNIRS_score_diff$score_diff[startsWith(fNIRS_score_diff$channel, "3x") & fNIRS_score_diff$id == fNIRS_score_diff$id[i]])
  fNIRS_score_diff$r4_avg_change[fNIRS_score_diff$id == fNIRS_score_diff$id[i]] <- mean(fNIRS_score_diff$score_diff[startsWith(fNIRS_score_diff$channel, "4x") & fNIRS_score_diff$id == fNIRS_score_diff$id[i]])
  fNIRS_score_diff$r5_avg_change[fNIRS_score_diff$id == fNIRS_score_diff$id[i]] <- mean(fNIRS_score_diff$score_diff[startsWith(fNIRS_score_diff$channel, "5x") & fNIRS_score_diff$id == fNIRS_score_diff$id[i]])
  fNIRS_score_diff$r6_avg_change[fNIRS_score_diff$id == fNIRS_score_diff$id[i]] <- mean(fNIRS_score_diff$score_diff[startsWith(fNIRS_score_diff$channel, "6x") & fNIRS_score_diff$id == fNIRS_score_diff$id[i]])
  fNIRS_score_diff$r7_avg_change[fNIRS_score_diff$id == fNIRS_score_diff$id[i]] <- mean(fNIRS_score_diff$score_diff[startsWith(fNIRS_score_diff$channel, "7x") & fNIRS_score_diff$id == fNIRS_score_diff$id[i]])
  fNIRS_score_diff$r8_avg_change[fNIRS_score_diff$id == fNIRS_score_diff$id[i]] <- mean(fNIRS_score_diff$score_diff[startsWith(fNIRS_score_diff$channel, "8x") & fNIRS_score_diff$id == fNIRS_score_diff$id[i]])
  fNIRS_score_diff$r9_avg_change[fNIRS_score_diff$id == fNIRS_score_diff$id[i]] <- mean(fNIRS_score_diff$score_diff[startsWith(fNIRS_score_diff$channel, "9x") & fNIRS_score_diff$id == fNIRS_score_diff$id[i]])
  fNIRS_score_diff$r10_avg_change[fNIRS_score_diff$id == fNIRS_score_diff$id[i]] <- mean(fNIRS_score_diff$score_diff[startsWith(fNIRS_score_diff$channel, "10x") & fNIRS_score_diff$id == fNIRS_score_diff$id[i]])
  fNIRS_score_diff$r11_avg_change[fNIRS_score_diff$id == fNIRS_score_diff$id[i]] <- mean(fNIRS_score_diff$score_diff[startsWith(fNIRS_score_diff$channel, "11x") & fNIRS_score_diff$id == fNIRS_score_diff$id[i]])
  fNIRS_score_diff$r12_avg_change[fNIRS_score_diff$id == fNIRS_score_diff$id[i]] <- mean(fNIRS_score_diff$score_diff[startsWith(fNIRS_score_diff$channel, "12x") & fNIRS_score_diff$id == fNIRS_score_diff$id[i]])
  fNIRS_score_diff$r13_avg_change[fNIRS_score_diff$id == fNIRS_score_diff$id[i]] <- mean(fNIRS_score_diff$score_diff[startsWith(fNIRS_score_diff$channel, "13x") & fNIRS_score_diff$id == fNIRS_score_diff$id[i]])
  fNIRS_score_diff$r14_avg_change[fNIRS_score_diff$id == fNIRS_score_diff$id[i]] <- mean(fNIRS_score_diff$score_diff[startsWith(fNIRS_score_diff$channel, "14x") & fNIRS_score_diff$id == fNIRS_score_diff$id[i]])
  fNIRS_score_diff$r15_avg_change[fNIRS_score_diff$id == fNIRS_score_diff$id[i]] <- mean(fNIRS_score_diff$score_diff[startsWith(fNIRS_score_diff$channel, "15x") & fNIRS_score_diff$id == fNIRS_score_diff$id[i]])
  fNIRS_score_diff$r16_avg_change[fNIRS_score_diff$id == fNIRS_score_diff$id[i]] <- mean(fNIRS_score_diff$score_diff[startsWith(fNIRS_score_diff$channel, "16x") & fNIRS_score_diff$id == fNIRS_score_diff$id[i]])
  fNIRS_score_diff$r17_avg_change[fNIRS_score_diff$id == fNIRS_score_diff$id[i]] <- mean(fNIRS_score_diff$score_diff[startsWith(fNIRS_score_diff$channel, "17x") & fNIRS_score_diff$id == fNIRS_score_diff$id[i]])
  fNIRS_score_diff$r18_avg_change[fNIRS_score_diff$id == fNIRS_score_diff$id[i]] <- mean(fNIRS_score_diff$score_diff[startsWith(fNIRS_score_diff$channel, "18x") & fNIRS_score_diff$id == fNIRS_score_diff$id[i]])
  fNIRS_score_diff$r19_avg_change[fNIRS_score_diff$id == fNIRS_score_diff$id[i]] <- mean(fNIRS_score_diff$score_diff[startsWith(fNIRS_score_diff$channel, "19x") & fNIRS_score_diff$id == fNIRS_score_diff$id[i]])
  fNIRS_score_diff$r20_avg_change[fNIRS_score_diff$id == fNIRS_score_diff$id[i]] <- mean(fNIRS_score_diff$score_diff[startsWith(fNIRS_score_diff$channel, "20x") & fNIRS_score_diff$id == fNIRS_score_diff$id[i]])
}

# reorient the fNIRS to reduce the number of rows to the actual number of participants
fNIRS_score_diff <- fNIRS_score_diff %>%
  select(-c(channel, score_T1, score_T3, score_diff))

# merge both datasets
differences_data <- merge(symp_score_diff, fNIRS_score_diff, by = "id")
differences_data <- differences_data %>% distinct()

## plot the differences in symptoms against the mean differences in each brain region activity
## (fNIRS ~ symptom) and compute the linear regression summary

# distress differences vs average region differences
model <- lm(r5_avg_change ~ distress_diff, data = differences_data)
summary(model) # channel 5 was the only one to show statistical significance

plot(x = differences_data$distress_diff,
     y = differences_data$r5_avg_change,
     type = "p",
     xlab = "Difference in Distress Scores (T3-T1)",
     ylab = "Average Difference in Region 5 Scores (T3-T1)",
     main = "Distress vs Brain Channel Score Differences")
abline(model,
       col = "red")

# fear and anxiety differences vs region differences
model <- lm(r20_avg_change ~ fear_diff, data = differences_data)
summary(model) # none of the channels showed statistical significance

plot(x = differences_data$fear_diff,
     y = differences_data$r5_avg_change,
     type = "p",
     xlab = "Difference in Fear Scores (T3-T1)",
     ylab = "Average Difference in Region 5 Scores (T3-T1)",
     main = "Distress vs Brain Channel Score Differences")
abline(model,
       col = "red")

# mania differences vs region differences
model <- lm(r11_avg_change ~ mania_diff, data = differences_data)
summary(model) # channel 11 was the only one to show statistical significance

plot(x = differences_data$mania_diff,
     y = differences_data$r11_avg_change,
     type = "p",
     xlab = "Difference in Fear Scores (T3-T1)",
     ylab = "Average Difference in Region 11 Scores (T3-T1)",
     main = "Distress vs Brain Channel Score Differences")
abline(model,
       col = "red")

## plot the mean differences in each brain region activity against the differences in symptoms
## (symptom ~ fNIRS) and compute the linear regression summary

# distress differences vs average region differences
model <- lm(distress_diff ~ r5_avg_change, data = differences_data)
summary(model) # channel 5 was the only one to show statistical significance

plot(x = differences_data$r5_avg_change,
     y = differences_data$distress_diff,
     type = "p",
     ylab = "Difference in Distress Scores (T3-T1)",
     xlab = "Average Difference in Region 5 Scores (T3-T1)",
     main = "Distress vs Brain Channel Score Differences")
abline(model,
       col = "red")

# fear and anxiety differences vs region differences
model <- lm(fear_diff ~ r20_avg_change, data = differences_data)
summary(model) # none of the channels showed statistical significance

plot(x = differences_data$r5_avg_change,
     y = differences_data$fear_diff,
     type = "p",
     ylab = "Difference in Fear Scores (T3-T1)",
     xlab = "Average Difference in Region 5 Scores (T3-T1)",
     main = "Distress vs Brain Channel Score Differences")
abline(model,
       col = "red")

# mania differences vs region differences
model <- lm(mania_diff ~ r11_avg_change, data = differences_data)
summary(model) # channel 11 was the only one to show statistical significance

plot(x = differences_data$r11_avg_change,
     y = differences_data$mania_diff,
     type = "p",
     ylab = "Difference in Mania Scores (T3-T1)",
     xlab = "Average Difference in Region 11 Scores (T3-T1)",
     main = "Distress vs Brain Channel Score Differences")
abline(model,
       col = "red")

## do fNIRS scores in T1 predict the change in symptom scores?

## create a data frame with the change in symptoms and the average fNIRS scores by channel for T1
# create a dataset with the T1 fNIRS scores per patient
fNIRS_score_avg <- mutate(fNIRS_T1)

# take the averages for each region per patient
for(i in 1:length(fNIRS_score_avg$id)) {
  fNIRS_score_avg$r1_avg[fNIRS_score_avg$id == fNIRS_T1$id[i]] <- mean(fNIRS_T1$score[startsWith(fNIRS_T1$channel, "1x") & fNIRS_T1$id == fNIRS_T1$id[i]])
  fNIRS_score_avg$r2_avg[fNIRS_score_avg$id == fNIRS_T1$id[i]] <- mean(fNIRS_T1$score[startsWith(fNIRS_T1$channel, "2x") & fNIRS_T1$id == fNIRS_T1$id[i]])
  fNIRS_score_avg$r3_avg[fNIRS_score_avg$id == fNIRS_T1$id[i]] <- mean(fNIRS_T1$score[startsWith(fNIRS_T1$channel, "3x") & fNIRS_T1$id == fNIRS_T1$id[i]])
  fNIRS_score_avg$r4_avg[fNIRS_score_avg$id == fNIRS_T1$id[i]] <- mean(fNIRS_T1$score[startsWith(fNIRS_T1$channel, "4x") & fNIRS_T1$id == fNIRS_T1$id[i]])
  fNIRS_score_avg$r5_avg[fNIRS_score_avg$id == fNIRS_T1$id[i]] <- mean(fNIRS_T1$score[startsWith(fNIRS_T1$channel, "5x") & fNIRS_T1$id == fNIRS_T1$id[i]])
  fNIRS_score_avg$r6_avg[fNIRS_score_avg$id == fNIRS_T1$id[i]] <- mean(fNIRS_T1$score[startsWith(fNIRS_T1$channel, "6x") & fNIRS_T1$id == fNIRS_T1$id[i]])
  fNIRS_score_avg$r7_avg[fNIRS_score_avg$id == fNIRS_T1$id[i]] <- mean(fNIRS_T1$score[startsWith(fNIRS_T1$channel, "7x") & fNIRS_T1$id == fNIRS_T1$id[i]])
  fNIRS_score_avg$r8_avg[fNIRS_score_avg$id == fNIRS_T1$id[i]] <- mean(fNIRS_T1$score[startsWith(fNIRS_T1$channel, "8x") & fNIRS_T1$id == fNIRS_T1$id[i]])
  fNIRS_score_avg$r9_avg[fNIRS_score_avg$id == fNIRS_T1$id[i]] <- mean(fNIRS_T1$score[startsWith(fNIRS_T1$channel, "9x") & fNIRS_T1$id == fNIRS_T1$id[i]])
  fNIRS_score_avg$r10_avg[fNIRS_score_avg$id == fNIRS_T1$id[i]] <- mean(fNIRS_T1$score[startsWith(fNIRS_T1$channel, "10x") & fNIRS_T1$id == fNIRS_T1$id[i]])
  fNIRS_score_avg$r11_avg[fNIRS_score_avg$id == fNIRS_T1$id[i]] <- mean(fNIRS_T1$score[startsWith(fNIRS_T1$channel, "11x") & fNIRS_T1$id == fNIRS_T1$id[i]])
  fNIRS_score_avg$r12_avg[fNIRS_score_avg$id == fNIRS_T1$id[i]] <- mean(fNIRS_T1$score[startsWith(fNIRS_T1$channel, "12x") & fNIRS_T1$id == fNIRS_T1$id[i]])
  fNIRS_score_avg$r13_avg[fNIRS_score_avg$id == fNIRS_T1$id[i]] <- mean(fNIRS_T1$score[startsWith(fNIRS_T1$channel, "13x") & fNIRS_T1$id == fNIRS_T1$id[i]])
  fNIRS_score_avg$r14_avg[fNIRS_score_avg$id == fNIRS_T1$id[i]] <- mean(fNIRS_T1$score[startsWith(fNIRS_T1$channel, "14x") & fNIRS_T1$id == fNIRS_T1$id[i]])
  fNIRS_score_avg$r15_avg[fNIRS_score_avg$id == fNIRS_T1$id[i]] <- mean(fNIRS_T1$score[startsWith(fNIRS_T1$channel, "15x") & fNIRS_T1$id == fNIRS_T1$id[i]])
  fNIRS_score_avg$r16_avg[fNIRS_score_avg$id == fNIRS_T1$id[i]] <- mean(fNIRS_T1$score[startsWith(fNIRS_T1$channel, "16x") & fNIRS_T1$id == fNIRS_T1$id[i]])
  fNIRS_score_avg$r17_avg[fNIRS_score_avg$id == fNIRS_T1$id[i]] <- mean(fNIRS_T1$score[startsWith(fNIRS_T1$channel, "17x") & fNIRS_T1$id == fNIRS_T1$id[i]])
  fNIRS_score_avg$r18_avg[fNIRS_score_avg$id == fNIRS_T1$id[i]] <- mean(fNIRS_T1$score[startsWith(fNIRS_T1$channel, "18x") & fNIRS_T1$id == fNIRS_T1$id[i]])
  fNIRS_score_avg$r19_avg[fNIRS_score_avg$id == fNIRS_T1$id[i]] <- mean(fNIRS_T1$score[startsWith(fNIRS_T1$channel, "19x") & fNIRS_T1$id == fNIRS_T1$id[i]])
  fNIRS_score_avg$r20_avg[fNIRS_score_avg$id == fNIRS_T1$id[i]] <- mean(fNIRS_T1$score[startsWith(fNIRS_T1$channel, "20x") & fNIRS_T1$id == fNIRS_T1$id[i]])
}

# reorient the fNIRS to reduce the number of rows to the actual number of participants
fNIRS_score_avg <- fNIRS_score_avg %>%
  select(-c(channel, score))

# merge both datasets
difference_v_avg <- merge(symp_score_diff, fNIRS_score_avg, by = "id") %>% distinct()

## plot the mean T1 scores in each brain region activity against the differences in symptoms
## (symptom ~ fNIRS) and compute the linear regression summary

# distress differences vs average region activity in T1
model <- lm(distress_diff ~ r15_avg, data = difference_v_avg)
summary(model) # none of the channels showed statistical significance

plot(x = difference_v_avg$r5_avg,
     y = difference_v_avg$distress_diff,
     type = "p",
     xlab = "Average Scores in Region 5 (T1)",
     ylab = "Difference in Distress Scores (T3-T1)",
     main = "Distress Score Differences vs Brain Channel Average")
abline(model,
       col = "red")

# fear differences vs average region activity in T1
model <- lm(fear_diff ~ r20_avg, data = difference_v_avg)
summary(model) # none of the channels showed statistical significance

plot(x = difference_v_avg$r5_avg,
     y = difference_v_avg$fear_diff,
     type = "p",
     xlab = "Average Scores in Region 5 (T1)",
     ylab = "Difference in Fear Scores (T3-T1)",
     main = "Fear Score Differences vs Brain Channel Average")
abline(model,
       col = "red")

# mania differences vs average region activity in T1
model <- lm(mania_diff ~ r20_avg, data = difference_v_avg)
summary(model) # none of the channels showed statistical significance, channel 11 and 18 had p-values closest to 0.05

plot(x = difference_v_avg$r5_avg,
     y = difference_v_avg$mania_diff,
     type = "p",
     xlab = "Average Scores in Region 5 (T1)",
     ylab = "Difference in Mania Scores (T3-T1)",
     main = "Mania Score Differences vs Brain Channel Average")
abline(model,
       col = "red")

## plot the mean T1 scores in each brain region activity against the differences in symptoms
## (fNIRS ~ symptom) and compute the linear regression summary

# distress differences vs average region activity in T1
model <- lm(r5_avg ~ distress_diff, data = difference_v_avg)
summary(model) # none of the channels showed statistical significance

plot(x = difference_v_avg$distress_diff,
     y = difference_v_avg$r1_avg,
     type = "p",
     xlab = "Difference in Distress Scores (T3-T1)",
     ylab = "Average Scores in Region 5 (T1)",
     main = "Distress Score Differences vs Brain Channel Average")
abline(model,
       col = "red")

# fear differences vs average region activity in T1
model <- lm(r20_avg ~ fear_diff, data = difference_v_avg)
summary(model) # none of the channels showed statistical significance

plot(x = difference_v_avg$fear_diff,
     y = difference_v_avg$r1_avg,
     type = "p",
     xlab = "Difference in Fear Scores (T3-T1)",
     ylab = "Average Scores in Region 5 (T1)",
     main = "Fear Score Differences vs Brain Channel Average")
abline(model,
       col = "red")

# mania differences vs average region activity in T1
model <- lm(r20_avg ~ mania_diff, data = difference_v_avg)
summary(model) # none of the channels showed statistical significance

plot(x = difference_v_avg$mania_diff,
     y = difference_v_avg$r1_avg,
     type = "p",
     xlab = "Difference in Mania Scores (T3-T1)",
     ylab = "Average Scores in Region 5 (T1)",
     main = "Mania Score Differences vs Brain Channel Average")
abline(model,
       col = "red")

# create a table of the p-values for distress
dependent_var <- "fear_diff"
independent_vars <- colnames(difference_v_avg[5:24])

# Initialize a list to store the models
models <- list()

# Function to fit and store models for each combination of predictors
fit_models <- function(dep_var, indep_vars, data) {
  models <- list()
  index <- 1
  for (i in 1:length(indep_vars)) {
    combs <- combn(indep_vars, i)
    for (j in 1:ncol(combs)) {
      predictors <- combs[, j]
      formula <- as.formula(paste(dep_var, "~", paste(predictors, collapse = " + ")))
      model <- lm(formula, data = difference_v_avg)
      models[[index]] <- model
      index <- index + 1
    }
  }
  return(models)
}

# Fit models for all combinations of predictors
all_models <- fit_models(dependent_var, independent_vars, data)

# Display the summaries of the models
sapply(all_models, summary)

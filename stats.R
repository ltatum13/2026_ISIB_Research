# import fNIRS and symptom data into R script
source("/Users/lailatatum/Library/Mobile Documents/com~apple~CloudDocs/Y2S3/BIOS4110/PROJECTS/r_proj/data_cleaning.R")

# get the initial summary statistics for the symptom survey data
summary(symptom_fNIRS_data)

# perform paired t test on distress variable during T1 and T3
t.test(symptom_fNIRS_data[symptom_fNIRS_data$timepoint == "T1", "distress"], 
       symptom_fNIRS_data[symptom_fNIRS_data$timepoint == "T3", "distress"],
       paired = TRUE)

# perform paired t test on fear variable during T1 and T3
t.test(symptom_data$fear[symptom_data$timepoint == "T1"], 
       symptom_data$fear[symptom_data$timepoint == "T3"],
       paired = TRUE)

# perform paired t test on mania variable during T1 and T3
t.test(symptom_data$mania[symptom_data$timepoint == "T1"], 
       symptom_data$mania[symptom_data$timepoint == "T3"],
       paired = TRUE)

# create a box plot to compare the variables from T1 and T3
boxplot(symptom_data$distress[symptom_data$timepoint == "T1"], 
        symptom_data$distress[symptom_data$timepoint == "T3"], 
        col = c("orange", "green"), 
        ylab = "Trimesters",
        xlab = "Symptom Scores",
        main = "Change in Distress across T1 and T3",
        names = c("T1", "T3"),
        horizontal = T)

boxplot(symptom_data$fear[symptom_data$timepoint == "T1"], 
        symptom_data$fear[symptom_data$timepoint == "T3"], 
        col = c("orange", "green"), 
        ylab = "Trimesters",
        xlab = "Symptom Scores",
        main = "Change in Fear & Anxiety across T1 and T3",
        names = c("T1", "T3"),
        horizontal = T)

boxplot(symptom_data$mania[symptom_data$timepoint == "T1"], 
        symptom_data$mania[symptom_data$timepoint == "T3"], 
        col = c("orange", "green"), 
        ylab = "Trimesters",
        xlab = "Symptom Scores",
        main = "Change in Mania across T1 and T3",
        names = c("T1", "T3"),
        horizontal = T)
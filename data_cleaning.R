# import tidyverse libraries
library("tidyr")
library("dplyr")

# get data for symptoms and fNIRS from import script
source("/Users/lailatatum/Library/Mobile Documents/com~apple~CloudDocs/Y2S3/BIOS4110/PROJECTS/scripts/import.R")

## clean symptom data
# focus on the following variables: id, imas_fear, imas_mania, imas_distress, 
# imas_fearf1, imas_maniaf1, imas_distressf1
symptom_data <- symptom_data |> select("participant_id", "imas_distress", "imas_distressf1",
                                       "imas_fear", "imas_fearf1", "imas_mania", "imas_maniaf1")

# rename distress, fear, and mania variables
names(symptom_data) <- c("id", "distress_T1", "distress_T3", "fear_T1", "fear_T3", "mania_T1", "mania_T3")

# remove patients with missing values
symptom_data <- symptom_data[is.na(symptom_data$distress_T1) != TRUE, ]
symptom_data <- symptom_data[is.na(symptom_data$distress_T3) != TRUE, ]

# pivot the symptom data to be longer
symptom_data <- symptom_data |>
  pivot_longer(cols = -id,
               names_to = c(".value", "timepoint"),
               names_pattern = "(.+)_(..)$")

## clean fNIRS scan data
# replace F0 and F1 with T1 and T3
fNIRS_data$timepoint[fNIRS_data$timepoint == "F0"] <- "T1"
fNIRS_data$timepoint[fNIRS_data$timepoint == "F1"] <- "T3"

# remove observations from channels like 1x1, 2x2, etc.
fNIRS_data <- fNIRS_data[fNIRS_data$channel != "1x1", ]
fNIRS_data <- fNIRS_data[fNIRS_data$channel != "2x2", ]
fNIRS_data <- fNIRS_data[fNIRS_data$channel != "3x3", ]
fNIRS_data <- fNIRS_data[fNIRS_data$channel != "4x4", ]
fNIRS_data <- fNIRS_data[fNIRS_data$channel != "5x5", ]
fNIRS_data <- fNIRS_data[fNIRS_data$channel != "6x6", ]
fNIRS_data <- fNIRS_data[fNIRS_data$channel != "7x7", ]
fNIRS_data <- fNIRS_data[fNIRS_data$channel != "8x8", ]
fNIRS_data <- fNIRS_data[fNIRS_data$channel != "9x9", ]
fNIRS_data <- fNIRS_data[fNIRS_data$channel != "10x10", ]
fNIRS_data <- fNIRS_data[fNIRS_data$channel != "11x11", ]
fNIRS_data <- fNIRS_data[fNIRS_data$channel != "12x12", ]
fNIRS_data <- fNIRS_data[fNIRS_data$channel != "13x13", ]
fNIRS_data <- fNIRS_data[fNIRS_data$channel != "14x14", ]
fNIRS_data <- fNIRS_data[fNIRS_data$channel != "15x15", ]
fNIRS_data <- fNIRS_data[fNIRS_data$channel != "16x16", ]
fNIRS_data <- fNIRS_data[fNIRS_data$channel != "17x17", ]
fNIRS_data <- fNIRS_data[fNIRS_data$channel != "18x18", ]
fNIRS_data <- fNIRS_data[fNIRS_data$channel != "19x19", ]
fNIRS_data <- fNIRS_data[fNIRS_data$channel != "20x20", ]

# remove participant IDs that are removed from survey data
fNIRS_data <- fNIRS_data[!fNIRS_data$id == "HB006", ]
fNIRS_data <- fNIRS_data[!fNIRS_data$id == "HB039", ]
fNIRS_data <- fNIRS_data[!fNIRS_data$id == "HB059", ]
fNIRS_data <- fNIRS_data[!fNIRS_data$id == "HB020", ]

symptom_data <- symptom_data[!symptom_data$id == "HB005", ]
symptom_data <- symptom_data[!symptom_data$id == "HB034", ]
symptom_data <- symptom_data[!symptom_data$id == "HB051", ]

# remove rows with missing data
fNIRS_data <- subset(fNIRS_data, ave(fNIRS_data$id, fNIRS_data$id, FUN = length) > 380)

symptom_data <- symptom_data[!symptom_data$id == "HB001", ]
symptom_data <- symptom_data[!symptom_data$id == "HB008", ]
symptom_data <- symptom_data[!symptom_data$id == "HB058", ]

# merge datasets
symptom_fNIRS_data <- merge(symptom_data, fNIRS_data, by = c("id", "timepoint"))

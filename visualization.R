## import fNIRS and symptom data into R script
source("/Users/lailatatum/Library/Mobile Documents/com~apple~CloudDocs/Y2S3/BIOS4110/PROJECTS/r_proj/data_cleaning.R")

## create a heatmap to map out functional connectivity like a matrix
# gather fNIRS data from T1
fNIRS_T1_data <- symptom_fNIRS_data %>%
  group_by(timepoint, channel) %>%
  summarize(
    connect_avg = mean(score),
    .groups = "drop"
  ) %>%
  subset(timepoint == "T1", select = c(timepoint, channel, connect_avg)) %>%
  separate(channel, into = c("source", "detector"), sep = "x")

# gather fNIRS data from T3
fNIRS_T3_data <- symptom_fNIRS_data %>%
  group_by(timepoint, channel) %>%
  summarize(
    connect_avg = mean(score),
    .groups = "drop"
  ) %>%
  subset(timepoint == "T3", select = c(timepoint, channel, connect_avg)) %>%
  separate(channel, into = c("source", "detector"), sep = "x")

# use the data frames to create a heatmap per trimester
ggplot(fNIRS_T1_data, aes(x = source, y = detector, fill = connect_avg)) +
  geom_tile() +
  labs(title = "T1 Functional Connectivity")

ggplot(fNIRS_T3_data, aes(x = source, y = detector, fill = connect_avg)) +
  geom_tile() +
  labs(title = "T3 Functional Connectivity")


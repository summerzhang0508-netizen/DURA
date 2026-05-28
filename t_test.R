library(dplyr)
library(tidyr)
library(broom)
library(knitr)

#set up file location
setwd("C:/Users/Administrator/Desktop/plot")
#read file
data <- read.csv("2_3_data.csv")

# get baseline values
baseline_values <- data %>%
  filter(phase == "baseline") %>%
  pull(flip_min_distance_mPCA_mean_bc)

# get training_1 values
training1_values <- data %>%
  filter(phase == "training_1") %>%
  pull(flip_min_distance_mPCA_mean_bc)

# independent two-sample t-test
ttest_result <- t.test(
  baseline_values,
  training1_values,
  paired = FALSE
)

print(ttest_result)



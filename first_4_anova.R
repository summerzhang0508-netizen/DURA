library(car)
library(dplyr)
library(tidyr)

source("function_call.R")

data <- read.csv("2_3_data.csv")


# select data for analysis
analysis_data <- data %>%
  select_phase(c("training_1", "training_2")) %>%
  select_speed(-3)

# create first 4 labels
first_4_data <- create_first_4 (
  analysis_data,
  n_trials = 4
)

#collect factors
learning_data_first_4 <- first_4_data %>%
  mutate(
    ppid_full = factor(ppid_full),
    phase = factor(phase, levels = c("training_1", "training_2")),
    target_x_label = factor(
      target_x_label,
      levels = c("L60", "L30", "R30", "R60")
    ))

# two-way between-subjects ANOVA
transfer_model <- lm(
  flip_min_distance_mPCA_mean_bc ~ phase * target_x_label,
  data = learning_data_first_4
)

transfer_anova <- Anova(
  transfer_model,
  type = 3
)

anova_table <- as.data.frame(transfer_anova)

print(anova_table)
  
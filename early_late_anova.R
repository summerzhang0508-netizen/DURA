library(ez)
library(dplyr)
library(tidyr)

source("function_call.R")

data <- read.csv("2_3_data.csv")


# select data for analysis
analysis_data <- data %>%
  select_phase("training_1") %>%
  select_speed(-3) 

#select_set_order("63_36")

# create Early/Late labels
early_late_data <- create_early_late(
  analysis_data,
  n_trials = 4
)

#collect factors (ppid, period, levels)
learning_data <- early_late_data %>%
  mutate(
    ppid_full = factor(ppid_full),
    period = factor(period, levels = c("Early", "Late")),
    target_x_label = factor(
      target_x_label,
      levels = c("L60", "L30", "R30", "R60")
    ))

learning_anova <- ezANOVA(
  data = learning_data,
  dv = flip_min_distance_mPCA_mean_bc,
  wid = ppid_full,
  within = .(period),
  between = .(target_x_label),
  type = 3,
  detailed = TRUE
)

anova_table <- as.data.frame(learning_anova$ANOVA)

print(anova_table)

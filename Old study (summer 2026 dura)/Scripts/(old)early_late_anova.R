library(ez)
library(dplyr)
library(tidyr)
library(afex)

source("function_call.R")

data <- read.csv("2_3_data.csv")


# select data for analysis
analysis_data <- data %>%
  select_phase("training_1") %>%
  select_speed(-3) 

# manipulate the selected grou
#select_set_order("63_36")

# create Early/Late labels
early_late_data <- create_early_late(
  analysis_data,
  n_trials = 4
)

#collect factors (ppid, period, levels)
learning_data_before_after <- early_late_data %>%
  mutate(
    ppid_full = factor(ppid_full),
    period = factor(period, levels = c("Early", "Late")),
    target_x_label = factor(
      target_x_label,
      levels = c("L60", "L30", "R30", "R60")
      )
    )

learning_summary <- learning_data_before_after %>%
  group_by(phase, target_x_label) %>%
  summarise(
    Mean = mean(flip_min_distance_mPCA_mean_bc),
    SD = sd(flip_min_distance_mPCA_mean_bc),
    N = n(),
    .groups = "drop"
  )

print(learning_summary)

learning_anova <- ezANOVA(
  data = learning_data_before_after,
  dv = flip_min_distance_mPCA_mean_bc,
  wid = ppid_full,
  within = .(period),
  between = .(target_x_label),
  type = 3,
  detailed = TRUE
)

anova_table <- as.data.frame(learning_anova$ANOVA)

print(anova_table)



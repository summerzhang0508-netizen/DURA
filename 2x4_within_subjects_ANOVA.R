library(dplyr)
library(tidyr)
library(afex)
library(apaTables)


source("function_call.R")

data <- read.csv("2_3_data.csv")


# select data for analysis
analysis_data <- data %>%
  select_phase("training_1") %>%
  select_speed(-3) %>%
  select_set_order("63_36")

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

afex_options(
  type = 3,
  correction_aov = "none",
  es_aov = "pes"
)

learning_aov <- aov_car(
  flip_min_distance_mPCA_mean_bc ~ period * target_x_label +
    Error(ppid_full / (period * target_x_label)),
  data = learning_data_before_after,
  factorize = FALSE
)

print(nice(learning_aov, sig_symbols = TRUE), digits = 2)

apa.ezANOVA.table(learning_aov$Anova)

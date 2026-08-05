# load libraries
library(dplyr)
library(tidyr)
library(afex)

data <- read.csv("2_3_data.csv")

source("function_call.R")

# select data for analysis
analysis_data <- data %>%
  select_phase("training_1") %>%
  select_speed(-2) %>%
  select_set_order("36_63")

# create Early/Late labels
learning_data_before_after <- create_early_late(
  analysis_data,
  n_trials = 4
)

# global afex settings
afex_options(
  type = 3,
  correction_aov = "none",
  es_aov = "pes"
  )

# sets up within-subject anova
learning_aov <- aov_car(
  flip_min_distance_mPCA_mean_bc ~ period * target_x_label +
    Error(ppid_full / (period * target_x_label)),
  data = learning_data_before_after,
  factorize = FALSE
)

# display anova table
anova_table <- as.data.frame(nice(learning_aov))
print(anova_table)


##################################################


# follow up t tests and bonferroni
ttest_results <- learning_data_before_after %>%
  group_by(ppid_full, target_x_label, period) %>%
  summarise(
    score = mean(flip_min_distance_mPCA_mean_bc, na.rm = TRUE),
    .groups = "drop"
  ) %>%

#convert to wide format
pivot_wider(
  names_from = period,
  values_from = score
) %>%
  
#analyze each target
group_by(target_x_label) %>%
summarise(
  t = t.test(Early, Late, paired = TRUE)$statistic,
  df = t.test(Early, Late, paired = TRUE)$parameter,
  p = t.test(Early, Late, paired = TRUE)$p.value,
  Mean_Early = mean(Early, na.rm = TRUE),
  Mean_Late = mean(Late, na.rm = TRUE),
  .groups = "drop"
) %>%
mutate(
  p_bonferroni = p.adjust(p, method = "bonferroni"),
  t = round(t, 2),
  df = round(df, 0),
  Mean_Early = round(Mean_Early, 2),
  Mean_Late = round(Mean_Late, 2),
    
  p = ifelse(p < .001, "< .001", sprintf("%.3f", p)),
  p_bonferroni = ifelse(
    p_bonferroni < .001,
    "< .001",
    sprintf("%.3f", p_bonferroni)
  )
)

print(ttest_results)

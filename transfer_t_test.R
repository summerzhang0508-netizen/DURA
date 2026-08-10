library(dplyr)
library(tidyr)

# Load functions
source("function_call.R")

# Read data
data <- read.csv("2_3_data.csv")


# --------------------------------------------------
# Prepare transfer data
# --------------------------------------------------

analysis_data <- data %>%
  select_phase(c("training_1", "training_2")) %>%
  select_speed(c(-2, -3))


# Keep first 4 trials
first_4_data <- create_first_4(
  analysis_data,
  n_trials = 4
)


# Average first 4 trials for each participant
transfer_summary <- first_4_data %>%
  group_by(
    ppid_full,
    speed_label,
    set_order,
    phase,
    target_x_label
  ) %>%
  summarise(
    score = mean(
      flip_min_distance_mPCA_mean_bc,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# ==================================================
# WATER SPEED = 2
# ==================================================

results_speed2 <- bind_rows(
  
  transfer_ttest(
    transfer_summary,
    -2,
    "L60",
    "63_36", "training_1",
    "36_63", "training_2"
  ),
  
  transfer_ttest(
    transfer_summary,
    -2,
    "L30",
    "36_63", "training_1",
    "63_36", "training_2"
  ),
  
  transfer_ttest(
    transfer_summary,
    -2,
    "R30",
    "63_36", "training_1",
    "36_63", "training_2"
  ),
  
  transfer_ttest(
    transfer_summary,
    -2,
    "R60",
    "36_63", "training_1",
    "63_36", "training_2"
  )
) %>%
  
  # Bonferroni correction for the 4 targets
  mutate(
    p_bonferroni = p.adjust(
      p,
      method = "bonferroni"
    ),
    significant_bonferroni = p_bonferroni < .05
  )


# ==================================================
# WATER SPEED = 3
# ==================================================

results_speed3 <- bind_rows(
  
  transfer_ttest(
    transfer_summary,
    -3,
    "L60",
    "63_36", "training_1",
    "36_63", "training_2"
  ),
  
  transfer_ttest(
    transfer_summary,
    -3,
    "L30",
    "36_63", "training_1",
    "63_36", "training_2"
  ),
  
  transfer_ttest(
    transfer_summary,
    -3,
    "R30",
    "63_36", "training_1",
    "36_63", "training_2"
  ),
  
  transfer_ttest(
    transfer_summary,
    -3,
    "R60",
    "36_63", "training_1",
    "63_36", "training_2"
  )
) %>%
  
  # Bonferroni correction for the 4 targets
  mutate(
    p_bonferroni = p.adjust(
      p,
      method = "bonferroni"
    ),
    significant_bonferroni = p_bonferroni < .05
  )


# ==================================================
# Print results
# ==================================================

print("Water Speed = 2")
print(results_speed2)

print("Water Speed = 3")
print(results_speed3)
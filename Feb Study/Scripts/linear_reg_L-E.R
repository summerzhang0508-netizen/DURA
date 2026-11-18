library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(performance)

# ============================================================
# 1. Load data
# ============================================================

data <- read.csv("Feb Study/Data/new_data.csv")
questionnaire <- read.csv("Feb Study/Data/participant_experience.csv")

# ============================================================
# 2. Clean trial data
# ============================================================

data <- data %>%
  mutate(
    phase = trimws(tolower(phase)),
    target_x_label = trimws(target_x_label),
    speed_label = as.numeric(speed_label),
    phase_target_trial_num = as.numeric(phase_target_trial_num)
  )

outcome <- "flip_min_distance_xPCA_mean_bc"

# ============================================================
# 3. Filter Training 1 trials
# ============================================================

training1_data <- data %>%
  filter(
    phase == "training_1",
    speed_label == -3,
    target_x_label %in% c("L60", "R60"),
    !is.na(.data[[outcome]])
  ) %>%
  arrange(ppid_full, phase_target_trial_num)

# ============================================================
# 4. Calculate Early Training 1 (first 2 trials)
# ============================================================

training1_early <- training1_data %>%
  group_by(ppid_full) %>%
  arrange(phase_target_trial_num) %>%
  slice_head(n = 2) %>%
  summarise(
    Early1 = mean(.data[[outcome]], na.rm = TRUE),
    .groups = "drop"
  )

# ============================================================
# 5. Calculate Late Training 1 (last 2 trials)
# ============================================================

training1_late <- training1_data %>%
  group_by(ppid_full) %>%
  arrange(phase_target_trial_num) %>%
  slice_tail(n = 2) %>%
  summarise(
    Late1 = mean(.data[[outcome]], na.rm = TRUE),
    .groups = "drop"
  )

# ============================================================
# 6. Calculate change score
# Late Training 1 - Early Training 1
# ============================================================

training1_change <- training1_early %>%
  inner_join(training1_late, by = "ppid_full") %>%
  mutate(
    transfer_score = Late1 - Early1
  )

# ============================================================
# 7. Add target pairing information
# ============================================================

experiment_info <- data %>%
  select(ppid_full, experiment) %>%
  distinct()

training1_change <- training1_change %>%
  left_join(experiment_info, by = "ppid_full") %>%
  mutate(
    target_pairing = case_when(
      experiment == "novel_TU_3.0M_spatial_generalization" ~ "R60_to_L60",
      experiment == "novel_TD_3.0M_spatial_generalization" ~ "L60_to_R60",
      TRUE ~ NA_character_
    ),
    target_pairing = factor(
      target_pairing,
      levels = c("R60_to_L60", "L60_to_R60")
    )
  )

# ============================================================
# 8. Clean questionnaire responses
# ============================================================

questionnaire <- questionnaire %>%
  mutate(
    gaming_frequency = tolower(trimws(gaming_frequency)),
    vr_experience = tolower(trimws(vr_experience)),
    sports_participation = tolower(trimws(sports_participation))
  )

# ============================================================
# 9. Convert questionnaire responses to numeric scores
# ============================================================

response_mapping <- c(
  "never" = 0,
  "once a year" = 1,
  "once a month" = 2,
  "once a week" = 3,
  "multiple times per week" = 4
)

questionnaire <- questionnaire %>%
  mutate(
    gaming_score = unname(response_mapping[gaming_frequency]),
    vr_score = unname(response_mapping[vr_experience]),
    sports_score = unname(response_mapping[sports_participation])
  )
# ============================================================
# 10. Merge questionnaire and change scores
# ============================================================

regression_data_training1 <- training1_change %>%
  left_join(
    questionnaire %>%
      select(ppid_full, gaming_score, vr_score, sports_score),
    by = "ppid_full"
  ) %>%
  filter(
    !is.na(transfer_score),
    !is.na(gaming_score),
    !is.na(vr_score),
    !is.na(sports_score),
    !is.na(target_pairing)
  )

# ============================================================
# 11. Regression model
# ============================================================

model_training1 <- lm(
  transfer_score ~ gaming_score + vr_score + sports_score + target_pairing,
  data = regression_data_training1
)

summary(model_training1)

broom::tidy(model_training1)

# Optional model diagnostics
# Uncomment if the 'see' package is installed:
# check_model(model_training1)

# ============================================================
# 12. Summary statistics by target pairing
# ============================================================

group_summary_training1 <- regression_data_training1 %>%
  group_by(target_pairing) %>%
  summarise(
    mean_change = mean(transfer_score, na.rm = TRUE),
    sd_change = sd(transfer_score, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )

print(group_summary_training1)

# ============================================================
# 13. Plot colors
# ============================================================

group_colors <- c(
  "R60_to_L60" = "#ff0404",
  "L60_to_R60" = "darkgreen"
)

# ============================================================
# 14. Plot function
# ============================================================

make_training1_plot <- function(x_variable, x_title) {
  
  plot_data <- regression_data_training1 %>%
    mutate(
      target_pairing = factor(
        target_pairing,
        levels = c("R60_to_L60", "L60_to_R60")
      )
    )
  
  ggplot(
    plot_data,
    aes(
      x = .data[[x_variable]],
      y = transfer_score,
      color = target_pairing
    )
  ) +
    geom_point(
      position = position_jitter(width = 0.15, height = 0),
      size = 2,
      alpha = 0.8
    ) +
    # Linear regression with confidence interval
    geom_smooth(
      aes(group = target_pairing),
      method = "lm",
      formula = y ~ x,
      se = TRUE,
      linewidth = 1
    ) +
    # LOESS trend line
    geom_smooth(
      aes(group = target_pairing),
      method = "loess",
      formula = y ~ x,
      se = FALSE,
      linetype = "dashed",
      linewidth = 0.8,
      span = 1.2
    ) +
    scale_color_manual(
      name = "Target Pairing",
      breaks = c("R60_to_L60", "L60_to_R60"),
      values = group_colors,
      labels = c(
        "R60_to_L60" = "R60 → L60 (Group 1)",
        "L60_to_R60" = "L60 → R60 (Group 2)"
      )
    ) +
    labs(
      x = x_title,
      y = "Late Training 1 - Early Training 1"
    ) +
    theme_classic() +
    theme(
      legend.position = "right",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 9),
      legend.key.width = unit(1.2, "cm")
    )
}

# ============================================================
# 15. Create plots with updated axis titles
# ============================================================

plot_gaming_training1 <- make_training1_plot(
  "gaming_score",
  "Gaming Frequency"
)

plot_vr_training1 <- make_training1_plot(
  "vr_score",
  "VR Experience"
)

plot_sports_training1 <- make_training1_plot(
  "sports_score",
  "Sports Participation"
)

print(plot_gaming_training1)
print(plot_vr_training1)
print(plot_sports_training1)

# ============================================================
# 16. Save plots
# ============================================================

ggsave(
  "gaming_training1.png",
  plot_gaming_training1,
  width = 8,
  height = 6,
  dpi = 300
)

ggsave(
  "vr_training1.png",
  plot_vr_training1,
  width = 8,
  height = 6,
  dpi = 300
)

ggsave(
  "sports_training1.png",
  plot_sports_training1,
  width = 8,
  height = 6,
  dpi = 300
)

# ============================================================
# 17. Export regression dataset
# ============================================================

write.csv(
  regression_data_training1,
  "training1_change_scores.csv",
  row.names = FALSE
)

# ============================================================
# 18. Print final results
# ============================================================

cat("\nNumber of participants included:", nrow(regression_data_training1), "\n")

cat("\nParticipants per target pairing:\n")
print(table(regression_data_training1$target_pairing))

cat("\nGroup summary:\n")
print(group_summary_training1)

cat("\nRegression results:\n")
print(summary(model_training1))

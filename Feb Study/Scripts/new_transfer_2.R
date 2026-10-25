
# ============================================================
# NEW DATA: TRANSFER PERFORMANCE
# BASELINE vs. WASHOUT 1 AT (FIRST 2 TARGET-SPECIFIC TRIALS)
#
# Includes:
# 1. Violin plot for first 2 target-specific trials
# 2. All four targets: L60, L30, R30, R60
# 3. Baseline vs. Washout 1 AT
# 4. Paired t-tests for each target
# 5. Holm correction for multiple comparisons
# 6. T-test results printed in R terminal
# 7. Saves graph only; NO CSV output
# ============================================================

# 1. Load packages
library(ggplot2)
library(dplyr)
library(tidyr)

# 2. Read the new dataset
data <- read.csv("Feb Study/Data/new_data.csv")

# Water speed
chosen_speed <- -3

# Dependent variable
error_variable <- "flip_min_distance_xPCA_mean_bc"

# ============================================================
# 3. Select Baseline and Washout 1 AT
# ============================================================

transfer_data <- data %>%
  filter(
    speed_label == chosen_speed,
    phase %in% c("baseline", "washout_1_AT"),
    target_x_label %in% c("L60", "L30", "R30", "R60"),
    !is.na(.data[[error_variable]])
  ) %>%
  group_by(
    ppid_full, set_order, speed_label, phase, target_x_label
  ) %>%
  arrange(trial_num, .by_group = TRUE) %>%
  mutate(target_trial_rank = row_number()) %>%
  ungroup() %>%
  filter(target_trial_rank <= 2)

# ============================================================
# 4. Average the first 2 trials for each participant
# ============================================================

first_2_means <- transfer_data %>%
  group_by(
    ppid_full, set_order, speed_label, phase, target_x_label
  ) %>%
  summarise(
    participant_mean = mean(.data[[error_variable]], na.rm = TRUE),
    trials_used = n(),
    .groups = "drop"
  ) %>%
  filter(trials_used > 0) %>%
  mutate(
    phase = factor(
      phase,
      levels = c("baseline", "washout_1_AT"),
      labels = c("Baseline", "Washout 1 AT")
    ),
    target_x_label = factor(
      target_x_label,
      levels = c("L60", "L30", "R30", "R60")
    )
  )

# ============================================================
# 5. Paired t-tests
# Compare Baseline vs. Washout 1 AT for each target
# Only participants with observations in BOTH phases are used
# ============================================================

paired_data <- first_2_means %>%
  select(
    ppid_full, target_x_label, phase, participant_mean
  ) %>%
  pivot_wider(
    names_from = phase,
    values_from = participant_mean
  ) %>%
  filter(
    !is.na(Baseline),
    !is.na(`Washout 1 AT`)
  )

cat("\n============================================\n")
cat("PAIRED T-TESTS: BASELINE vs. WASHOUT 1 AT\n")
cat("Using first 2 target-specific trials\n")
cat("============================================\n")

test_results <- list()

for (target in c("L60", "L30", "R30", "R60")) {

  target_data <- paired_data %>%
    filter(target_x_label == target)

  baseline_values <- target_data$Baseline
  washout_values <- target_data$`Washout 1 AT`

  cat("\n--------------------------------------------\n")
  cat("Target:", target, "\n")
  cat("Number of paired participants:", nrow(target_data), "\n")

  if (nrow(target_data) < 2) {
    cat("Not enough paired participants to run the t-test.\n")
    next
  }

  test_result <- t.test(
    baseline_values,
    washout_values,
    paired = TRUE
  )

  cat("Baseline mean:",
      round(mean(baseline_values), 2), "cm\n")
  cat("Washout 1 AT mean:",
      round(mean(washout_values), 2), "cm\n")
  cat("Mean difference (Baseline - Washout):",
      round(mean(baseline_values - washout_values), 2), "cm\n\n")

  print(test_result)

  test_results[[target]] <- data.frame(
    Target = target,
    N = nrow(target_data),
    Baseline_Mean = mean(baseline_values),
    Washout1_Mean = mean(washout_values),
    Mean_Difference = mean(baseline_values - washout_values),
    t_value = unname(test_result$statistic),
    df = unname(test_result$parameter),
    p_value = test_result$p.value,
    CI_lower = test_result$conf.int[1],
    CI_upper = test_result$conf.int[2]
  )
}

# Print all results and apply Holm correction
if (length(test_results) > 0) {

  test_summary <- bind_rows(test_results) %>%
    mutate(
      p_adjusted_Holm = p.adjust(p_value, method = "holm"),
      significant_Holm = p_adjusted_Holm < 0.05
    )

  cat("\n========== HOLM-ADJUSTED RESULTS ==========\n")
  print(test_summary, row.names = FALSE)
}

# ============================================================
# 6. Calculate means and 95% confidence intervals for the plot
# ============================================================

summary_data <- first_2_means %>%
  group_by(phase, target_x_label) %>%
  summarise(
    N = sum(!is.na(participant_mean)),
    Mean = mean(participant_mean, na.rm = TRUE),
    SD = sd(participant_mean, na.rm = TRUE),
    SEM = SD / sqrt(N),
    CI_lo = ifelse(
      N > 1,
      Mean - qt(0.975, df = N - 1) * SEM,
      NA_real_
    ),
    CI_hi = ifelse(
      N > 1,
      Mean + qt(0.975, df = N - 1) * SEM,
      NA_real_
    ),
    .groups = "drop"
  )

# ============================================================
# 7. Create x-axis positions
# ============================================================

first_2_means <- first_2_means %>%
  mutate(
    loc_num = as.numeric(target_x_label),
    x_pos = ifelse(
      phase == "Baseline",
      loc_num * 0.7,
      loc_num * 0.7 + 4
    )
  )

summary_data <- summary_data %>%
  mutate(
    loc_num = as.numeric(target_x_label),
    x_pos = ifelse(
      phase == "Baseline",
      loc_num * 0.7,
      loc_num * 0.7 + 4
    )
  )

# ============================================================
# 8. Create transfer plot
# ============================================================

first_2_plot <- ggplot() +

  # Highlight +/- 5 cm region around zero
  annotate(
    "rect",
    xmin = -Inf,
    xmax = Inf,
    ymin = -5,
    ymax = 5,
    fill = "cyan",
    alpha = 0.45
  ) +

  # Zero-error reference line
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "black",
    linewidth = 0.8
  ) +

  # Violin distributions
  geom_violin(
    data = first_2_means,
    aes(
      x = x_pos,
      y = participant_mean,
      group = interaction(phase, target_x_label),
      fill = target_x_label
    ),
      alpha = 0.15,
  width = 0.5,
  color = NA,
  bw = 5,
  adjust = 1,
  trim = FALSE
  ) +

  # Individual participant points
  geom_jitter(
    data = first_2_means,
    aes(
      x = x_pos,
      y = participant_mean,
      color = target_x_label
    ),
    width = 0.1,
    height = 0,
    alpha = 0.25,
    size = 1.5
  ) +

  # Lines connecting each target's mean across phases
  geom_line(
    data = summary_data,
    aes(
      x = x_pos,
      y = Mean,
      color = target_x_label,
      group = target_x_label
    ),
    linewidth = 1
  ) +

  # Mean points
  geom_point(
    data = summary_data,
    aes(
      x = x_pos,
      y = Mean,
      color = target_x_label
    ),
    size = 2
  ) +

  # 95% confidence intervals around means
  geom_errorbar(
    data = summary_data,
    aes(
      x = x_pos,
      ymin = CI_lo,
      ymax = CI_hi,
      color = target_x_label
    ),
    width = 0.05,
    linewidth = 1,
    na.rm = TRUE
  ) +

  # Y-axis
  scale_y_continuous(
    breaks = seq(-75, 50, by = 25),
    expand = expansion(mult = c(0, 0))
  ) +

  coord_cartesian(ylim = c(-85, 55)) +

  # Target colors
  scale_color_manual(
    name = "Target ID",
    values = c(
      "L60" = "#06472a",
      "L30" = "#ff4500",
      "R30" = "#1f00df",
      "R60" = "#ff0404"
    )
  ) +

  scale_fill_manual(
    name = "Target ID",
    values = c(
      "L60" = "#06472a",
      "L30" = "#ff4500",
      "R30" = "#1f00df",
      "R60" = "#ff0404"
    )
  ) +

  # Target labels and phase labels
  scale_x_continuous(
    breaks = c(
      0.7, 1.4, 2.1, 2.8, 1.75,
      4.7, 5.4, 6.1, 6.8, 5.75
    ),
    labels = c(
      "L60", "L30", "R30", "R60", "\n\nBaseline",
      "L60", "L30", "R30", "R60", "\n\nWashout 1 AT"
    ),
    expand = expansion(mult = c(0.02, 0.02))
  ) +

  labs(
    title = "Transfer Performance (First 2 Trials)",
    subtitle = "Baseline vs. Washout 1 AT | Water Speed = 3 m/s",
    x = "Target ID",
    y = "Signed Error (cm)"
  ) +

  theme_minimal() +

  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "lightgrey"),
    panel.grid.minor.y = element_blank(),
    axis.line.y.left = element_line(color = "lightgrey"),
    axis.text.y = element_text(size = 8),
    legend.title = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    ),
    plot.subtitle = element_text(hjust = 0.5)
  )

# Display plot
print(first_2_plot)

# Save graph only; no CSV files
ggsave(
  filename = "new_data_transfer_baseline_vs_washout1.png",
  plot = first_2_plot,
  width = 10,
  height = 5.5,
  dpi = 300
)

cat("\nGraph saved:\n")
cat("- new_data_transfer_baseline_vs_washout1.png\n")

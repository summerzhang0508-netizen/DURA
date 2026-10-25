# ============================================================
# EARLY TRAINING PERFORMANCE: TRAINING 1 VS TRAINING 2
#
# Compares early performance for:
#   - L60 (green target)
#   - R60 (red target)
#
# Early performance = mean of first 2 trials
# Statistical tests = independent-samples Welch t-tests
# Results print in the R terminal
# No statistical-results CSV is saved
# ============================================================

# 1. Load packages
library(ggplot2)
library(dplyr)

# 2. Read data
data <- read.csv("Feb Study/Data/new_data.csv")

chosen_speed <- -3
error_variable <- "flip_min_distance_xPCA_mean_bc"

# ============================================================
# 3. Keep training trials and identify the first 2 trials
# ============================================================

early_data <- data %>%
  filter(
    speed_label == chosen_speed,
    phase %in% c("training_1", "training_2"),
    target_x_label %in% c("L60", "R60"),
    !is.na(.data[[error_variable]])
  ) %>%
  group_by(ppid_full, set_order, phase, target_x_label) %>%
  arrange(trial_num, .by_group = TRUE) %>%
  mutate(trial_order = row_number()) %>%
  filter(trial_order <= 2) %>%
  ungroup()

# Average the first 2 trials for each participant, phase and target
participant_early <- early_data %>%
  group_by(ppid_full, set_order, phase, target_x_label) %>%
  summarise(
    early_error = mean(.data[[error_variable]], na.rm = TRUE),
    trials_used = n(),
    .groups = "drop"
  ) %>%
  filter(trials_used > 0) %>%
  mutate(
    phase = factor(
      phase,
      levels = c("training_1", "training_2"),
      labels = c("Training 1", "Training 2")
    ),
    target_x_label = factor(
      target_x_label,
      levels = c("L60", "R60")
    )
  )


# ============================================================
# 4. INDEPENDENT-SAMPLES WELCH T-TESTS
# Compare early L60 across phases and early R60 across phases
# ============================================================

test_results <- list()

cat("\n============================================\n")
cat("EARLY PERFORMANCE: INDEPENDENT WELCH T-TESTS\n")
cat("============================================\n")

for (target in c("L60", "R60")) {
  
  # Get early observations for this target in each phase
  target_data <- participant_early %>%
    filter(target_x_label == target)
  
  training1 <- target_data %>%
    filter(phase == "Training 1") %>%
    pull(early_error)
  
  training2 <- target_data %>%
    filter(phase == "Training 2") %>%
    pull(early_error)
  
  cat("\n--------------------------------------------\n")
  cat("Target:", target, "\n")
  cat("Training 1 N:", length(training1), "\n")
  cat("Training 2 N:", length(training2), "\n")
  
  if (length(training1) < 2 || length(training2) < 2) {
    cat("Not enough observations to run this test.\n")
    next
  }
  
  # Independent-samples Welch t-test
  result <- t.test(
    training1,
    training2,
    paired = FALSE
  )
  
  cat("Training 1 early mean:",
      round(mean(training1), 2), "cm\n")
  cat("Training 2 early mean:",
      round(mean(training2), 2), "cm\n")
  cat("Difference (Training 1 - Training 2):",
      round(mean(training1) - mean(training2), 2), "cm\n\n")
  
  print(result)
  
  test_results[[target]] <- data.frame(
    Target = target,
    N_Training1 = length(training1),
    N_Training2 = length(training2),
    Mean_Training1 = mean(training1),
    Mean_Training2 = mean(training2),
    Difference_T1_minus_T2 = mean(training1) - mean(training2),
    t_value = unname(result$statistic),
    df = unname(result$parameter),
    p_value = result$p.value
  )
}

# Print the summary and correct for two comparisons
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
# 5. Calculate means and 95% confidence intervals for the plot
# ============================================================

summary_data <- participant_early %>%
  group_by(phase, target_x_label) %>%
  summarise(
    N = n(),
    mean_error = mean(early_error, na.rm = TRUE),
    sd_error = sd(early_error, na.rm = TRUE),
    se_error = sd_error / sqrt(N),
    CI = ifelse(
      N > 1,
      qt(0.975, df = N - 1) * se_error,
      NA_real_
    ),
    .groups = "drop"
  )

# ============================================================
# GRAPH: EARLY PERFORMANCE BY TARGET
# Left panel = Green L60
# Right panel = Red R60
# ============================================================
# Summary means and 95% confidence intervals
summary_data <- participant_early %>%
  group_by(phase, target_x_label) %>%
  summarise(
    N = n(),
    mean_error = mean(early_error, na.rm = TRUE),
    sd_error = sd(early_error, na.rm = TRUE),
    se_error = sd_error / sqrt(N),
    CI = ifelse(
      N > 1,
      qt(0.975, df = N - 1) * se_error,
      NA_real_
    ),
    .groups = "drop"
  )

# Create graph
p_early <- ggplot(
  participant_early,
  aes(x = phase, y = early_error)
) +
  
  # +/- 5 cm band
  annotate(
    "rect",
    xmin = -Inf, xmax = Inf,
    ymin = -5, ymax = 5,
    fill = "cyan",
    alpha = 0.25
  ) +
  
  # Zero-error reference line
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.6
  ) +
  
  # Violin distributions
  geom_violin(
    aes(fill = target_x_label),
    width = 0.6,
    alpha = 0.18,
    color = NA,
    bw = 20,
    trim = FALSE
  ) +
  
  # Individual participant points
  geom_jitter(
    aes(color = target_x_label),
    width = 0.08,
    height = 0,
    alpha = 0.4,
    size = 1.8
  ) +
  
  # Connect means across phases for each target
  geom_line(
    data = summary_data,
    aes(
      x = phase,
      y = mean_error,
      color = target_x_label,
      group = target_x_label
    ),
    inherit.aes = FALSE,
    linewidth = 1
  ) +
  
  # Mean points
  geom_point(
    data = summary_data,
    aes(
      x = phase,
      y = mean_error,
      color = target_x_label
    ),
    inherit.aes = FALSE,
    size = 3
  ) +
  
  # 95% confidence intervals
  geom_errorbar(
    data = summary_data,
    aes(
      x = phase,
      ymin = mean_error - CI,
      ymax = mean_error + CI,
      color = target_x_label
    ),
    inherit.aes = FALSE,
    width = 0.12,
    linewidth = 0.8,
    na.rm = TRUE
  ) +
  
  # Separate target panels
  facet_wrap(
    ~ target_x_label,
    nrow = 1,
  ) +
  
  scale_color_manual(
    name = "Target ID",
    values = c(L60 = "darkgreen", R60 = "red")
  ) +
  
  scale_fill_manual(
    name = "Target ID",
    values = c(L60 = "darkgreen", R60 = "red")
  ) +
  
  labs(
    title = "Early Training Performance by Target",
    subtitle = "Mean of first 2 trials | Water Speed = 3 m/s",
    x = "Training Phase",
    y = "Signed Error (cm)"
  ) +
  
  scale_y_continuous(
    breaks = seq(-100, 200, by = 25)
  ) +
  
  theme_minimal(base_size = 12) +
  
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(color = "lightgrey"),
    panel.grid.minor.y = element_blank(),
    axis.title = element_text(face = "bold"),
    legend.title = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    strip.text = element_text(face = "bold")
  )

# Display graph
print(p_early)

# Save graph
ggsave(
  filename = "new_data_early_training_comparison.png",
  plot = p_early,
  width = 11,
  height = 6,
  dpi = 300
)

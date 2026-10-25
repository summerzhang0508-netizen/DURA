
# ============================================================
# NEW DATA: EARLY vs. LATE TRAINING PERFORMANCE
# Includes:
# 1. Early/late violin plot
# 2. Paired t-tests for each phase and target
# 3. Holm correction for multiple comparisons
# 4. Export of plot and test results
# ============================================================

# 1. Load packages
library(ggplot2)
library(dplyr)
library(tidyr)

# 2. Read the new dataset
data <- read.csv("Feb Study/Data/new_data.csv")

# Select water speed
chosen_speed <- -3

# Dependent variable
error_variable <- "flip_min_distance_xPCA_mean_bc"

# ============================================================
# 3. Keep the two training phases and trained targets
# ============================================================

training_data <- data %>%
  filter(
    speed_label == chosen_speed,
    phase %in% c("training_1", "training_2"),
    target_x_label %in% c("L60", "R60"),
    !is.na(.data[[error_variable]])
  ) %>%
  group_by(ppid_full, phase, target_x_label) %>%
  arrange(trial_num, .by_group = TRUE) %>%
  mutate(trial_order = row_number()) %>%
  ungroup()

# ============================================================
# 4. Identify first 2 and last 2 trials in each phase
# ============================================================

early_late <- training_data %>%
  group_by(ppid_full, phase, target_x_label) %>%
  mutate(
    total_trials = n(),
    period = case_when(
      trial_order <= 2 ~ "Early",
      trial_order > total_trials - 2 ~ "Late",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(period)) %>%
  ungroup()

# Average the first 2 and last 2 trials for each participant
participant_means <- early_late %>%
  group_by(ppid_full, phase, target_x_label, period) %>%
  summarise(
    participant_mean = mean(.data[[error_variable]], na.rm = TRUE),
    trials_used = sum(!is.na(.data[[error_variable]])),
    .groups = "drop"
  ) %>%
  filter(trials_used > 0) %>%
  mutate(
    period = factor(period, levels = c("Early", "Late")),
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
# 5. Prepare paired data for the paired t-tests
# ============================================================

paired_data <- participant_means %>%
  select(ppid_full, phase, target_x_label, period, participant_mean) %>%
  pivot_wider(
    names_from = period,
    values_from = participant_mean
  ) %>%
  filter(!is.na(Early), !is.na(Late))


# ============================================================
# 6. Run paired t-tests: Early vs. Late
# ============================================================

paired_t_tests <- paired_data %>%
  group_by(phase, target_x_label) %>%
  group_modify(~ {
    test_result <- t.test(
      .x$Early,
      .x$Late,
      paired = TRUE
    )
    
    tibble(
      N = nrow(.x),
      Early_Mean = mean(.x$Early),
      Late_Mean = mean(.x$Late),
      Early_minus_Late = mean(.x$Early - .x$Late),
      t_value = unname(test_result$statistic),
      df = unname(test_result$parameter),
      p_value = test_result$p.value,
      CI_lower = test_result$conf.int[1],
      CI_upper = test_result$conf.int[2]
    )
  }) %>%
  ungroup() %>%
  mutate(
    p_adjusted_Holm = p.adjust(p_value, method = "holm"),
    significant_Holm = p_adjusted_Holm < 0.05
  )

# Print the complete results directly in the R terminal
cat("\n========== PAIRED T-TEST RESULTS ==========\n")
print(paired_t_tests, n = Inf, width = Inf)

cat("\n========== HOLM-ADJUSTED SIGNIFICANCE ==========\n")

paired_t_tests %>%
  select(
    phase,
    target_x_label,
    N,
    Early_Mean,
    Late_Mean,
    Early_minus_Late,
    t_value,
    df,
    p_value,
    p_adjusted_Holm,
    significant_Holm
  ) %>%
  print(n = Inf, width = Inf)


# ============================================================
# 7. Calculate means and 95% confidence intervals for the plot
# ============================================================

summary_data <- participant_means %>%
  group_by(phase, target_x_label, period) %>%
  summarise(
    N = sum(!is.na(participant_mean)),
    mean_error = mean(participant_mean, na.rm = TRUE),
    sd_error = sd(participant_mean, na.rm = TRUE),
    se_error = sd_error / sqrt(N),
    CI = ifelse(
      N > 1,
      qt(0.975, df = N - 1) * se_error,
      NA_real_
    ),
    .groups = "drop"
  ) %>%
  mutate(
    plot_x = case_when(
      period == "Early" & target_x_label == "L60" ~ 1,
      period == "Early" & target_x_label == "R60" ~ 2,
      period == "Late"  & target_x_label == "L60" ~ 4,
      period == "Late"  & target_x_label == "R60" ~ 5
    )
  )

# Give individual participant values the same x-axis positions
participant_means <- participant_means %>%
  mutate(
    plot_x = case_when(
      period == "Early" & target_x_label == "L60" ~ 1,
      period == "Early" & target_x_label == "R60" ~ 2,
      period == "Late"  & target_x_label == "L60" ~ 4,
      period == "Late"  & target_x_label == "R60" ~ 5
    )
  )

# ============================================================
# 8. Create the plot
# ============================================================

p <- ggplot() +
  
  # Cyan band showing the +/- 5 cm region around zero
  geom_rect(
    data = data.frame(
      phase = factor(
        c("Training 1", "Training 2"),
        levels = c("Training 1", "Training 2")
      )
    ),
    aes(
      xmin = -Inf,
      xmax = Inf,
      ymin = -5,
      ymax = 5
    ),
    fill = "cyan",
    alpha = 0.25,
    inherit.aes = FALSE
  ) +
  
  # Zero-error reference line
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    linewidth = 0.6
  ) +
  
  # Violin distributions
  geom_violin(
    data = participant_means,
    aes(
      x = plot_x,
      y = participant_mean,
      fill = target_x_label,
      group = interaction(target_x_label, period)
    ),
    alpha = 0.15,
    width = 0.5,
    color = NA,
    bw = 20,
    adjust = 1,
    trim = FALSE
  ) +
  
  # Individual participant observations
  geom_jitter(
    data = participant_means,
    aes(
      x = plot_x,
      y = participant_mean,
      color = target_x_label
    ),
    width = 0.10,
    height = 0,
    alpha = 0.35,
    size = 1.5,
    na.rm = TRUE
  ) +
  
  # Lines connecting Early and Late means for each target
  geom_line(
    data = summary_data,
    aes(
      x = plot_x,
      y = mean_error,
      color = target_x_label,
      group = interaction(phase, target_x_label)
    ),
    linewidth = 1.1
  ) +
  
  # Mean points
  geom_point(
    data = summary_data,
    aes(
      x = plot_x,
      y = mean_error,
      color = target_x_label
    ),
    size = 2.5
  ) +
  
  # 95% confidence intervals around the means
  geom_errorbar(
    data = summary_data,
    aes(
      x = plot_x,
      ymin = mean_error - CI,
      ymax = mean_error + CI,
      color = target_x_label
    ),
    width = 0.12,
    linewidth = 0.8,
    na.rm = TRUE
  ) +
  # Center Early and Late between their target labels
  geom_text(
    data = data.frame(
      phase = factor(
        c("Training 1", "Training 1",
          "Training 2", "Training 2"),
        levels = c("Training 1", "Training 2")
      ),
      x = c(1.5, 4.5, 1.5, 4.5),
      label = c("Early", "Late", "Early", "Late")
    ),
    aes(x = x, y = -Inf, label = label),
    inherit.aes = FALSE,
    vjust = -2,
    size = 3.5
  ) +
  # X-axis labels
  scale_x_continuous(
    breaks = c(1, 2, 4, 5),
    labels = c(
      "L60",
      "R60",
      "L60",
      "R60"
    ),
    limits = c(0.5, 5.5),
    expand = c(0, 0)
  ) +
  
  # Colors
  scale_color_manual(
    name = "Target ID",
    values = c(
      "L60" = "darkgreen",
      "R60" = "red"
    )
  ) +

  
  scale_fill_manual(
    name = "Target ID",
    values = c(
      "L60" = "darkgreen",
      "R60" = "red"
    )
  ) +
  
  # Separate panels for Training 1 and Training 2
  facet_wrap(~ phase, nrow = 1) +
  
  labs(
    title = "Early vs. Late Training Performance",
    subtitle = "Water Speed = 3 m/s",
    x = "Target ID",
    y = "Signed Error (cm)"
  ) +
  
  # Set y-axis ticks and horizontal gridlines every 25 cm
  scale_y_continuous(
    breaks = seq(-100, 200, by = 25)
  ) +
  theme_minimal(base_size = 12) +
  
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "lightgrey"),
    panel.grid.minor.y = element_blank(),
    axis.line.y.left = element_line(color = "lightgrey"),
    axis.text.y = element_text(size = 8),
    panel.spacing.y = unit(0.9, "cm"),
    legend.title = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
    ),
    strip.text.y = element_text(face = "bold"),
    axis.text.x.top = element_text(face = "bold"),
    axis.ticks.x.top = element_blank()
  )


# Display plot
print(p)

# Save high-resolution plot
ggsave(
  "new_data_early_vs_late_training.png",
  plot = p,
  width = 11,
  height = 6,
  dpi = 300
)


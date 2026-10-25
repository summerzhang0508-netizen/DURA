# ============================================================
# NEW DATA: TRANSFER PERFORMANCE (FIRST 2 TRIALS)
# Includes:
# 1. Violin plot for first 2 target-specific trials
# 2. All four targets
# 3. Washout 1 AT and Washout 2 AT
# 4. Independent t-tests between set_order groups
# 5. CSV exports of statistical results
# ============================================================

# 1. Load packages
library(ggplot2)
library(dplyr)
library(tidyr)

# 2. Read the new dataset
data <- read.csv("new_data.csv")

# New dataset has water speed 3 m/s only
chosen_speed <- -3

# Dependent variable
error_variable <- "flip_min_distance_xPCA_mean_bc"

# ============================================================
# 3. Select the all-target washout phases
# ============================================================

transfer_data <- data %>%
  filter(
    speed_label == chosen_speed,
    phase %in% c("washout_1_AT", "washout_2_AT"),
    target_x_label %in% c("L60", "L30", "R30", "R60"),
    !is.na(.data[[error_variable]])
  ) %>%
  group_by(ppid_full, set_order, speed_label, phase, target_x_label) %>%
  arrange(trial_num, .by_group = TRUE) %>%
  mutate(
    target_trial_rank = row_number()
  ) %>%
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
    trials_used = sum(!is.na(.data[[error_variable]])),
    .groups = "drop"
  ) %>%
  filter(trials_used > 0) %>%
  mutate(
    phase = factor(
      phase,
      levels = c("washout_1_AT", "washout_2_AT"),
      labels = c("Washout 1 AT", "Washout 2 AT")
    ),
    target_x_label = factor(
      target_x_label,
      levels = c("L60", "L30", "R30", "R60")
    )
  )

# ============================================================
# 5. Independent t-tests
# Compare set_order groups separately for each target and phase
# ============================================================

independent_t_tests <- first_2_means %>%
  group_by(phase, target_x_label) %>%
  group_modify(~ {
    
    test_data <- .x %>%
      filter(!is.na(participant_mean), !is.na(set_order)) %>%
      mutate(set_order = droplevels(factor(set_order)))
    
    # Check that both groups have at least 2 participants
    group_counts <- table(test_data$set_order)
    
    if (length(group_counts) != 2 || any(group_counts < 2)) {
      return(tibble(
        N_group1 = NA_integer_,
        N_group2 = NA_integer_,
        Group1 = NA_character_,
        Group2 = NA_character_,
        Group1_Mean = NA_real_,
        Group2_Mean = NA_real_,
        Group1_minus_Group2 = NA_real_,
        t_value = NA_real_,
        df = NA_real_,
        p_value = NA_real_,
        CI_lower = NA_real_,
        CI_upper = NA_real_
      ))
    }
    
    test_result <- t.test(
      participant_mean ~ set_order,
      data = test_data,
      var.equal = FALSE
    )
    
    group_names <- levels(test_data$set_order)
    
    group_summary <- test_data %>%
      group_by(set_order) %>%
      summarise(
        N = n(),
        Mean = mean(participant_mean),
        .groups = "drop"
      )
    
    tibble(
      N_group1 = group_summary$N[1],
      N_group2 = group_summary$N[2],
      Group1 = group_names[1],
      Group2 = group_names[2],
      Group1_Mean = group_summary$Mean[1],
      Group2_Mean = group_summary$Mean[2],
      Group1_minus_Group2 =
        group_summary$Mean[1] - group_summary$Mean[2],
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
    significant_Holm = !is.na(p_adjusted_Holm) &
      p_adjusted_Holm < 0.05
  )

# Print independent t-test results
cat("\n========== INDEPENDENT T-TEST RESULTS ==========\n")
print(independent_t_tests)

# Save results
write.csv(
  independent_t_tests,
  "new_data_transfer_first_2_independent_t_tests.csv",
  row.names = FALSE
)

# ============================================================
# 6. Calculate group means and 95% confidence intervals
# for plotting
# ============================================================

summary_data <- first_2_means %>%
  group_by(speed_label, phase, target_x_label) %>%
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
      phase == "Washout 1 AT",
      loc_num * 0.7,
      loc_num * 0.7 + 4
    )
  )

summary_data <- summary_data %>%
  mutate(
    loc_num = as.numeric(target_x_label),
    x_pos = ifelse(
      phase == "Washout 1 AT",
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
    width = 0.4,
    color = NA,
    adjust = 2,
    trim = TRUE
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
  
  # Lines connecting each target's mean across the two phases
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
    linewidth = 1
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
      "L60", "L30", "R30", "R60", "\n\nWashout 1 AT",
      "L60", "L30", "R30", "R60", "\n\nWashout 2 AT"
    ),
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  
  labs(
    title = "Transfer Performance (First 2 Trials)",
    subtitle = "Water Speed = 3 m/s",
    x = "Target ID",
    y = "Min Error to Target (cm)"
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

# Save high-resolution plot
ggsave(
  filename = "new_data_transfer_first_2.png",
  plot = first_2_plot,
  width = 10,
  height = 5.5,
  dpi = 300
)

cat("\nFiles saved:\n")
cat("- new_data_transfer_first_2.png\n")
cat("- new_data_transfer_first_2_independent_t_tests.csv\n")
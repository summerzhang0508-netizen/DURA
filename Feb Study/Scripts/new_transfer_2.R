# ============================================================
# NEW DATA: TRANSFER PERFORMANCE
# BASELINE vs. WASHOUT 1 AT (FIRST 2 TARGET-SPECIFIC TRIALS)
#
# Statistical analysis:
#   - Separate analyses for each training group
#   - Two-way repeated-measures ANOVA:
#       Target x Time Point
#   - Paired post-hoc tests:
#       Baseline vs. Washout 1 AT for each target
#   - Bonferroni correction for the 4 target comparisons
#
# Group mapping:
#   TU = Group 1
#   TD = Group 2
#
# Output:
#   - Statistical results printed in the R console
#   - ANOVA output saved to CSV
#   - Transfer plot saved as PNG
# ============================================================

library(ggplot2)
library(dplyr)
library(tidyr)

# ============================================================
# 1. LOAD DATA AND SET PARAMETERS
# ============================================================

data <- read.csv("Feb Study/Data/new_data.csv")

chosen_speed <- -3
error_variable <- "flip_min_distance_xPCA_mean_bc"

training_groups <- c(
  "novel_TU_3.0M_spatial_generalization" = "Group 1 (TU)",
  "novel_TD_3.0M_spatial_generalization" = "Group 2 (TD)"
)

target_levels <- c("L60", "L30", "R30", "R60")
phase_levels <- c("Baseline", "Washout 1 AT")


# ============================================================
# 2. SELECT FIRST TWO TARGET-SPECIFIC TRIALS
# ============================================================

transfer_data <- data %>%
  filter(
    speed_label == chosen_speed,
    phase %in% c("baseline", "washout_1_AT"),
    target_x_label %in% target_levels,
    experiment %in% names(training_groups),
    !is.na(.data[[error_variable]])
  ) %>%
  group_by(
    ppid_full,
    experiment,
    speed_label,
    phase,
    target_x_label
  ) %>%
  arrange(trial_num, .by_group = TRUE) %>%
  mutate(
    target_trial_rank = row_number()
  ) %>%
  ungroup() %>%
  filter(target_trial_rank <= 2)


# ============================================================
# 3. CALCULATE PARTICIPANT MEANS
# ============================================================

first_2_means <- transfer_data %>%
  group_by(
    ppid_full,
    experiment,
    speed_label,
    phase,
    target_x_label
  ) %>%
  summarise(
    participant_mean = mean(
      .data[[error_variable]],
      na.rm = TRUE
    ),
    trials_used = n(),
    .groups = "drop"
  ) %>%
  filter(trials_used > 0) %>%
  mutate(
    training_group = unname(
      training_groups[as.character(experiment)]
    ),
    phase = factor(
      phase,
      levels = c("baseline", "washout_1_AT"),
      labels = phase_levels
    ),
    target_x_label = factor(
      target_x_label,
      levels = target_levels
    )
  )

cat("\n============================================================\n")
cat("TRANSFER ANALYSIS: BASELINE VS. WASHOUT 1 AT\n")
cat("First 2 target-specific trials | Speed = 3 m/s\n")
cat("Outcome:", error_variable, "\n")
cat("============================================================\n")

cat("\nNumber of participant-level observations by group, phase, and target:\n")

print(
  first_2_means %>%
    count(
      training_group,
      phase,
      target_x_label,
      name = "N_observations"
    ) %>%
    arrange(training_group, phase, target_x_label)
)


# ============================================================
# 4. TWO-WAY REPEATED-MEASURES ANOVA
#    RUN SEPARATELY FOR EACH TRAINING GROUP
# ============================================================

anova_results <- list()
posthoc_results <- list()
complete_data_list <- list()

# ADDED: collect ANOVA output for CSV
anova_csv_results <- list()

for (group_name in unname(training_groups)) {
  
  cat("\n\n")
  cat("############################################################\n")
  cat("TRAINING GROUP:", group_name, "\n")
  cat("############################################################\n")
  
  group_data <- first_2_means %>%
    filter(training_group == group_name) %>%
    select(
      ppid_full,
      target_x_label,
      phase,
      participant_mean
    ) %>%
    group_by(ppid_full, target_x_label, phase) %>%
    summarise(
      participant_mean = mean(participant_mean, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Identify participants with all 4 targets at both phases
  complete_participants <- group_data %>%
    group_by(ppid_full) %>%
    summarise(
      n_cells = n_distinct(
        interaction(target_x_label, phase, drop = TRUE)
      ),
      has_all_values = n() == 8 &&
        all(!is.na(participant_mean)),
      .groups = "drop"
    ) %>%
    filter(n_cells == 8, has_all_values) %>%
    pull(ppid_full)
  
  complete_group_data <- group_data %>%
    filter(ppid_full %in% complete_participants) %>%
    mutate(
      target_x_label = factor(
        target_x_label,
        levels = target_levels
      ),
      phase = factor(
        phase,
        levels = phase_levels
      )
    )
  
  complete_data_list[[group_name]] <- complete_group_data
  
  cat(
    "\nParticipants with complete data:",
    length(complete_participants), "\n"
  )
  
  cat(
    "Participants with any missing target/time-point cell:",
    n_distinct(group_data$ppid_full) - length(complete_participants),
    "\n"
  )
  
  if (length(complete_participants) < 2) {
    cat(
      "\nANOVA and paired post-hoc tests skipped: fewer than 2 participants ",
      "have complete data for all 8 target-by-time-point combinations.\n",
      sep = ""
    )
    next
  }
  
  # ----------------------------------------------------------
  # 4A. TWO-WAY REPEATED-MEASURES ANOVA
  # ----------------------------------------------------------
  
  anova_model <- aov(
    participant_mean ~
      target_x_label * phase +
      Error(ppid_full / (target_x_label * phase)),
    data = complete_group_data
  )
  
  cat("\n--- TWO-WAY REPEATED-MEASURES ANOVA ---\n")
  print(summary(anova_model))
  
  anova_results[[group_name]] <- anova_model
  
  # ADDED: capture printed ANOVA output for CSV
  anova_output <- capture.output(summary(anova_model))
  
  anova_csv_results[[group_name]] <- data.frame(
    training_group = group_name,
    anova_output = anova_output
  )
  
  # ----------------------------------------------------------
  # 4B. PAIRED POST-HOC TESTS BY TARGET
  # ----------------------------------------------------------
  
  cat("\n--- PAIRED POST-HOC TESTS BY TARGET ---\n")
  
  group_wide <- complete_group_data %>%
    select(
      ppid_full,
      target_x_label,
      phase,
      participant_mean
    ) %>%
    pivot_wider(
      names_from = phase,
      values_from = participant_mean
    )
  
  raw_posthoc <- lapply(target_levels, function(target_name) {
    
    target_data <- group_wide %>%
      filter(target_x_label == target_name)
    
    paired_test <- t.test(
      target_data$`Baseline`,
      target_data$`Washout 1 AT`,
      paired = TRUE
    )
    
    data.frame(
      training_group = group_name,
      target = target_name,
      N = nrow(target_data),
      baseline_mean = mean(target_data$`Baseline`),
      washout_mean = mean(target_data$`Washout 1 AT`),
      mean_difference = mean(
        target_data$`Washout 1 AT` -
          target_data$`Baseline`
      ),
      t = unname(paired_test$statistic),
      df = unname(paired_test$parameter),
      p_raw = paired_test$p.value,
      CI_lower = paired_test$conf.int[1],
      CI_upper = paired_test$conf.int[2]
    )
  })
  
  posthoc_table <- bind_rows(raw_posthoc) %>%
    mutate(
      p_bonferroni = p.adjust(p_raw, method = "bonferroni"),
      significant_bonferroni = p_bonferroni < 0.05
    )
  
  posthoc_results[[group_name]] <- posthoc_table
  
  print(
    posthoc_table %>%
      mutate(
        across(
          c(
            baseline_mean,
            washout_mean,
            mean_difference,
            t,
            p_raw,
            p_bonferroni,
            CI_lower,
            CI_upper
          ),
          ~ round(.x, 4)
        )
      )
  )
}


# ============================================================
# 5. COMBINE AND PRINT POST-HOC RESULTS
# ============================================================

cat("\n\n")
cat("============================================================\n")
cat("SUMMARY OF POST-HOC TESTS\n")
cat("Group 1 = TU\n")
cat("Group 2 = TD\n")
cat("============================================================\n\n")

all_posthoc_results <- bind_rows(posthoc_results)

if (nrow(all_posthoc_results) > 0) {
  
  final_results <- all_posthoc_results %>%
    mutate(
      training_group = recode(
        training_group,
        "Group 1 (TU)" = "Group 1",
        "Group 2 (TD)" = "Group 2"
      ),
      across(
        c(
          baseline_mean,
          washout_mean,
          mean_difference,
          t,
          p_raw,
          p_bonferroni,
          CI_lower,
          CI_upper
        ),
        ~ round(.x, 4)
      )
    ) %>%
    select(
      training_group,
      target,
      N,
      baseline_mean,
      washout_mean,
      mean_difference,
      t,
      df,
      p_raw,
      CI_lower,
      CI_upper,
      p_bonferroni,
      significant_bonferroni
    ) %>%
    arrange(
      training_group,
      factor(target, levels = target_levels)
    )
  
  print(final_results, row.names = FALSE)
  
} else {
  cat("No post-hoc results available.\n")
}


# ============================================================
# 6. DESCRIPTIVE STATISTICS FOR PLOTTING
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
  ) %>%
  mutate(
    loc_num = as.numeric(target_x_label),
    
    # Target-specific positions for each phase
    x_pos = ifelse(
      phase == "Baseline",
      loc_num * 0.7,
      loc_num * 0.7 + 4
    ),
    
    # Mean line positions
    line_x = ifelse(
      phase == "Baseline",
      1.75,
      5.75
    )
  )


# ============================================================
# 7. PLOT: GROUP 1 VS GROUP 2 SIDE BY SIDE
# ============================================================

target_colors <- c(
  "L60" = "#06472a",
  "L30" = "#ff4500",
  "R30" = "#1f00df",
  "R60" = "#ff0404"
)

# Add group labels to participant-level plotting data
plot_data <- first_2_means %>%
  mutate(
    training_group = factor(
      training_group,
      levels = c("Group 1 (TU)", "Group 2 (TD)"),
      labels = c("Group 1: TU", "Group 2: TD")
    ),
    loc_num = as.numeric(target_x_label),
    x_pos = ifelse(
      phase == "Baseline",
      loc_num * 0.7,
      loc_num * 0.7 + 4
    )
  )

# Calculate mean and confidence intervals separately
# for each training group, phase, and target
summary_data <- first_2_means %>%
  group_by(training_group, phase, target_x_label) %>%
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
  ) %>%
  mutate(
    training_group = factor(
      training_group,
      levels = c("Group 1 (TU)", "Group 2 (TD)"),
      labels = c("Group 1: TU", "Group 2: TD")
    ),
    loc_num = as.numeric(target_x_label),
    x_pos = ifelse(
      phase == "Baseline",
      loc_num * 0.7,
      loc_num * 0.7 + 4
    ),
    line_x = ifelse(
      phase == "Baseline",
      1.75,
      5.75
    )
  )

first_2_plot <- ggplot() +
  
  # ±5 cm region
  annotate(
    "rect",
    xmin = -Inf,
    xmax = Inf,
    ymin = -5,
    ymax = 5,
    fill = "cyan",
    alpha = 0.12
  ) +
  
  # Zero reference line
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "black",
    linewidth = 0.6
  ) +
  
  # Violin distributions
  geom_violin(
    data = plot_data,
    aes(
      x = x_pos,
      y = participant_mean,
      fill = target_x_label,
      group = interaction(target_x_label, phase)
    ),
    width = 0.3,
    bw = 5,
    adjust = 1,
    trim = FALSE,
    alpha = 0.14,
    color = NA
  ) +
  
  # Individual participant means
  geom_jitter(
    data = plot_data,
    aes(
      x = x_pos,
      y = participant_mean,
      color = target_x_label
    ),
    width = 0.08,
    height = 0,
    alpha = 0.35,
    size = 1.8
  ) +
  
  # Connect each target's means across the two phases
  geom_line(
    data = summary_data,
    aes(
      x = line_x,
      y = Mean,
      color = target_x_label,
      group = target_x_label
    ),
    linewidth = 1.1
  ) +
  
  # Mean points
  geom_point(
    data = summary_data,
    aes(
      x = line_x,
      y = Mean,
      color = target_x_label
    ),
    size = 3
  ) +
  
  # 95% confidence intervals
  geom_errorbar(
    data = summary_data,
    aes(
      x = line_x,
      ymin = CI_lo,
      ymax = CI_hi,
      color = target_x_label
    ),
    width = 0.12,
    linewidth = 0.8
  ) +
  
  scale_color_manual(values = target_colors) +
  scale_fill_manual(values = target_colors) +
  
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
  
  scale_y_continuous(
    breaks = seq(-30, 30, 10),
    limits = c(-30, 30)
  ) +
  
  coord_cartesian(
    ylim = c(-30, 30),
    expand = FALSE
  ) +
  
  labs(
    title = "Transfer Performance (First 2 Trials)",
    subtitle = "Baseline vs. Washout 1 AT | Water Speed = 3 m/s",
    x = "Target ID",
    y = "Signed Error (cm)",
    color = "Target ID",
    fill = "Target ID"
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
    plot.subtitle = element_text(hjust = 0.5),
    strip.text = element_text(face = "bold", size = 11)
  ) +
  
  # Two panels, one for each training group
  facet_wrap(
    ~ training_group,
    nrow = 1
  )

print(first_2_plot)


# ============================================================
# 8. SAVE PLOT
# ============================================================

ggsave(
  filename = "new_data_transfer_group1_group2.png",
  plot = first_2_plot,
  width = 14,
  height = 6.5,
  dpi = 300
)
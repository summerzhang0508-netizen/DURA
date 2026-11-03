library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)

# ============================================================
# NEW DATA: SEX DIFFERENCES IN TRANSFER
# BASELINE VS. WASHOUT 1 AT
# FIRST 2 TARGET-SPECIFIC TRIALS
# ============================================================

data <- read.csv("Feb Study/Data/new_data.csv")


# ------------------------------------------------------------
# 1. Select BASELINE and WASHOUT 1 AT
#    Include both phases and available water speeds
# ------------------------------------------------------------

analysis_data <- data %>%
  filter(
    phase %in% c("baseline", "washout_1_AT"),
    speed_label %in% c(-3, -2),
    sex %in% c("F", "M"),
    !is.na(flip_min_distance_xPCA_mean_bc)
  )


# ------------------------------------------------------------
# 2. Keep FIRST 2 target-specific trials
#    for each participant, phase, and target
# ------------------------------------------------------------

early_transfer_data <- analysis_data %>%
  group_by(
    ppid_full,
    sex,
    speed_label,
    phase,
    target_x_label
  ) %>%
  arrange(
    phase_target_trial_num,
    .by_group = TRUE
  ) %>%
  filter(
    phase_target_trial_num <= 2
  ) %>%
  ungroup()


# ------------------------------------------------------------
# 3. Calculate one mean per participant and phase
#    Averaged across the four targets
# ------------------------------------------------------------

transfer_by_sex <- early_transfer_data %>%
  group_by(
    ppid_full,
    sex,
    speed_label,
    phase
  ) %>%
  summarise(
    mean_error = mean(
      flip_min_distance_xPCA_mean_bc,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  
  mutate(
    phase = factor(
      phase,
      levels = c("baseline", "washout_1_AT"),
      labels = c("Baseline", "Washout_1_AT")
    ),
    
    sex = factor(
      sex,
      levels = c("F", "M")
    )
  ) %>%
  
  pivot_wider(
    names_from = phase,
    values_from = mean_error
  ) %>%
  
  filter(
    !is.na(Baseline),
    !is.na(Washout_1_AT)
  ) %>%
  
  mutate(
    # Same calculation as your old analysis
    Transfer = Baseline - Washout_1_AT
  )


# ------------------------------------------------------------
# 4. Check participant numbers
# ------------------------------------------------------------

cat("\n============================================\n")
cat("PARTICIPANT NUMBERS BY SEX AND SPEED\n")
cat("============================================\n")

transfer_by_sex %>%
  group_by(speed_label, sex) %>%
  summarise(
    participants = n_distinct(ppid_full),
    .groups = "drop"
  ) %>%
  print(n = Inf)


# ------------------------------------------------------------
# 5. Descriptive statistics
# ------------------------------------------------------------

cat("\n============================================\n")
cat("DESCRIPTIVE STATISTICS\n")
cat("============================================\n")

transfer_by_sex %>%
  group_by(speed_label, sex) %>%
  summarise(
    N = n(),
    Baseline_M = mean(Baseline, na.rm = TRUE),
    Baseline_SD = sd(Baseline, na.rm = TRUE),
    Washout_M = mean(Washout_1_AT, na.rm = TRUE),
    Washout_SD = sd(Washout_1_AT, na.rm = TRUE),
    Transfer_M = mean(Transfer, na.rm = TRUE),
    Transfer_SD = sd(Transfer, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    across(where(is.numeric), ~ round(.x, 2))
  ) %>%
  print(n = Inf)


# ------------------------------------------------------------
# 6. Welch independent-samples t-test
#    Female vs Male separately for each water speed
# ------------------------------------------------------------

gender_transfer_tests <- transfer_by_sex %>%
  group_by(speed_label) %>%
  group_modify(
    ~ tidy(
      t.test(
        Transfer ~ sex,
        data = .x
      )
    )
  ) %>%
  ungroup()


# ------------------------------------------------------------
# 7. Format t-test results
# ------------------------------------------------------------

gender_transfer_table <- gender_transfer_tests %>%
  mutate(
    Mean_F = round(estimate1, 2),
    Mean_M = round(estimate2, 2),
    Mean_Difference = round(estimate, 2),
    t = round(statistic, 2),
    df = round(parameter, 2),
    
    CI_Lower = round(conf.low, 2),
    CI_Upper = round(conf.high, 2),
    
    p = ifelse(
      p.value < .001,
      "< .001",
      sprintf("%.3f", p.value)
    ),
    
    significant =
      p.value < .05
  ) %>%
  
  select(
    speed_label,
    Mean_F,
    Mean_M,
    Mean_Difference,
    t,
    df,
    p,
    significant,
    CI_Lower,
    CI_Upper
  )

cat("\n============================================\n")
cat("GENDER DIFFERENCES IN TRANSFER\n")
cat("Baseline vs. Washout 1 AT\n")
cat("First 2 target-specific trials\n")
cat("============================================\n")

print(gender_transfer_table, row.names = FALSE)


# ------------------------------------------------------------
# 8. Plot
# ------------------------------------------------------------

sex_transfer_plot <- ggplot(
  transfer_by_sex,
  aes(
    x = sex,
    y = Transfer,
    fill = sex
  )
) +
  
  # Cyan band around zero
  annotate(
    "rect",
    xmin = -Inf,
    xmax = Inf,
    ymin = -5,
    ymax = 5,
    fill = "cyan",
    alpha = 0.15
  ) +
  
  # Zero = no difference between Baseline and Washout 1 AT
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "black",
    linewidth = 0.7
  ) +
  
  geom_violin(
    alpha = 0.15,
    width = 0.3,
    color = NA,
    bw = 15,
    adjust = 1,
    trim = FALSE
  ) +
  
  geom_jitter(
    aes(color = sex),
    width = 0.1,
    alpha = 0.6
  ) +
  
  facet_wrap(
    ~ speed_label,
    labeller = labeller(
      speed_label = c(
        "-3" = "Water Speed = 3 m/s",
        "-2" = "Water Speed = 2 m/s"
      )
    )
  ) +
  
  scale_y_continuous(
    breaks = seq(-75, 75, by = 25),
    expand = expansion(mult = c(0, 0))
  ) +
  
  coord_cartesian(
    ylim = c(-75, 75)
  ) +
  
  labs(
    x = "Sex",
    y = "Baseline-to-Washout Error Difference (cm)",
    title = "Transfer by Sex",
    subtitle = "Baseline vs. Washout 1 AT | First 2 Target-Specific Trials",
    color = "Sex",
    fill = "Sex"
  ) +
  
  scale_color_manual(
    values = c(
      "F" = "red",
      "M" = "blue"
    ),
    labels = c(
      "F" = "Female",
      "M" = "Male"
    )
  ) +
  
  scale_fill_manual(
    values = c(
      "F" = "red",
      "M" = "blue"
    ),
    labels = c(
      "F" = "Female",
      "M" = "Male"
    )
  ) +
  
  scale_x_discrete(
    labels = c(
      "F" = "Female",
      "M" = "Male"
    )
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

print(sex_transfer_plot)


# ------------------------------------------------------------
# 9. Save
# ------------------------------------------------------------

ggsave(
  filename = "sex_diff_transfer_new_data_baseline_washout1.png",
  plot = sex_transfer_plot,
  width = 9,
  height = 5,
  dpi = 300
)

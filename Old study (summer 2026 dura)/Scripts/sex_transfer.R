library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)

data <- read.csv("demo_data.csv")

source("function_call.R")

# ------------------------------------------------------------
# 1. Select both training phases and both speeds
# ------------------------------------------------------------

analysis_data <- data %>%
  select_phase(c("training_1", "training_2")) %>%
  select_speed(c(-3, -2))


# ------------------------------------------------------------
# 2. Keep first 4 trials of each phase
# ------------------------------------------------------------

early_transfer_data <- create_first_4(
  analysis_data,
  n_trials = 4
)


# ------------------------------------------------------------
# 3. One early mean per participant and phase
# ------------------------------------------------------------

transfer_by_sex <- early_transfer_data %>%
  group_by(
    ppid_full,
    sex_enter,
    speed_label,
    phase
  ) %>%
  
  summarise(
    mean_error = mean(
      flip_min_distance_mPCA_mean_bc,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  
  mutate(
    phase = factor(
      phase,
      levels = c("training_1", "training_2"),
      labels = c("Early_1", "Early_2")
    ),
    
    sex_enter = factor(
      sex_enter,
      levels = c("F", "M")
    )
  ) %>%
  
  pivot_wider(
    names_from = phase,
    values_from = mean_error
  ) %>%
  
  filter(
    !is.na(Early_1),
    !is.na(Early_2)
  ) %>%
  
  mutate(
    # Flip the sign so positive = better transfer
    Transfer = Early_1 - Early_2
  )


# ------------------------------------------------------------
# 4. Welch t-test at each water speed
# ------------------------------------------------------------

gender_transfer_tests <- transfer_by_sex %>%
  group_by(speed_label) %>%
  
  group_modify(
    ~ tidy(
      t.test(
        Transfer ~ sex_enter,
        data = .x
      )
    )
  ) %>%
  
  ungroup()


# ------------------------------------------------------------
# 5. Bonferroni correction across 2 tests
# ------------------------------------------------------------

gender_transfer_table <- gender_transfer_tests %>%
  mutate(
    p_bonferroni_numeric = p.adjust(
      p.value,
      method = "bonferroni"
    ),
    
    significant_bonferroni =
      p_bonferroni_numeric < .05,
    
    Mean_F = round(estimate1, 2),
    Mean_M = round(estimate2, 2),
    Mean_Difference = round(estimate, 2),
    t = round(statistic, 2),
    df = round(parameter, 2),
    CI_Lower = round(conf.low, 2),
    CI_Upper = round(conf.high, 2),
    
    p_raw = ifelse(
      p.value < .001,
      "< .001",
      sprintf("%.3f", p.value)
    ),
    
    p_bonferroni = ifelse(
      p_bonferroni_numeric < .001,
      "< .001",
      sprintf("%.3f", p_bonferroni_numeric)
    )
  ) %>%
  
  select(
    speed_label,
    Mean_F,
    Mean_M,
    Mean_Difference,
    t,
    df,
    p_raw,
    p_bonferroni,
    significant_bonferroni,
    CI_Lower,
    CI_Upper
  )

print(gender_transfer_table)


# ------------------------------------------------------------
# 6. Plot
# ------------------------------------------------------------

sex_transfer_plot <- ggplot(
  transfer_by_sex,
  aes(
    x = sex_enter,
    y = Transfer,
    fill = sex_enter
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
  
  # Zero = no difference between Early T1 and Early T2
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "black",
    linewidth = 0.7
  ) +
  
  geom_violin(
    alpha = 0.15,
    width = 0.5,
    color = NA,
    bw = 15,
    adjust = 1,
    trim = FALSE
  ) +
  
  geom_jitter(
    aes(color = sex_enter),
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
    breaks = seq(-100, 100, by = 25),
    expand = expansion(mult = c(0, 0))
  ) +
  
  coord_cartesian(
    ylim = c(-100, 100)
  ) +
  
  labs(
    x = "Sex",
    y = "Reduction in Initial Error (cm)",
    title = "Transfer by Sex",
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
    )
  )

print(sex_transfer_plot)

# ------------------------------------------------------------
# 7. Save
# ------------------------------------------------------------

ggsave(
  filename = "sex_diff_transfer.png",
  plot = sex_transfer_plot,
  width = 9,
  height = 5,
  dpi = 300
)


library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(purrr)

data <- read.csv("demo_data.csv")

source("function_call.R")

# ------------------------------------------------------------
# 1. Select TRAINING 1 ONLY and both water speeds
# ------------------------------------------------------------

analysis_data <- data %>%
  select_phase("training_1") %>%
  select_speed(c(-2, -3))


# ------------------------------------------------------------
# 2. Create Early and Late periods
# ------------------------------------------------------------

learning_data_early_late <- create_early_late(
  analysis_data,
  n_trials = 4
)


# ------------------------------------------------------------
# 3. Calculate amount of learning
#
# Learning = Early - Late
# Positive values = greater reduction in error
# ------------------------------------------------------------

learning_by_sex <- learning_data_early_late %>%
  group_by(
    ppid_full,
    sex_enter,
    speed_label,
    period
  ) %>%
  summarise(
    mean_error = mean(
      flip_min_distance_mPCA_mean_bc,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  
  pivot_wider(
    names_from = period,
    values_from = mean_error
  ) %>%
  
  filter(
    !is.na(Early),
    !is.na(Late)
  ) %>%
  
  mutate(
    Learning = Early - Late
  )


# ------------------------------------------------------------
# 4. Welch independent-samples t-test
#    Female vs Male separately for each water speed
# ------------------------------------------------------------

gender_ttests <- learning_by_sex %>%
  group_by(speed_label) %>%
  group_modify(
    ~ tidy(
      t.test(
        Learning ~ sex_enter,
        data = .x
      )
    )
  ) %>%
  ungroup()


# ------------------------------------------------------------
# 5. Bonferroni correction across the TWO water-speed tests
# ------------------------------------------------------------

gender_ttest_table <- gender_ttests %>%
  mutate(
    p_bonferroni_numeric = p.adjust(
      p.value,
      method = "bonferroni"
    ),
    
    significant_bonferroni =
      p_bonferroni_numeric < .05,
    
    Mean_Group1 = round(estimate1, 2),
    Mean_Group2 = round(estimate2, 2),
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
    Mean_Group1,
    Mean_Group2,
    Mean_Difference,
    t,
    df,
    p_raw,
    p_bonferroni,
    significant_bonferroni,
    CI_Lower,
    CI_Upper
  )

print(gender_ttest_table)


# ------------------------------------------------------------
# 6. Plot
# ------------------------------------------------------------

sex_learning_plot <- ggplot(
  learning_by_sex,
  aes(
    x = sex_enter,
    y = Learning,
    fill = sex_enter
  )
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
  # Cyan band around zero = little/no change in error
  annotate(
    "rect",
    xmin = -Inf,
    xmax = Inf,
    ymin = -5,
    ymax = 5,
    fill = "cyan",
    alpha = 0.15
  ) +
  
  # zero = no change in error
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "black",
    linewidth = 0.7
  ) +
  
  labs(
    x = "Sex",
    y = "Early-to-Late Error Reduction (cm)",
    title = "Learning by Sex",
    color = "Sex",
    fill = "Sex"
  ) +
  
  scale_y_continuous(
    breaks = seq(-100, 100, by = 25),
    expand = expansion(mult = c(0, 0))
  ) +
  
  coord_cartesian(
    ylim = c(-100, 100)
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
  
  facet_grid(
    . ~ speed_label,
    labeller = labeller(
      speed_label = c(
        "-3" = "Water Speed = 3 m/s",
        "-2" = "Water Speed = 2 m/s"
      )
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


# ------------------------------------------------------------
# 7. Save
# ------------------------------------------------------------

ggsave(
  filename = "sex_diff_learning_training1.png",
  plot = sex_learning_plot,
  width = 9,
  height = 5,
  dpi = 300
)
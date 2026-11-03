library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(purrr)

# ============================================================
# 1. LOAD NEW DATA
# ============================================================

data <- read.csv("Feb Study/Data/new_data.csv")

# ============================================================
# 2. SELECT TRAINING 1 ONLY
# ============================================================

analysis_data <- data %>%
  filter(
    phase == "training_1",
    speed_label == -3,
    sex %in% c("F", "M"),
    !is.na(flip_min_distance_xPCA_mean_bc)
  )

# ============================================================
# 3. SELECT FIRST 2 AND LAST 2 TARGET-SPECIFIC TRIALS
# ============================================================

learning_data_first_last <- analysis_data %>%
  group_by(
    ppid_full,
    sex,
    speed_label,
    target_x_label
  ) %>%
  arrange(phase_target_trial_num, .by_group = TRUE) %>%
  mutate(
    trial_rank = row_number(),
    total_trials = n()
  ) %>%
  filter(
    trial_rank <= 2 |
      trial_rank > total_trials - 2
  ) %>%
  mutate(
    period = ifelse(
      trial_rank <= 2,
      "Early",
      "Late"
    )
  ) %>%
  ungroup()

# ============================================================
# 4. CALCULATE EARLY AND LATE MEANS
# ============================================================

learning_by_sex <- learning_data_first_last %>%
  group_by(
    ppid_full,
    sex,
    speed_label,
    period
  ) %>%
  summarise(
    mean_error = mean(
      flip_min_distance_xPCA_mean_bc,
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
    Learning = Early - Late,
    sex = factor(
      sex,
      levels = c("F", "M"),
      labels = c("Female", "Male")
    )
  )

# ============================================================
# 5. CHECK PARTICIPANT NUMBERS
# ============================================================

cat("\n============================================\n")
cat("PARTICIPANT NUMBERS BY SEX\n")
cat("============================================\n")

learning_by_sex %>%
  group_by(sex) %>%
  summarise(
    participants = n_distinct(ppid_full),
    .groups = "drop"
  ) %>%
  print()

# ============================================================
# 6. WELCH INDEPENDENT-SAMPLES T-TEST
#    FEMALE VS MALE
# ============================================================

gender_ttest <- tidy(
  t.test(
    Learning ~ sex,
    data = learning_by_sex
  )
)

# ============================================================
# 7. FORMAT T-TEST RESULTS
# ============================================================

gender_ttest_table <- gender_ttest %>%
  mutate(
    Mean_Female = mean(
      learning_by_sex$Learning[
        learning_by_sex$sex == "Female"
      ],
      na.rm = TRUE
    ),
    
    Mean_Male = mean(
      learning_by_sex$Learning[
        learning_by_sex$sex == "Male"
      ],
      na.rm = TRUE
    ),
    
    SD_Female = sd(
      learning_by_sex$Learning[
        learning_by_sex$sex == "Female"
      ],
      na.rm = TRUE
    ),
    
    SD_Male = sd(
      learning_by_sex$Learning[
        learning_by_sex$sex == "Male"
      ],
      na.rm = TRUE
    ),
    
    N_Female = sum(learning_by_sex$sex == "Female"),
    N_Male = sum(learning_by_sex$sex == "Male"),
    
    Mean_Female = round(Mean_Female, 2),
    Mean_Male = round(Mean_Male, 2),
    SD_Female = round(SD_Female, 2),
    SD_Male = round(SD_Male, 2),
    
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
    
    significant = ifelse(
      p.value < .05,
      "Yes",
      "No"
    )
  ) %>%
  
  select(
    N_Female,
    N_Male,
    Mean_Female,
    SD_Female,
    Mean_Male,
    SD_Male,
    Mean_Difference,
    t,
    df,
    p,
    significant,
    CI_Lower,
    CI_Upper
  )

cat("\n============================================\n")
cat("GENDER DIFFERENCES IN LEARNING\n")
cat("Water Speed = 3 m/s\n")
cat("============================================\n")

print(gender_ttest_table, row.names = FALSE)

# ============================================================
# 8. PLOT
# ============================================================

sex_learning_plot <- ggplot(
  learning_by_sex,
  aes(
    x = sex,
    y = Learning,
    fill = sex
  )
) +
  
  # Violin distributions
  geom_violin(
    alpha = 0.15,
    width = 0.2,
    color = NA,
    bw = 15,
    adjust = 1.5,
    trim = FALSE
  ) +
  
  # Individual participant values
  geom_jitter(
    aes(color = sex),
    width = 0.1,
    alpha = 0.6
  ) +
  
  # Cyan band around zero = little/no change
  annotate(
    "rect",
    xmin = -Inf,
    xmax = Inf,
    ymin = -5,
    ymax = 5,
    fill = "cyan",
    alpha = 0.15
  ) +
  
  # Zero = no change in error
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
    subtitle = "First 2 vs. Last 2 Training Trials",
    color = "Sex",
    fill = "Sex"
  ) +
  
  scale_y_continuous(
    breaks = seq(-75, 150, by = 25),
    expand = expansion(mult = c(0, 0))
  ) +
  
  coord_cartesian(
    ylim = c(-75, 150)
  ) +
  
  scale_color_manual(
    values = c(
      "Female" = "red",
      "Male" = "blue"
    )
  ) +
  
  scale_fill_manual(
    values = c(
      "Female" = "red",
      "Male" = "blue"
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

print(sex_learning_plot)

# ============================================================
# 9. SAVE PLOT
# ============================================================

ggsave(
  filename = "sex_diff_learning_new_data_first2_last2.png",
  plot = sex_learning_plot,
  width = 9,
  height = 5,
  dpi = 300
)

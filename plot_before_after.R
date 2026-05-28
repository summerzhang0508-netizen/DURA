library(ggplot2)
library(Hmisc)
library(dplyr)


data <- read.csv("early_late_4_phase_PCA_error_projectile_experiment _S1-projectile_experiment_etc_speed_neg3_0_neg2_0.csv")

#select speed and phase (for graph producing)
chosen_speeds <- c(-3, -2)
chosen_phases <- c("training_2")

plot_data <- data %>%
  filter(
    speed_label %in% chosen_speeds,
    phase %in% chosen_phases
  )

#define what is early and late (4 from each)
early_late_data <- plot_data %>%
  group_by(speed_label, phase, target_x_label) %>%
  mutate(
    trial_rank = dense_rank(trial_num),
    max_rank = max(trial_rank),
    period = case_when(
      trial_rank <= 4 ~ "Early",
      trial_rank > max_rank - 4 ~ "Late",
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup() %>%
  filter(!is.na(period)) %>%

#create custom x positions
mutate(
  period = factor(period, levels = c("Early", "Late")),
  target_x_label = factor(
    target_x_label,
    levels = c("L60", "L30", "R30", "R60")
  ),
  loc_num = as.numeric(target_x_label),
  x_pos = ifelse(period == "Early", loc_num, loc_num + 5)
)

#data summary (for CI)
summary_early_late <- early_late_data %>%
  group_by(speed_label, phase, period, target_x_label, x_pos) %>%
  summarise(
    Mean = mean(flip_min_distance_mPCA_mean_bc, na.rm = TRUE),
    SD = sd(flip_min_distance_mPCA_mean_bc, na.rm = TRUE),
    N = n(),
    SEM = SD / sqrt(N),
    CI_lo = Mean - 1.96 * SEM,
    CI_hi = Mean + 1.96 * SEM,
    .groups = "drop"
  )

#set line connecting position
summary_early_late <- summary_early_late %>%
  mutate(
    phase_x = ifelse(period == "Early", 2.5, 7.5)
  )

#ggplot set up
ggplot() +
  
  #point shade
  geom_violin(
    data = early_late_data,
    aes(
      x = x_pos,
      y = flip_min_distance_mPCA_mean_bc,
      group = interaction(speed_label, phase, x_pos),
      fill = target_x_label
    ),
    alpha = 0.15,
    width = 0.5,
    color = NA,
    bw = 20,
    adjust = 1,
    trim = FALSE
  ) +
  
    geom_jitter(
    data = early_late_data,
    aes(
      x = x_pos,
      y = flip_min_distance_mPCA_mean_bc,
      color = target_x_label
    ),
    width = 0.09,
    height = 0.6,
    alpha = 0.18,
    size = 1.5
  ) +
  
  #labels
  labs(
    title = "Test Graph (Early vs. Late)",
    x = "Trial Number",
    y = "Min Error to Target",
  ) +
  
  #set line
  geom_line(
    data = summary_early_late,
    aes(x = phase_x, y = Mean, color = target_x_label, group = target_x_label),
    linewidth = 1
  ) +
  
  #set point
  geom_point(
    data = summary_early_late,
    aes(x = phase_x, y = Mean, color = target_x_label),
    size = 2.5,
    shape = 1
  ) +
  
  #set error bar
  geom_errorbar(
    data = summary_early_late,
    aes(x = phase_x, ymin = CI_lo, ymax = CI_hi, color = target_x_label),
    width = 0.05,
    linewidth = 1.2
  ) +
  
  #Set y scale
  scale_y_continuous(
    breaks = seq(-100, 150, by = 25),
    expand = expansion(mult = c(0, 0))
  ) +
  coord_cartesian(
    ylim = c(-100, 150)
  ) +
  #set color
  scale_color_manual(values = c(
    "L60" = "#06472a",
    "L30" =  "#ff4500",
    "R30" = "#1f00df",
    "R60" =  "#ff0404"
  )) +
  
  scale_x_continuous(
    breaks = c(1, 2, 3, 4, 6, 7, 8, 9),
    labels = c("L60", "L30", "R30", "R60",
               "L60", "L30", "R30", "R60")
  ) +
  
  #color for shade
  scale_fill_manual(values = c(
    "L60" = "darkgreen",
    "L30" =  "#ff4500",
    "R30" = "#1f00df",
    "R60" =  "#ff0404"
  )) +
   facet_grid(speed_label ~ phase) +
  

  theme_minimal()+
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "lightgrey"),
    panel.grid.minor.y = element_blank(),
    axis.line.y.left = element_line(color = "lightgrey"),
    axis.text.y = element_text(size = 8),
    panel.spacing.y = unit(0.9, "cm")
    
  )


# Save plot
ggsave(
  filename = "test_early_late.png",
  plot = b_a_plot,
  width = 8,
  height = 5
)


  
  
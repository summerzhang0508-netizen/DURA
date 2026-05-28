library(ggplot2)
library(Hmisc)
library(dplyr)

#set up file location
setwd("C:/Users/user/Documents/R_DURA")
#read file

data <- read.csv("early_late_4_phase_PCA_error_projectile_experiment _S1-projectile_experiment_etc_speed_neg3_0_neg2_0.csv")
#data <- read.csv("2_3_data.csv")

#select speed and phase (for graph producing)
chosen_speeds <- c(-3, -2)

training_data <- data %>%
  filter(
    speed_label %in% chosen_speeds,
    phase %in% c("training_1", "training_2")
  )

# keep first cycle (first 4 trials)
first_cycle_data <- training_data %>%
  group_by(speed_label, phase, target_x_label) %>%
  mutate(
    trial_rank = phase_target_trial_num,
    .group = "drop"
  ) %>%

  filter(trial_rank <= 4)



mean_first_cycle <- first_cycle_data %>%
  group_by(speed_label, phase, target_x_label) %>%
  summarise(
    Mean = mean(flip_min_distance_mPCA_mean_bc, na.rm = TRUE),
    SD = sd(flip_min_distance_mPCA_mean_bc, na.rm = TRUE),
    N = n(),
    SEM = SD / sqrt(N),
    CI_lo = Mean - 1.96 * SEM,
    CI_hi = Mean + 1.96 * SEM,
    .groups = "drop"
  )

#create custom x positions
first_cycle_data <- first_cycle_data %>%
mutate(
  train_x = factor(phase, levels = c("training_1", "training_2")),
  target_x_label = factor(
    target_x_label,
    levels = c("L60", "L30", "R30", "R60")
  ),
  loc_num = as.numeric(target_x_label),
  x_pos = ifelse(phase == "training_1", loc_num, loc_num + 5)
)

#set line connecting position
mean_first_cycle <- mean_first_cycle %>%
  mutate(
    line_x = ifelse(phase == "training_1", 4.5, 5.5)
  )


#print(mean_first_cycle)

first_4_plot <- ggplot() +
  
  #point shade
  geom_violin(
    data = first_cycle_data,
    aes(
      x = x_pos,
      y = flip_min_distance_mPCA_mean_bc,
      group = interaction(speed_label, phase, x_pos),
      fill = target_x_label
    ),
    alpha = 0.15,
    width = 0.4,
    color = NA,
    #bw = 20,
    adjust = 2,
    trim = FALSE
  ) +
  
  geom_jitter(
    data = first_cycle_data,
    aes(
      x = x_pos,
      y = flip_min_distance_mPCA_mean_bc,
      color = target_x_label,
      group = interaction(speed_label, phase, x_pos),
    ),
    width = 0.1,
    height = 0.6,
    alpha = 0.18,
    size = 1.5
  ) +
  
  #labels
  labs(
    title = "Test Graph (First 4 Trial)",
    x = "Phase",
    y = "Min Error to Target",
  ) +
  
  #set line
  geom_line(
    data = mean_first_cycle,
    aes(x = line_x, y = Mean, color = target_x_label, group = target_x_label),
    linewidth = 1
  ) +
  
  geom_errorbar(
    data = mean_first_cycle,
    aes(x = line_x, ymin = CI_lo, ymax = CI_hi, color = target_x_label),
    width = 0.05,
    linewidth = 1.2
  ) +
  
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
    "L60" = "#06472a",
    "L30" =  "#ff4500",
    "R30" = "#1f00df",
    "R60" =  "#ff0404"
  )) +

  theme_minimal()+
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "lightgrey"),
    panel.grid.minor.y = element_blank(),
    axis.line.y.left = element_line(color = "lightgrey"),
    panel.spacing.y = unit(0.9, "cm")
  )+
  facet_grid(speed_label ~ .)

# Save plot
ggsave(
  filename = "test_first_4.png",
  plot = first_4_plot,
  width = 15,
  height = 8
)
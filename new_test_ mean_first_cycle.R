library(ggplot2)
library(Hmisc)
library(dplyr)

#set up file location
setwd("C:/Users/user/Documents/R_DURA")
#read file

data <- read.csv("2_3_data.csv")

chosen_speeds <- c(-3)

training_data <- data %>%
  filter(
    speed_label %in% chosen_speeds,
    phase %in% c("training_1", "training_2")
  )

# keep first cycle (first 4 trials)
early_4_data <- training_data %>%
  group_by(speed_label, phase, target_x_label) %>%
  mutate(
    trial_rank = phase_target_trial_num,
    .group = "drop"
  ) %>%
  filter(trial_rank <= 4)


early_4_data <- early_4_data %>%
  mutate(
    train_x = factor(phase, levels = c("training_1", "training_2")),
    target_x_label = factor(
      target_x_label,
      levels = c("L60", "L30", "R30", "R60")
    ),
    loc_num = as.numeric(target_x_label),
    x_pos = ifelse(phase == "training_1", loc_num, loc_num + 5)
  )

ggplot()+
  
  geom_jitter(
    data = early_4_data,
    aes(
      x = x_pos,
      y = flip_min_distance_mPCA_mean_bc,
      color = target_x_label,
      group = interaction(ppid_full, phase, x_pos),
    ),
    width = 0.2,
    height = 0.5,
    alpha = 1,
    size = 1.5
  ) +
  
  scale_y_continuous(
    breaks = seq(-100, 150, by = 25),
    limits = c(-100, 150)
    
  )

library(ez)
library(dplyr)
library(tidyr)

# Set working directory
setwd("C:/Users/user/Documents/R_DURA")

# Read data
data <- read.csv("2_3_data.csv")

# Filter data
anova_group <- data %>%
  filter(
    speed_label == -3,
    phase %in% c("baseline", "training_1"),
    target_x_label %in% c("L60", "R30"),
    set_order %in% c("63_36")
  ) %>%
  mutate(
    ppid_full = factor(ppid_full),
    phase = factor(phase),
    target_x_label = factor(target_x_label)
  )

anova_result <- ezANOVA(
  data = anova_group,
  dv = flip_min_distance_mPCA_mean_bc,
  wid =  ppid_full,
  within = .(phase, target_x_label),
  detailed = TRUE
)

print(anova_result)





library(readr)
library(ggplot2)
library(Hmisc)
library(dplyr)

#set up file location
#read file
data <- read.csv("2_3_data.csv")

#select speed (for graph producing)
chosen_speed <- -3
plot_data <- subset(data, speed_label == chosen_speed)

#data summary (for CI)
summary_data <- plot_data %>%
  group_by(speed_label, trial_num, target_x_label) %>%
  summarise(
    Mean = mean(flip_min_distance_mPCA_mean_bc, na.rm = TRUE),
    SD = sd(flip_min_distance_mPCA_mean_bc, na.rm = TRUE),
    N = n(),
    SEM = SD / sqrt(N),
    CI_lo = Mean - 1.96 * SEM,
    CI_hi = Mean + 1.96 * SEM,
    #color = factor(target_x_label)
  )

#plot the graph
line_plot <- ggplot(
  summary_data,
  mapping = aes(x = trial_num, y = Mean, color = target_x_label)
  ) +
  
  #add in CI
  geom_ribbon(aes(ymin = CI_lo, 
                  ymax = CI_hi, 
                  fill = target_x_label, 
                  group = target_x_label
  ), 
  linetype = 1, 
  alpha = 0.2, 
  color = NA
  ) +
  
  #labels
  labs(
    title = "Test Graph",
    x = "Trials",
    y = "Min Error to Target",
    color = "Location",
    
  ) +

  #set line and point size
  geom_line(aes(color = target_x_label), linewidth = 0.72) +
  geom_point(aes(color = target_x_label), size = 0.7) +
  
  #Set y scale
  scale_y_continuous(
    breaks = seq(-110, 110, by = 20),
    limits = c(-110, 110),
    expand = c(0, 0)
    ) +

  #set line color
  scale_color_manual(values = c(
    "R60" = "#ea242d",
    "R30" =  "#fb8e2e",
    "L30" =  "#2557fc",
    "L60" = "#31c753"
  )) +
  
  scale_fill_manual(values = c(
    "R60" = "pink",
    "R30" = "gold",
    "L30" = "lightblue",
    "L60" = "lightgreen"
  )) +

  
  theme_minimal()

# Save plot
ggsave(
  filename = "test_line.png",
  plot = line_plot,
  width = 15,
  height = 6
)

# Confirm it exists
file.exists("test.pdf")












library(readr)
library(ggplot2)
library(Hmisc)
library(dplyr)

#read file
data <- read.csv("2_3_data.csv")

#select speed (for graph producing)
chosen_speed <- -2
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

summary_data$target_x_label <- factor(
  summary_data$target_x_label,
  levels = c("L60", "L30", "R30", "R60")
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
    title = "Mean Minimum Error to Target Across Trials",
    x = "Trials",
    y = "Min Error to Target",
    color = "Target ID",
    
  ) +

  #set line and point size
  geom_line(aes(color = target_x_label), linewidth = 0.55) +
  geom_point(aes(color = target_x_label), size = 0.5) +
  
  #Set y scale
  scale_y_continuous(
    breaks = seq(-110, 110, by = 20),
    limits = c(-110, 110),
    expand = c(0, 0)
    ) +

  #set line color
  scale_color_manual(values = c(
    "L60" = "darkgreen",
    "L30" =  "#ff4500",
    "R30" = "#1f00df",
    "R60" =  "#ff0404"
  )) +
  
  scale_fill_manual(values = c(
    "L60" = "#66c266",
    "L30" = "#ff8c66",
    "R30" = "#7da0ff",
    "R60" = "#ff8080"
    ),
    guide = "none"
  ) +
  
  annotate(
    "text",
    x = 1,
    y = 105,
    label = "Water Speed = 2",
    hjust = 0,
    size = 4
  ) +
  
  theme_minimal() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    ),
    legend.title = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    strip.text.y = element_text(face = "bold"),
    axis.text.x.top = element_text(face = "bold"),
    axis.ticks.x.top = element_blank()
  )

# Save plot
ggsave(
  filename = "test_line.png",
  plot = line_plot,
  width = 9,
  height = 5
)

# Confirm it exists
file.exists("test.pdf")












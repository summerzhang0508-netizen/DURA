library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)

data <- read.csv("demo_data.csv")

source("function_call.R")

# select data for analysis
analysis_data <- data %>%
  select_phase(c("training_1", "training_2")) %>%
  select_speed(-2) 

# create Early/Late labels
learning_data_early_late <- create_early_late(
  analysis_data,
  n_trials = 4
)

learning_dv1 <- learning_data_early_late %>%
  group_by(ppid_full, sex_enter, speed_label, period, phase) %>%
  summarise(
    mean_error = mean(flip_min_distance_mPCA_mean_bc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
   phase = factor(
     phase,
      levels = c("training_1", "training_2"),
      labels = c("Training 1", "Training 2")
    )
  ) %>%
  pivot_wider(
    names_from = period,
    values_from = mean_error
  ) %>%
  filter(!is.na(Early), !is.na(Late)) %>%
  mutate(
    DV1 = Late - Early
  )

mean_dv1 <- learning_dv1 %>%
  group_by(speed_label, phase, sex_enter) %>%
  summarise(
    mean_DV1 = mean(DV1, na.rm = TRUE),
    .groups = "drop"
  )

#t-tests
train_t <- t.test(
  DV1 ~ sex_enter,
  data = learning_dv1 %>%
    filter(
      phase == "Training 1",
      speed_label == -2
    )
)

train_table <- tidy(train_t) %>%
  transmute(
    Mean_F = round(estimate1, 2),
    Mean_M = round(estimate2, 2),
    Mean_Difference = round(estimate, 2),
    t = round(statistic, 2),
    df = round(parameter, 2),
    p = ifelse(p.value < 0.001, "< .001", sprintf("%.3f", p.value)),
    CI_Lower = round(conf.low, 2),
    CI_Upper = round(conf.high, 2)
  )

print(train_table)

sex_early_late_plot <- ggplot(learning_dv1, aes(x = sex_enter, y = DV1, fill = sex_enter
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
    data = learning_dv1,
    width = 0.1,
    alpha = 0.6,
    aes(color = sex_enter),
  ) +
  
  labs(
    x = "Sex",
    y = "Late - Early Min Error to Target",
    title = "Violin Plots of Learning by Sex",
    color= "Sex",
    fill = "Sex"
  ) +
  
  #Set y scale
  scale_y_continuous(
    breaks = seq(-100, 100, by = 25),
    expand = expansion(mult = c(0, 0))
  ) +
  coord_cartesian(
    ylim = c(-100, 100)
  ) +
  
  scale_color_manual(values = c(
    "F" = "red",
    "M" = "blue"
    )
  ) +
  
  #color for shade
  scale_fill_manual(values = c(
    "F" = "red",
    "M" = "blue"
    )
  ) +
  
  facet_grid(
    speed_label ~ phase,
    labeller = labeller(
      speed_label = c(
        "-3" = "Water Speed = 3",
        "-2" = "Water Speed = 2"
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
    panel.spacing.y = unit(0.9, "cm"),
    legend.title = element_text(face = "bold"),
    axis.title.x = element_text(face = "bold"),
    axis.title.y = element_text(face = "bold"),
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    ),
    strip.text.y = element_text(face = "bold"),
    axis.text.x.top = element_text(face = "bold"),
    axis.ticks.x.top = element_blank()
  )

# Save plot
ggsave(
  filename = "sex_diff_early_late.png",
  plot = sex_early_late_plot,
  width = 8,
  height = 5
)



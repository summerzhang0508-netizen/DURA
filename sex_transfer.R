library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)

data <- read.csv("demo_data.csv")

source("function_call.R")

# select training 1 and training 2
analysis_data <- data %>%
  select_phase(c("training_1", "training_2")) %>%
  select_speed(c(-3, -2))

# get first 4 trials from each phase
early_transfer_data <- create_first_4(
  analysis_data,
  n_trials = 4
)

# compute Early means for each participant, speed, phase
transfer_dv2 <- early_transfer_data %>%
  group_by(ppid_full, sex_enter, speed_label, phase) %>%
  summarise(
    mean_error = mean(flip_min_distance_mPCA_mean_bc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    phase = factor(
      phase,
      levels = c("training_1", "training_2"),
      labels = c("Early_1", "Early_2")
    )
  ) %>%
  pivot_wider(
    names_from = phase,
    values_from = mean_error
  ) %>%
  filter(!is.na(Early_1), !is.na(Early_2)) %>%
  mutate(
    DV2 = Early_2 - Early_1
  )

transfer_t <- t.test(
  DV2 ~ sex_enter,
  data = transfer_dv2 %>%
    filter(speed_label == -2)
)

transfer_t_table <- tidy(transfer_t) %>%
  transmute(
    Mean_F = round(estimate1, 2),
    Mean_M = round(estimate2, 2),
    Mean_Difference = round(estimate, 2),
    t = round(statistic, 2),
    df = round(parameter, 2),
    p = ifelse(p.value < .001, "< .001", sprintf("%.3f", p.value)),
    CI_Lower = round(conf.low, 2),
    CI_Upper = round(conf.high, 2)
  )

print(transfer_t_table)

sex_transfer <- ggplot(transfer_dv2, aes(x = sex_enter, y = DV2, fill = sex_enter)) +
  
  geom_violin(
    alpha = 0.15,
    width = 0.5,
    color = NA,
    bw = 15,
    adjust = 1,
    trim = FALSE
  ) +
  
  geom_jitter(
    data = transfer_dv2,
    width = 0.1,
    alpha = 0.6,
    aes(color = sex_enter),
  ) +

  facet_wrap(~ speed_label,
             labeller = labeller(
               speed_label = c(
                 "-3" = "Water Speed = 3",
                 "-2" = "Water Speed = 2"
               )
             )
            ) +
  
  #Set y scale
  scale_y_continuous(
    breaks = seq(-100, 100, by = 25),
    expand = expansion(mult = c(0, 0))
  ) +
  coord_cartesian(
    ylim = c(-100, 100)
  ) +
  labs(
    x = "Sex",
    y = "Early 2 - Early 1 Min Error to Target",
    title = "Violin Plots of Transfer by Sex",
    color = "Sex",
    fill = "Sex"
  ) +
  scale_color_manual(values = c("F" = "red", "M" = "blue")) +
  scale_fill_manual(values = c("F" = "red", "M" = "blue")) +
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
  filename = "sex_diff_transfer.png",
  plot = sex_transfer,
  width = 8,
  height = 5
)


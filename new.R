library(dplyr)
library(ggplot2)

paired_data <- read.csv("2_3_data.csv") %>%
  mutate(study = "Paired target")

single_data <- read.csv("new_data.csv") %>%
  mutate(study = "Single target")


# Select Training Phase 1 and trained targets
targets_to_compare <- c("L60", "R60")

paired_learning <- paired_data %>%
  filter(
    phase == "training_1",
    target_x_label %in% targets_to_compare
  )

single_learning <- single_data %>%
  filter(
    phase == "training_1",
    target_x_label %in% targets_to_compare
  )


# Check that both datasets still contain observations
paired_learning %>%
  count(study, target_x_label) %>%
  print()

single_learning %>%
  count(study, target_x_label) %>%
  print()


# Compare the first 150 trials within Training Phase 1
common_trial_limit <- 150

paired_learning <- paired_learning %>%
  filter(phase_trial_num <= common_trial_limit)

single_learning <- single_learning %>%
  filter(phase_trial_num <= common_trial_limit)


# Check again after trial filtering
cat("Paired rows:", nrow(paired_learning), "\n")
cat("Single rows:", nrow(single_learning), "\n")

paired_learning %>%
  count(study, target_x_label) %>%
  print()

single_learning %>%
  count(study, target_x_label) %>%
  print()


# Combine datasets
learning_data <- bind_rows(
  paired_learning,
  single_learning
) %>%
  mutate(
    participant = interaction(
      study,
      ppid_full,
      drop = TRUE
    ),
    
    study = factor(
      study,
      levels = c(
        "Paired target",
        "Single target"
      )
    ),
    
    target_x_label = factor(
      target_x_label,
      levels = c("L60", "R60")
    )
  )


# Confirm both studies are present
learning_data %>%
  count(study, target_x_label) %>%
  print()


# One mean per participant, study, target, and phase trial
participant_trial_data <- learning_data %>%
  group_by(
    participant,
    study,
    target_x_label,
    phase_trial_num
  ) %>%
  summarise(
    error = mean(
      flip_min_distance_mPCA_mean_bc,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# Group summary
learning_summary <- participant_trial_data %>%
  group_by(
    study,
    target_x_label,
    phase_trial_num
  ) %>%
  summarise(
    mean_error = mean(error, na.rm = TRUE),
    sd_error = sd(error, na.rm = TRUE),
    n = sum(!is.na(error)),
    sem = sd_error / sqrt(n),
    .groups = "drop"
  )

print(learning_summary)

learning_summary %>%
  count(study, target_x_label) %>%
  print()


# Plot
learning_graph <- ggplot(
  learning_summary,
  aes(
    x = phase_trial_num,
    y = mean_error,
    group = study,
    linetype = study
  )
) +
  geom_ribbon(
    aes(
      ymin = mean_error - sem,
      ymax = mean_error + sem,
      fill = study
    ),
    alpha = 0.15,
    colour = NA
  ) +
  geom_line(linewidth = 1) +
  facet_wrap(
    ~ target_x_label,
    ncol = 1
  ) +
  labs(
    title = "Learning During Single- and Paired-Target Training",
    subtitle = "First 150 trials of Training Phase 1",
    x = "Training Phase 1 Trial",
    y = "Mean Error",
    linetype = "Training condition",
    fill = "Training condition"
  ) +
  theme_classic() +
  theme(
    legend.position = "top",
    strip.background = element_blank()
  )

print(learning_graph)
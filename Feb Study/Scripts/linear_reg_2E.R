
# ============================================================
# TRAINING 1 -> TRAINING 2 TARGET SWITCH ANALYSIS
#
# Early 1 = first 2 trials of Training 1 target
# Early 2 = first 2 trials of Training 2 target
# Outcome  = Early 2 - Early 1
#
# Group 1: R60 -> L60 (RED)
# Group 2: L60 -> R60 (DARK GREEN)
# ============================================================


# ============================================================
# 1. LOAD PACKAGES
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(performance)


# ============================================================
# 2. LOAD DATA
# ============================================================

data <- read.csv(
  "Feb Study/Data/new_data.csv",
  stringsAsFactors = FALSE
)

questionnaire <- read.csv(
  "Feb Study/Data/participant_experience.csv",
  stringsAsFactors = FALSE
)


# ============================================================
# 3. CLEAN VARIABLES
# ============================================================

data <- data %>%
  mutate(
    ppid_full = trimws(as.character(ppid_full)),
    phase = trimws(as.character(phase)),
    target_x_label = trimws(as.character(target_x_label)),
    speed_label = as.numeric(as.character(speed_label)),
    phase_target_trial_num =
      as.numeric(as.character(phase_target_trial_num))
  )

questionnaire <- questionnaire %>%
  mutate(
    ppid_full = trimws(as.character(ppid_full))
  )


# ============================================================
# 4. MERGE QUESTIONNAIRE WITH EXPERIMENTAL DATA
# ============================================================

data_merged <- data %>%
  left_join(
    questionnaire,
    by = "ppid_full"
  )


# ============================================================
# 5. SELECT TRAINING 1 AND TRAINING 2 DATA
# ============================================================

transfer_data <- data_merged %>%
  filter(
    phase %in% c("training_1", "training_2"),
    speed_label == -3,
    target_x_label %in% c("L60", "R60"),
    !is.na(flip_min_distance_xPCA_mean_bc)
  ) %>%
  mutate(
    original_row = row_number()
  )

cat("\nRows after filtering:", nrow(transfer_data), "\n")


# Check phases and targets

cat("\nPhase counts:\n")
print(table(transfer_data$phase))

cat("\nTarget counts:\n")
print(table(transfer_data$target_x_label))

cat("\nPhase by target:\n")
print(table(transfer_data$phase, transfer_data$target_x_label))


# Stop if no data remain

if (nrow(transfer_data) == 0) {
  stop("No data remaining after filtering.")
}


# ============================================================
# 6. SELECT FIRST 2 TRIALS IN EACH TRAINING PHASE
#    FOR EACH PARTICIPANT
# ============================================================

transfer_early <- transfer_data %>%
  group_by(ppid_full, phase) %>%
  arrange(
    is.na(phase_target_trial_num),
    phase_target_trial_num,
    original_row,
    .by_group = TRUE
  ) %>%
  slice_head(n = 2) %>%
  ungroup()

cat("\nSelected early trials:", nrow(transfer_early), "\n")


# Check number of trials selected per participant and phase

cat("\nSelected trial counts per participant and phase:\n")

print(
  transfer_early %>%
    count(ppid_full, phase)
)


# ============================================================
# 7. CALCULATE EARLY 1 (TRAINING 1)
# ============================================================

early1 <- transfer_early %>%
  filter(phase == "training_1") %>%
  group_by(ppid_full) %>%
  summarise(
    Early1 = mean(
      flip_min_distance_xPCA_mean_bc,
      na.rm = TRUE
    ),
    target_Early1 = first(target_x_label),
    .groups = "drop"
  )


# ============================================================
# 8. CALCULATE EARLY 2 (TRAINING 2)
# ============================================================

early2 <- transfer_early %>%
  filter(phase == "training_2") %>%
  group_by(ppid_full) %>%
  summarise(
    Early2 = mean(
      flip_min_distance_xPCA_mean_bc,
      na.rm = TRUE
    ),
    target_Early2 = first(target_x_label),
    .groups = "drop"
  )


# ============================================================
# 9. MATCH TRAINING PHASES BY PARTICIPANT
#    DO NOT MATCH BY TARGET
# ============================================================

transfer_by_participant <- inner_join(
  early1,
  early2,
  by = "ppid_full"
) %>%
  mutate(
    transfer_score = Early2 - Early1,
    
    target_pairing = case_when(
      target_Early1 == "R60" & target_Early2 == "L60" ~
        "R60_to_L60",
      
      target_Early1 == "L60" & target_Early2 == "R60" ~
        "L60_to_R60",
      
      TRUE ~ NA_character_
    )
  )


# ============================================================
# 10. CHECK TARGET ORDER AND PARTICIPANT COUNTS
# ============================================================

cat("\nEarly 1 participants:", nrow(early1), "\n")
cat("Early 2 participants:", nrow(early2), "\n")

cat(
  "Matched participants:",
  nrow(transfer_by_participant),
  "\n"
)

cat("\nTarget order counts:\n")

print(
  table(
    transfer_by_participant$target_pairing,
    useNA = "always"
  )
)

cat("\nTarget sequence check:\n")

print(
  transfer_by_participant %>%
    count(target_Early1, target_Early2)
)


# ============================================================
# 11. ADD EXPERIMENT GROUP INFORMATION
# ============================================================

experiment_info <- data %>%
  select(ppid_full, experiment) %>%
  distinct()


transfer_by_participant <- transfer_by_participant %>%
  left_join(
    experiment_info,
    by = "ppid_full"
  )


# ============================================================
# 12. CHECK EXPERIMENT GROUP MAPPING
# ============================================================

cat("\nExperiment group counts:\n")
print(table(transfer_by_participant$experiment, useNA = "always"))

cat("\nExperiment by target pairing:\n")

print(
  table(
    transfer_by_participant$experiment,
    transfer_by_participant$target_pairing,
    useNA = "always"
  )
)


# ============================================================
# 13. CODE QUESTIONNAIRE RESPONSES
# ============================================================

# Convert responses to lowercase and remove surrounding spaces

normalize_response <- function(x) {
  tolower(trimws(as.character(x)))
}


response_codes <- c(
  "never" = 0,
  "once a year +" = 1,
  "once a month" = 2,
  "once a week" = 3,
  "multiple times per week" = 4
)


transfer_by_participant <- transfer_by_participant %>%
  left_join(
    questionnaire %>%
      select(
        ppid_full,
        gaming_frequency,
        vr_experience,
        sports_participation
      ) %>%
      distinct(),
    by = "ppid_full"
  ) %>%
  mutate(
    gaming_clean = normalize_response(gaming_frequency),
    vr_clean = normalize_response(vr_experience),
    sports_clean = normalize_response(sports_participation),
    
    gaming_score = unname(
      response_codes[gaming_clean]
    ),
    
    vr_score = unname(
      response_codes[vr_clean]
    ),
    
    sports_score = unname(
      response_codes[sports_clean]
    ),
    
    target_pairing = factor(
      target_pairing,
      levels = c("R60_to_L60", "L60_to_R60")
    )
  )


# ============================================================
# 14. CHECK QUESTIONNAIRE CODING
# ============================================================

cat("\nGaming questionnaire responses:\n")
print(table(transfer_by_participant$gaming_clean, useNA = "always"))

cat("\nVR questionnaire responses:\n")
print(table(transfer_by_participant$vr_clean, useNA = "always"))

cat("\nSports questionnaire responses:\n")
print(table(transfer_by_participant$sports_clean, useNA = "always"))


cat("\nMissing scores:\n")

print(
  transfer_by_participant %>%
    summarise(
      missing_transfer = sum(is.na(transfer_score)),
      missing_gaming = sum(is.na(gaming_score)),
      missing_vr = sum(is.na(vr_score)),
      missing_sports = sum(is.na(sports_score)),
      missing_pairing = sum(is.na(target_pairing))
    )
)


# ============================================================
# 15. PREPARE COMPLETE REGRESSION DATA
# ============================================================

regression_data <- transfer_by_participant %>%
  select(
    ppid_full,
    Early1,
    Early2,
    target_Early1,
    target_Early2,
    transfer_score,
    target_pairing,
    experiment,
    gaming_score,
    vr_score,
    sports_score
  ) %>%
  drop_na()


cat("\nFinal regression sample size:\n")
print(nrow(regression_data))


if (nrow(regression_data) == 0) {
  stop(
    "Regression dataset is empty. ",
    "Check questionnaire response coding and missing data."
  )
}


# Check one row per participant

if (anyDuplicated(regression_data$ppid_full)) {
  stop("There are duplicate participant rows in the regression dataset.")
}


# ============================================================
# 16. LINEAR REGRESSION
# ============================================================

# One change score per participant.
# Target order is included as a predictor.

model <- lm(
  transfer_score ~
    gaming_score +
    vr_score +
    sports_score +
    target_pairing,
  data = regression_data
)


# ============================================================
# 17. REGRESSION RESULTS
# ============================================================

cat("\n============================================\n")
cat("REGRESSION SUMMARY\n")
cat("============================================\n")

print(summary(model))


cat("\n============================================\n")
cat("COEFFICIENTS WITH 95% CONFIDENCE INTERVALS\n")
cat("============================================\n")

coefficients <- broom::tidy(
  model,
  conf.int = TRUE
)

print(coefficients)


cat("\n============================================\n")
cat("MODEL PERFORMANCE\n")
cat("============================================\n")

model_fit <- broom::glance(model)

print(model_fit)


cat("\n============================================\n")
cat("MODEL DIAGNOSTICS\n")
cat("============================================\n")

print(check_collinearity(model))
print(check_heteroscedasticity(model))
print(check_normality(model))


# ============================================================
# 18. PAIRING COLOURS
# ============================================================

pairing_colours <- c(
  "R60_to_L60" = "#ff0404",
  "L60_to_R60" = "darkgreen"
)


pairing_labels <- c(
  "R60_to_L60" = "R60 -> L60 (Group 1)",
  "L60_to_R60" = "L60 -> R60 (Group 2)"
)


# ============================================================
# 19. PLOT FUNCTION
# ============================================================

make_plot <- function(data, x_variable, x_label, plot_title) {
  
  ggplot(
    data,
    aes(
      x = .data[[x_variable]],
      y = transfer_score,
      colour = target_pairing
    )
  ) +
    
    # Participant observations
    geom_jitter(
      width = 0.08,
      height = 0,
      alpha = 0.8
    ) +
    
    # Linear trend with confidence interval
    geom_smooth(
      aes(group = target_pairing),
      method = "lm",
      se = TRUE,
      linewidth = 1
    ) +
    
    # Exploratory LOESS curve (dashed)
    geom_smooth(
      aes(group = target_pairing),
      method = "loess",
      se = FALSE,
      linetype = "dashed",
      linewidth = 0.8,
      span = 1.2
    ) +
    
    scale_color_manual(
      values = pairing_colours,
      labels = pairing_labels
    ) +
    
    labs(
      title = plot_title,
      x = x_label,
      y = "Early Training 2 - Early Training 1",
      colour = "Target Pairing"
    ) +
    
    theme_classic()
}


# ============================================================
# 20. GAMING FREQUENCY PLOT
# ============================================================

gaming_plot <- make_plot(
  data = regression_data,
  x_variable = "gaming_score",
  x_label = "Gaming Frequency",
  plot_title = "Gaming Frequency and Training Change"
)

print(gaming_plot)


# ============================================================
# 21. VR EXPERIENCE PLOT
# ============================================================

vr_plot <- make_plot(
  data = regression_data,
  x_variable = "vr_score",
  x_label = "VR Experience",
  plot_title = "VR Experience and Training Change"
)

print(vr_plot)


# ============================================================
# 22. SPORTS PARTICIPATION PLOT
# ============================================================

sports_plot <- make_plot(
  data = regression_data,
  x_variable = "sports_score",
  x_label = "Sports Participation",
  plot_title = "Sports Participation and Training Change"
)

print(sports_plot)


# ============================================================
# 23. SAVE PLOTS
# ============================================================

ggsave(
  "gaming_training_change.png",
  gaming_plot,
  width = 7,
  height = 5,
  dpi = 300
)

ggsave(
  "vr_training_change.png",
  vr_plot,
  width = 7,
  height = 5,
  dpi = 300
)

ggsave(
  "sports_training_change.png",
  sports_plot,
  width = 7,
  height = 5,
  dpi = 300
)


# ============================================================
# 24. SAVE DATA AND RESULTS
# ============================================================

write.csv(
  transfer_by_participant,
  "training_change_all_participants.csv",
  row.names = FALSE
)

write.csv(
  regression_data,
  "training_change_regression_data.csv",
  row.names = FALSE
)

write.csv(
  coefficients,
  "training_change_regression_coefficients.csv",
  row.names = FALSE
)

write.csv(
  model_fit,
  "training_change_model_fit.csv",
  row.names = FALSE
)


# ============================================================
# 25. RESIDUAL DIAGNOSTIC PLOTS
# ============================================================

par(mfrow = c(2, 2))
plot(model)
par(mfrow = c(1, 1))


# ============================================================
# 26. COMPLETED
# ============================================================

cat("\n============================================\n")
cat("ANALYSIS COMPLETED\n")
cat("Outcome: Early Training 2 - Early Training 1\n")
cat("Group 1: R60 -> L60 (Red)\n")
cat("Group 2: L60 -> R60 (Dark Green)\n")
cat("============================================\n")

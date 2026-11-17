
# ============================================================
# TRANSFER ANALYSIS: LINEAR MIXED-EFFECTS REGRESSION
# Predicting transfer performance from individual differences
# Accounting for target and experimental group
# ============================================================


# ============================================================
# 1. LOAD PACKAGES
# ============================================================

library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(lmerTest)
library(broom.mixed)
library(performance)


# ============================================================
# 2. LOAD AND MERGE DATA
# ============================================================

data <- read.csv("Feb Study/Data/new_data.csv")

questionnaire <- read.csv(
  "Feb Study/Data/participant_experience.csv"
)

data_merged <- data %>%
  left_join(
    questionnaire,
    by = "ppid_full"
  )

# Check merge
stopifnot(nrow(data_merged) == nrow(data))

stopifnot(
  n_distinct(data_merged$ppid_full) == 36
)


# ============================================================
# 3. CHECK EXPERIMENTAL GROUPS
# ============================================================

# The experiment variable identifies the experimental group
# and the order in which participants experienced the targets.

participant_info <- data_merged %>%
  distinct(ppid_full, experiment)

# Each participant should belong to one experimental group
if (anyDuplicated(participant_info$ppid_full)) {
  stop("A participant has more than one experimental group.")
}

print(table(participant_info$experiment))


# ============================================================
# 4. SELECT BASELINE AND TRANSFER TEST, SPEED -3
# ============================================================

transfer_data <- data_merged %>%
  filter(
    phase %in% c("baseline", "washout_1_AT"),
    speed_label == -3,
    !is.na(flip_min_distance_xPCA_mean_bc),
    !is.na(target_x_label)
  )

cat("\nPhase distribution:\n")
print(table(transfer_data$phase))

cat("\nTarget distribution:\n")
print(table(transfer_data$target_x_label))


# ============================================================
# 5. SELECT FIRST TWO TRIALS FOR EACH TARGET
# ============================================================

transfer_early <- transfer_data %>%
  group_by(
    ppid_full,
    phase,
    target_x_label
  ) %>%
  arrange(
    phase_target_trial_num,
    .by_group = TRUE
  ) %>%
  mutate(
    trial_rank = row_number()
  ) %>%
  filter(
    trial_rank <= 2
  ) %>%
  mutate(
    period = case_when(
      phase == "baseline" ~ "Early1",
      phase == "washout_1_AT" ~ "Early2"
    )
  ) %>%
  ungroup()


# ============================================================
# 6. CALCULATE MEAN ERROR FOR EACH PARTICIPANT,
#    TARGET AND PERIOD
# ============================================================

transfer_by_target <- transfer_early %>%
  group_by(
    ppid_full,
    target_x_label,
    period
  ) %>%
  summarise(
    mean_error = mean(
      flip_min_distance_xPCA_mean_bc,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# ============================================================
# 7. CALCULATE TRANSFER SCORE FOR EACH TARGET
# ============================================================

transfer_by_target <- transfer_by_target %>%
  pivot_wider(
    names_from = period,
    values_from = mean_error
  ) %>%
  filter(
    !is.na(Early1),
    !is.na(Early2)
  ) %>%
  mutate(
    transfer_score = Early2 - Early1
  )

# Each participant can now have a separate score
# for each target.

cat("\nTransfer scores by target:\n")
print(table(transfer_by_target$target_x_label))

cat("\nNumber of target observations per participant:\n")

print(
  transfer_by_target %>%
    count(ppid_full) %>%
    count(n, name = "number_of_participants")
)


# ============================================================
# 8. MERGE WITH QUESTIONNAIRE AND EXPERIMENTAL GROUP
# ============================================================

regression_data <- transfer_by_target %>%
  left_join(
    questionnaire,
    by = "ppid_full"
  ) %>%
  left_join(
    participant_info,
    by = "ppid_full"
  )


# ============================================================
# 9. CODE QUESTIONNAIRE RESPONSES
# ============================================================

response_codes <- c(
  "never" = 0,
  "once a year +" = 1,
  "once a month" = 2,
  "once a week" = 3,
  "multiple times per week" = 4
)

regression_data <- regression_data %>%
  mutate(
    gaming_score = unname(
      response_codes[as.character(gaming_frequency)]
    ),
    
    vr_score = unname(
      response_codes[as.character(vr_experience)]
    ),
    
    sports_score = unname(
      response_codes[as.character(sports_participation)]
    ),
    
    target_x_label = factor(target_x_label),
    
    experiment = factor(experiment)
  )


# ============================================================
# 10. CHECK TARGETS AND EXPERIMENTAL GROUPS
# ============================================================

cat("\nTargets included:\n")
print(levels(regression_data$target_x_label))

cat("\nExperimental groups:\n")
print(table(regression_data$experiment))

cat("\nTargets by experimental group:\n")

print(
  regression_data %>%
    distinct(ppid_full, target_x_label, experiment) %>%
    count(experiment, target_x_label)
)


# ============================================================
# 11. PREPARE COMPLETE REGRESSION DATA
# ============================================================

regression_complete <- regression_data %>%
  select(
    ppid_full,
    transfer_score,
    gaming_score,
    vr_score,
    sports_score,
    target_x_label,
    experiment
  ) %>%
  drop_na() %>%
  mutate(
    target_x_label = droplevels(target_x_label),
    experiment = droplevels(experiment)
  )

cat("\nNumber of participants:\n")
print(n_distinct(regression_complete$ppid_full))

cat("\nNumber of observations:\n")
print(nrow(regression_complete))

# Check that there are enough observations
if (nrow(regression_complete) <= 6) {
  stop("Not enough observations for the proposed model.")
}


# ============================================================
# 12. RUN LINEAR MIXED-EFFECTS REGRESSION
# ============================================================

# Fixed effects:
# Gaming frequency
# VR experience
# Sports participation
# Target
# Experimental group / target order

# Random effect:
# Participant ID, to account for repeated observations

model <- lmer(
  transfer_score ~
    gaming_score +
    vr_score +
    sports_score +
    target_x_label +
    experiment +
    (1 | ppid_full),
  data = regression_complete,
  REML = TRUE
)


# ============================================================
# 13. DISPLAY REGRESSION RESULTS
# ============================================================

cat("\n============================================\n")
cat("LINEAR MIXED-EFFECTS REGRESSION\n")
cat("============================================\n")

cat("\nRegression formula:\n")
print(formula(model))

cat("\nRegression summary:\n")
print(summary(model))

cat("\nRegression coefficients with confidence intervals:\n")

coefficients <- broom.mixed::tidy(
  model,
  effects = "fixed",
  conf.int = TRUE
)

print(coefficients)

cat("\nModel fit:\n")
print(performance::model_performance(model))

cat("\nMarginal and conditional R-squared:\n")
print(performance::r2_nakagawa(model))


# ============================================================
# 14. SCATTERPLOTS COLOURED BY TARGET
# ============================================================

# GAMING FREQUENCY

gaming_plot <- ggplot(
  regression_complete,
  aes(
    x = gaming_score,
    y = transfer_score,
    colour = target_x_label
  )
) +
  geom_jitter(
    width = 0.08,
    height = 0
  ) +
  geom_smooth(
    aes(group = 1),
    method = "lm",
    se = TRUE,
    colour = "blue"
  ) +
  labs(
    title = "Gaming Frequency and Transfer Performance",
    x = "Gaming Frequency",
    y = "Transfer Score (Early 2 - Early 1)",
    colour = "Target"
  ) +
  theme_classic()


# VR EXPERIENCE

vr_plot <- ggplot(
  regression_complete,
  aes(
    x = vr_score,
    y = transfer_score,
    colour = target_x_label
  )
) +
  geom_jitter(
    width = 0.08,
    height = 0
  ) +
  geom_smooth(
    aes(group = 1),
    method = "lm",
    se = TRUE,
    colour = "blue"
  ) +
  labs(
    title = "VR Experience and Transfer Performance",
    x = "VR Experience",
    y = "Transfer Score (Early 2 - Early 1)",
    colour = "Target"
  ) +
  theme_classic()


# SPORTS PARTICIPATION

sports_plot <- ggplot(
  regression_complete,
  aes(
    x = sports_score,
    y = transfer_score,
    colour = target_x_label
  )
) +
  geom_jitter(
    width = 0.08,
    height = 0
  ) +
  geom_smooth(
    aes(group = 1),
    method = "lm",
    se = TRUE,
    colour = "blue"
  ) +
  labs(
    title = "Sports Participation and Transfer Performance",
    x = "Sports Participation",
    y = "Transfer Score (Early 2 - Early 1)",
    colour = "Target"
  ) +
  theme_classic()


# Display plots

print(gaming_plot)
print(vr_plot)
print(sports_plot)


# ============================================================
# 15. SAVE SCATTERPLOTS
# ============================================================

ggsave(
  "transfer_gaming_scatterplot_target.png",
  gaming_plot,
  width = 7,
  height = 5,
  dpi = 300
)

ggsave(
  "transfer_vr_scatterplot_target.png",
  vr_plot,
  width = 7,
  height = 5,
  dpi = 300
)

ggsave(
  "transfer_sports_scatterplot_target.png",
  sports_plot,
  width = 7,
  height = 5,
  dpi = 300
)


# ============================================================
# 16. MODEL DIAGNOSTICS
# ============================================================

cat("\n============================================\n")
cat("MODEL DIAGNOSTICS\n")
cat("============================================\n")

print(check_collinearity(model))
print(check_heteroscedasticity(model))
print(check_normality(model))

# Diagnostic plots

par(mfrow = c(2, 2))
plot(model)
par(mfrow = c(1, 1))


# ============================================================
# 17. SAVE REGRESSION RESULTS
# ============================================================

write.csv(
  regression_complete,
  "transfer_regression_target_data.csv",
  row.names = FALSE
)

write.csv(
  coefficients,
  "transfer_regression_target_coefficients.csv",
  row.names = FALSE
)

write.csv(
  performance::model_performance(model),
  "transfer_regression_target_model_fit.csv",
  row.names = FALSE
)


# ============================================================
# 18. COMPLETED
# ============================================================

cat("\n============================================\n")
cat("TRANSFER REGRESSION ANALYSIS COMPLETED!\n")
cat("============================================\n")

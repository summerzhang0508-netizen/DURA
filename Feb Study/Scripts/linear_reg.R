# ============================================================
# TRANSFER REGRESSION ANALYSIS
# Predicting transfer performance from individual differences
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
stopifnot(n_distinct(data_merged$ppid_full) == 36)


# ============================================================
# 3. SELECT BASELINE AND TRANSFER TEST, SPEED -3
# ============================================================

transfer_data <- data_merged %>%
  filter(
    phase %in% c("baseline", "washout_1_AT"),
    speed_label == -3,
    !is.na(flip_min_distance_xPCA_mean_bc)
  )


# Check that both phases are present
print(table(transfer_data$phase))


# ============================================================
# 4. SELECT FIRST 2 TARGET-SPECIFIC TRIALS
#    FOR EACH PARTICIPANT AND PHASE
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
# 5. CALCULATE EARLY1 AND EARLY2 MEANS
#    FIRST AVERAGE WITHIN EACH TARGET
#    THEN AVERAGE ACROSS TARGETS
# ============================================================

transfer_by_participant <- transfer_early %>%
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
  ) %>%
  group_by(
    ppid_full,
    period
  ) %>%
  summarise(
    mean_error = mean(
      mean_error,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
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


# Check transfer scores
print(head(transfer_by_participant))


# ============================================================
# 6. MERGE TRANSFER SCORES WITH QUESTIONNAIRE
# ============================================================

regression_data <- transfer_by_participant %>%
  left_join(
    questionnaire,
    by = "ppid_full"
  )


# ============================================================
# 7. CODE QUESTIONNAIRE RESPONSES
# ============================================================

# Never = 0
# Once a year + = 1
# Once a month = 2
# Once a week = 3
# Multiple times per week = 4

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
    )
  )


# ============================================================
# 8. CHECK PARTICIPANT NUMBERS
# ============================================================

cat("\n============================================\n")
cat("TRANSFER REGRESSION PARTICIPANT NUMBERS\n")
cat("============================================\n")

cat(
  "Participants with transfer scores:",
  nrow(transfer_by_participant),
  "\n"
)


# ============================================================
# 9. CHECK QUESTIONNAIRE DISTRIBUTIONS
# ============================================================

cat("\n============================================\n")
cat("QUESTIONNAIRE RESPONSE DISTRIBUTIONS\n")
cat("============================================\n")

print(table(regression_data$gaming_frequency))
print(table(regression_data$vr_experience))
print(table(regression_data$sports_participation))


# ============================================================
# 10. PREPARE COMPLETE REGRESSION DATA
# ============================================================

regression_complete <- regression_data %>%
  select(
    ppid_full,
    transfer_score,
    gaming_score,
    vr_score,
    sports_score
  ) %>%
  drop_na()

# Ensure one observation per participant
stopifnot(
  !anyDuplicated(regression_complete$ppid_full)
)

cat(
  "\nParticipants included in regression:",
  nrow(regression_complete),
  "\n"
)


# ============================================================
# 11. REUSABLE MULTIPLE LINEAR REGRESSION FUNCTION
# ============================================================

run_regression <- function(
    dataset,
    outcome,
    predictors,
    id = "ppid_full"
) {
  
  # Check that variables exist
  required_variables <- c(
    id,
    outcome,
    predictors
  )
  
  missing_variables <- setdiff(
    required_variables,
    names(dataset)
  )
  
  if (length(missing_variables) > 0) {
    stop(
      "Variables not found: ",
      paste(missing_variables, collapse = ", ")
    )
  }
  
  # Prepare complete cases
  regression_data <- dataset %>%
    select(all_of(required_variables)) %>%
    drop_na()
  
  # One observation per participant
  if (anyDuplicated(regression_data[[id]])) {
    stop("There must be one observation per participant.")
  }
  
  if (nrow(regression_data) <= length(predictors) + 1) {
    stop("Not enough complete participants for this model.")
  }
  
  # Build model formula
  regression_formula <- reformulate(
    predictors,
    response = outcome
  )
  
  # Run regression
  model <- lm(
    regression_formula,
    data = regression_data
  )
  
  # Extract results
  coefficients <- broom::tidy(
    model,
    conf.int = TRUE
  )
  
  model_fit <- broom::glance(model)
  
  # Return results
  list(
    data = regression_data,
    formula = regression_formula,
    model = model,
    coefficients = coefficients,
    model_fit = model_fit
  )
}


# ============================================================
# 12. SELECT OUTCOME AND PREDICTORS
# ============================================================

outcome <- "transfer_score"

predictors <- c(
  "gaming_score",
  "vr_score",
  "sports_score"
)


# ============================================================
# 13. RUN REGRESSION
# ============================================================

results <- run_regression(
  dataset = regression_data,
  outcome = outcome,
  predictors = predictors
)


# ============================================================
# 14. DISPLAY REGRESSION RESULTS
# ============================================================

cat("\n============================================\n")
cat("TRANSFER MULTIPLE LINEAR REGRESSION\n")
cat("============================================\n")

cat("\nRegression formula:\n")
print(results$formula)

cat("\nRegression summary:\n")
print(summary(results$model))

cat("\nRegression coefficients:\n")
print(results$coefficients)

cat("\nModel fit:\n")
print(results$model_fit)


# ============================================================
# 15. SCATTERPLOTS
# ============================================================

# GAMING FREQUENCY VS TRANSFER

gaming_plot <- ggplot(
  regression_complete,
  aes(
    x = gaming_score,
    y = transfer_score
  )
) +
  geom_jitter(
    width = 0.08,
    height = 0
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  geom_smooth(
    method = "loess",
    se = FALSE,
    linetype = "dashed"
  ) +
  labs(
    title = "Gaming Frequency and Transfer Performance",
    x = "Gaming Frequency",
    y = "Transfer Score (Early 2 - Early 1)"
  ) +
  theme_classic()


# VR EXPERIENCE VS TRANSFER

vr_plot <- ggplot(
  regression_complete,
  aes(
    x = vr_score,
    y = transfer_score
  )
) +
  geom_jitter(
    width = 0.08,
    height = 0
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  geom_smooth(
    method = "loess",
    se = FALSE,
    linetype = "dashed"
  ) +
  labs(
    title = "VR Experience and Transfer Performance",
    x = "VR Experience",
    y = "Transfer Score (Early 2 - Early 1)"
  ) +
  theme_classic()


# SPORTS PARTICIPATION VS TRANSFER

sports_plot <- ggplot(
  regression_complete,
  aes(
    x = sports_score,
    y = transfer_score
  )
) +
  geom_jitter(
    width = 0.08,
    height = 0
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE
  ) +
  geom_smooth(
    method = "loess",
    se = FALSE,
    linetype = "dashed"
  ) +
  labs(
    title = "Sports Participation and Transfer Performance",
    x = "Sports Participation",
    y = "Transfer Score (Early 2 - Early 1)"
  ) +
  theme_classic()


# Display plots
print(gaming_plot)
print(vr_plot)
print(sports_plot)


# ============================================================
# 16. SAVE SCATTERPLOTS
# ============================================================

ggsave(
  "transfer_gaming_scatterplot.png",
  gaming_plot,
  width = 7,
  height = 5,
  dpi = 300
)

ggsave(
  "transfer_vr_scatterplot.png",
  vr_plot,
  width = 7,
  height = 5,
  dpi = 300
)

ggsave(
  "transfer_sports_scatterplot.png",
  sports_plot,
  width = 7,
  height = 5,
  dpi = 300
)


# ============================================================
# 17. MODEL DIAGNOSTICS
# ============================================================

cat("\n============================================\n")
cat("MODEL DIAGNOSTICS\n")
cat("============================================\n")

print(check_collinearity(results$model))
print(check_heteroscedasticity(results$model))
print(check_normality(results$model))

par(mfrow = c(2, 2))
plot(results$model)
par(mfrow = c(1, 1))


# ============================================================
# 18. SAVE REGRESSION RESULTS
# ============================================================

write.csv(
  results$data,
  "transfer_regression_participant_data.csv",
  row.names = FALSE
)

write.csv(
  results$coefficients,
  "transfer_regression_coefficients.csv",
  row.names = FALSE
)

write.csv(
  results$model_fit,
  "transfer_regression_model_fit.csv",
  row.names = FALSE
)


# ============================================================
# 19. COMPLETED
# ============================================================

cat("\n============================================\n")
cat("TRANSFER REGRESSION ANALYSIS COMPLETED!\n")
cat("============================================\n")

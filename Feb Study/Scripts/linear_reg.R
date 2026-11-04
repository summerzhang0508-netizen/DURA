library(dplyr)
library(tidyr)
library(ggplot2)
library(broom)
library(performance)

# ============================================================
# 1. LOAD AND MERGE DATA
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
# 2. SELECT TRAINING 1, WATER SPEED -3
# ============================================================

analysis_data <- data_merged %>%
  filter(
    phase == "training_1",
    speed_label == -3,
    !is.na(flip_min_distance_xPCA_mean_bc)
  )


# ============================================================
# 3. SELECT FIRST 2 AND LAST 2 TARGET-SPECIFIC TRIALS
# ============================================================

learning_data <- analysis_data %>%
  group_by(
    ppid_full,
    target_x_label
  ) %>%
  arrange(
    phase_target_trial_num,
    .by_group = TRUE
  ) %>%
  mutate(
    trial_rank = row_number(),
    total_trials = n()
  ) %>%
  filter(
    trial_rank <= 2 |
      trial_rank > total_trials - 2
  ) %>%
  mutate(
    period = ifelse(
      trial_rank <= 2,
      "Early",
      "Late"
    )
  ) %>%
  ungroup()


# ============================================================
# 4. CALCULATE EARLY AND LATE MEANS
#    FIRST AVERAGE WITHIN EACH TARGET
#    THEN AVERAGE ACROSS TARGETS
# ============================================================

learning_by_participant <- learning_data %>%
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
    mean_error = mean(mean_error, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = period,
    values_from = mean_error
  ) %>%
  filter(
    !is.na(Early),
    !is.na(Late)
  ) %>%
  mutate(
    Learning = Early - Late
  )


# ============================================================
# 5. MERGE PARTICIPANT-LEVEL LEARNING WITH QUESTIONNAIRE
# ============================================================

regression_data <- learning_by_participant %>%
  left_join(
    questionnaire,
    by = "ppid_full"
  )


# ============================================================
# 6. CODE QUESTIONNAIRE RESPONSES
# ============================================================

# Ordinal coding:
# Never = 0
# Once a year or less = 1
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
# 7. CHECK PARTICIPANT NUMBERS
# ============================================================

cat("\n============================================\n")
cat("REGRESSION PARTICIPANT NUMBERS\n")
cat("============================================\n")

cat(
  "Participants with learning scores:",
  nrow(learning_by_participant),
  "\n"
)

cat(
  "Participants with complete questionnaire data:",
  sum(complete.cases(
    regression_data[, c(
      "Learning",
      "gaming_score",
      "vr_score",
      "sports_score"
    )]
  )),
  "\n"
)


# ============================================================
# 8. CHECK QUESTIONNAIRE DISTRIBUTIONS
# ============================================================

cat("\n============================================\n")
cat("QUESTIONNAIRE RESPONSE DISTRIBUTIONS\n")
cat("============================================\n")

print(table(regression_data$gaming_frequency))
print(table(regression_data$vr_experience))
print(table(regression_data$sports_participation))


# ============================================================
# 9. PREPARE COMPLETE REGRESSION DATA
# ============================================================

regression_complete <- regression_data %>%
  select(
    ppid_full,
    Learning,
    gaming_score,
    vr_score,
    sports_score
  ) %>%
  drop_na()

# Ensure one observation per participant
stopifnot(
  !anyDuplicated(regression_complete$ppid_full)
)




# ============================================================
# 10. REUSABLE MULTIPLE LINEAR REGRESSION
# ============================================================

run_regression <- function(
    dataset,
    outcome,
    predictors,
    id = "ppid_full"
) {
  
  # Check that variables exist
  required_variables <- c(id, outcome, predictors)
  
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
# 11. SELECT YOUR OUTCOME AND PREDICTORS
# ============================================================

# Current analysis:
# Do gaming, VR and sports experience predict learning?

outcome <- "Learning"

predictors <- c(
  "gaming_score",
  "vr_score",
  "sports_score"
)


# ============================================================
# 12. RUN REGRESSION
# ============================================================

results <- run_regression(
  dataset = regression_data,
  outcome = outcome,
  predictors = predictors
)


# ============================================================
# 13. DISPLAY RESULTS
# ============================================================

cat("\n============================================\n")
cat("MULTIPLE LINEAR REGRESSION\n")
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
# 14. MODEL DIAGNOSTICS
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
# 15. SAVE RESULTS
# ============================================================

write.csv(
  results$data,
  paste0("regression_", outcome, "_participant_data.csv"),
  row.names = FALSE
)

write.csv(
  results$coefficients,
  paste0("regression_", outcome, "_coefficients.csv"),
  row.names = FALSE
)

write.csv(
  results$model_fit,
  paste0("regression_", outcome, "_model_fit.csv"),
  row.names = FALSE
)

cat("\nRegression analysis completed!\n")


library(dplyr)

#define speed
select_speed <- function(data, speeds) {
  
  data %>%
    filter(
      speed_label %in% speeds
    )
}

# define set order
select_set_order <- function(data, set_order_value) {
  
  data %>%
    filter(
      set_order == set_order_value
    )
}

#define phase
select_phase <- function(data, phase_name) {
  
  data %>%
    filter(
      phase %in% phase_name
    )
}

#define sex
select_sex <- function(data, sex){
  data %>%
    filter(
      sex_enter %in% sex
    )
}

#define what is early and late(can be changed)
create_early_late <- function(data, n_trials) {
  
  data %>%
    group_by(ppid_full, speed_label, phase, target_x_label) %>%
    arrange(trial_num, .by_group = TRUE) %>%
    mutate(
      trial_rank = dense_rank(trial_num),
      max_rank = max(trial_rank),
      period = case_when(
        trial_rank <= n_trials ~ "Early",
        trial_rank > max_rank - n_trials ~ "Late",
        TRUE ~ NA_character_
      )
    ) %>%
    ungroup() %>%
    filter(!is.na(period)) %>%
    mutate(
      ppid_full = factor(ppid_full),
      period = factor(period, levels = c("Early", "Late")),
      target_x_label = factor(
      target_x_label,
      levels = c("L60", "L30", "R30", "R60")
    )
  )
}

# select first n trials per participant, phase, and target
create_first_4 <- function(data, n_trials) {
  
  data %>%
    group_by(ppid_full, speed_label, phase, target_x_label) %>%
    mutate(
      trial_rank = dense_rank(trial_num)
    ) %>%
    filter(
      trial_rank <= n_trials
    ) %>%
    ungroup()
}
# Transfer independent t-test
transfer_ttest <- function(data,
                           speed,
                           target,
                           training_group,
                           training_phase,
                           transfer_group,
                           transfer_phase) {
  
  test_data <- data %>%
    filter(
      speed_label == speed,
      
      (set_order == training_group &
         phase == training_phase &
         target_x_label == target) |
        
        (set_order == transfer_group &
           phase == transfer_phase &
           target_x_label == target)
    ) %>%
    mutate(
      condition = ifelse(
        set_order == training_group &
          phase == training_phase,
        "Training",
        "Transfer"
      )
    )
  
  # Welch independent-samples t-test
  test <- t.test(
    score ~ condition,
    data = test_data
  )
  
  tibble(
    Speed = abs(speed),
    Target = target,
    
    Mean_Training = mean(
      test_data$score[test_data$condition == "Training"],
      na.rm = TRUE
    ),
    
    Mean_Transfer = mean(
      test_data$score[test_data$condition == "Transfer"],
      na.rm = TRUE
    ),
    
    t = unname(test$statistic),
    df = unname(test$parameter),
    p = test$p.value
  )
}
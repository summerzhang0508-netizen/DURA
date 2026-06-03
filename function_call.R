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

#define what is early and late(can be changed)
create_early_late <- function(data, n_trials = 4) {
  
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
    filter(!is.na(period))
}

# select first n trials per participant, phase, and target
create_first_4 <- function(data, n_trials = 4) {
  
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
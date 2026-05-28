#Define early and late
group_by(speed_label, phase, target_x_label) %>%
  mutate(
    trial_rank = dense_rank(trial_num),
    max_rank = max(trial_rank),
    period = case_when(
      trial_rank <= 4 ~ "Early",
      trial_rank > max_rank - 4 ~ "Late",
      TRUE ~ NA_character_
    )
  ) %>%
  ungroup() %>%
  filter(!is.na(period)) %>%
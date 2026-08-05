# ============================================================
# MEAN MINIMUM ERROR ACROSS ALL TRIALS
#
# Backgrounds:
#   Baseline   = solid grey
#   Training 1 = Early and Late highlighted
#   Washout 1  = alternating grey every 8 trials
#   Training 2 = Early and Late highlighted
#   Washout 2  = alternating grey every 8 trials
# ============================================================

library(ggplot2)
library(dplyr)

# Load your create_early_late() function
# Change the filename if necessary
source("function_call.R")


# ------------------------------------------------------------
# 1. Read data
# ------------------------------------------------------------

data <- read.csv("2_3_data.csv")


# ------------------------------------------------------------
# 2. Select one water-speed condition
# ------------------------------------------------------------

chosen_speed <- -2

plot_data <- data %>%
  filter(speed_label == chosen_speed)


# ------------------------------------------------------------
# 3. Calculate means and confidence intervals
# ------------------------------------------------------------

summary_data <- plot_data %>%
  group_by(
    speed_label,
    trial_num,
    target_x_label
  ) %>%
  summarise(
    Mean = mean(
      flip_min_distance_mPCA_mean_bc,
      na.rm = TRUE
    ),
    
    SD = sd(
      flip_min_distance_mPCA_mean_bc,
      na.rm = TRUE
    ),
    
    # Number of non-missing observations
    N = sum(
      !is.na(flip_min_distance_mPCA_mean_bc)
    ),
    
    SEM = SD / sqrt(N),
    
    CI_lo = Mean - 1.96 * SEM,
    CI_hi = Mean + 1.96 * SEM,
    
    .groups = "drop"
  ) %>%
  mutate(
    target_x_label = factor(
      target_x_label,
      levels = c(
        "L60",
        "L30",
        "R30",
        "R60"
      )
    )
  )


# ------------------------------------------------------------
# 4. Create one solid background for baseline
# ------------------------------------------------------------

baseline_block <- plot_data %>%
  filter(phase == "baseline") %>%
  summarise(
    xmin = min(trial_num, na.rm = TRUE) - 0.5,
    xmax = max(trial_num, na.rm = TRUE) + 0.5
  )


# ------------------------------------------------------------
# 5. Identify Early and Late trials
#    using your external function
# ------------------------------------------------------------

early_late_data <- create_early_late(
  plot_data,
  n_trials = 4
)


# ------------------------------------------------------------
# 6. Create Early/Late background blocks
#    for Training 1 and Training 2 only
# ------------------------------------------------------------

training_period_blocks <- early_late_data %>%
  filter(
    phase %in% c(
      "training_1",
      "training_2"
    )
  ) %>%
  group_by(
    phase,
    period
  ) %>%
  summarise(
    xmin = min(trial_num, na.rm = TRUE) - 0.5,
    xmax = max(trial_num, na.rm = TRUE) + 0.5,
    .groups = "drop"
  ) %>%
  mutate(
    training_shade = case_when(
      phase == "training_1" &
        period == "Early" ~ "training1_early",
      
      phase == "training_1" &
        period == "Late" ~ "training1_late",
      
      phase == "training_2" &
        period == "Early" ~ "training2_early",
      
      phase == "training_2" &
        period == "Late" ~ "training2_late"
    )
  )

# Check the selected Early/Late ranges
print(training_period_blocks)


# ------------------------------------------------------------
# 7. Create alternating 8-trial blocks
#    for Washout 1 and Washout 2
# ------------------------------------------------------------

washout_blocks <- plot_data %>%
  filter(
    phase %in% c(
      "washout_1",
      "washout_2"
    )
  ) %>%
  group_by(phase) %>%
  mutate(
    # First trial of each washout phase
    phase_start = min(
      trial_num,
      na.rm = TRUE
    ),
    
    # Split each washout into groups of 8 trials
    block_number = floor(
      (trial_num - phase_start) / 8
    ),
    
    # Alternate the block colors
    shade = ifelse(
      block_number %% 2 == 0,
      "shade_1",
      "shade_2"
    )
  ) %>%
  group_by(
    phase,
    block_number,
    shade
  ) %>%
  summarise(
    xmin = min(trial_num, na.rm = TRUE) - 0.5,
    xmax = max(trial_num, na.rm = TRUE) + 0.5,
    .groups = "drop"
  )


# ------------------------------------------------------------
# 8. Create vertical lines between washout blocks
# ------------------------------------------------------------

washout_boundaries <- washout_blocks %>%
  group_by(phase) %>%
  arrange(
    block_number,
    .by_group = TRUE
  ) %>%
  
  # Exclude the first boundary because that is the
  # beginning of the washout phase
  filter(
    block_number > min(block_number)
  ) %>%
  
  transmute(
    phase,
    boundary = xmin
  ) %>%
  
  ungroup()


# ------------------------------------------------------------
# 9. Create vertical lines at phase changes
# ------------------------------------------------------------

phase_lines <- plot_data %>%
  group_by(phase) %>%
  summarise(
    end_trial = max(
      trial_num,
      na.rm = TRUE
    ) + 0.5,
    .groups = "drop"
  ) %>%
  
  # Do not draw a line after the final phase
  filter(
    end_trial < max(end_trial)
  )

print(phase_lines)


# ------------------------------------------------------------
# 10. Define target colors
# ------------------------------------------------------------

target_line_colors <- c(
  "L60" = "darkgreen",
  "L30" = "#ff4500",
  "R30" = "#1f00df",
  "R60" = "#ff0404"
)

target_fill_colors <- c(
  "L60" = "#66c266",
  "L30" = "#ff8c66",
  "R30" = "#7da0ff",
  "R60" = "#ff8080"
)


# ------------------------------------------------------------
# 11. Create the plot
# ------------------------------------------------------------

line_plot <- ggplot(
  data = summary_data,
  mapping = aes(
    x = trial_num,
    y = Mean,
    color = target_x_label
  )
) +
  
  # ----------------------------------------------------------
# Solid grey baseline background
# ----------------------------------------------------------

geom_rect(
  data = baseline_block,
  aes(
    xmin = xmin,
    xmax = xmax,
    ymin = -Inf,
    ymax = Inf
  ),
  inherit.aes = FALSE,
  fill = "grey80",
  alpha = 0.35,
  color = NA
) +
  
  # ----------------------------------------------------------
# Early/Late backgrounds during Training 1 and Training 2
# ----------------------------------------------------------

geom_rect(
  data = training_period_blocks,
  aes(
    xmin = xmin,
    xmax = xmax,
    ymin = -Inf,
    ymax = Inf,
    fill = training_shade
  ),
  inherit.aes = FALSE,
  alpha = 0.30,
  color = NA,
  show.legend = FALSE
) +
  
  # ----------------------------------------------------------
# Alternating backgrounds during Washout 1 and Washout 2
# ----------------------------------------------------------

geom_rect(
  data = washout_blocks,
  aes(
    xmin = xmin,
    xmax = xmax,
    ymin = -Inf,
    ymax = Inf,
    fill = shade
  ),
  inherit.aes = FALSE,
  alpha = 0.30,
  color = NA,
  show.legend = FALSE
) +
  
  # ----------------------------------------------------------
# Dashed boundaries every 8 trials during washouts
# ----------------------------------------------------------

geom_vline(
  data = washout_boundaries,
  aes(
    xintercept = boundary
  ),
  inherit.aes = FALSE,
  linetype = "dashed",
  color = "grey45",
  linewidth = 0.45
) +
  
  # ----------------------------------------------------------
# Dashed vertical lines at phase changes
# ----------------------------------------------------------

geom_vline(
  data = phase_lines,
  aes(
    xintercept = end_trial
  ),
  inherit.aes = FALSE,
  linetype = "dashed",
  color = "black",
  linewidth = 0.5
) +
  
  # ----------------------------------------------------------
# Horizontal no-error reference line
# ----------------------------------------------------------

geom_hline(
  yintercept = 0,
  linetype = "dashed",
  color = "black",
  linewidth = 0.6
) +
  
  # ----------------------------------------------------------
# Confidence-interval ribbons
# ----------------------------------------------------------

geom_ribbon(
  aes(
    ymin = CI_lo,
    ymax = CI_hi,
    fill = target_x_label,
    group = target_x_label
  ),
  alpha = 0.20,
  color = NA
) +
  
  # ----------------------------------------------------------
# Mean lines
# ----------------------------------------------------------

geom_line(
  linewidth = 0.55
) +
  
  # ----------------------------------------------------------
# Mean points
# ----------------------------------------------------------

geom_point(
  size = 0.5
) +
  
  # ----------------------------------------------------------
# Labels
# ----------------------------------------------------------

labs(
  title = "Mean Minimum Error to Target Across Trials",
  x = "Trials",
  y = "Min Error to Target",
  color = "Target ID",
  fill = "Target ID"
) +
  
  # ----------------------------------------------------------
# Y-axis
# ----------------------------------------------------------

scale_y_continuous(
  breaks = seq(
    -110,
    110,
    by = 20
  ),
  limits = c(
    -110,
    110
  ),
  expand = expansion(
    mult = c(0, 0)
  )
) +
  
  # ----------------------------------------------------------
# Target line colors
# ----------------------------------------------------------

scale_color_manual(
  name = "Target ID",
  values = target_line_colors,
  breaks = c(
    "L60",
    "L30",
    "R30",
    "R60"
  )
) +
  
  # ----------------------------------------------------------
# All fill colors
#
# Includes:
#   washout backgrounds
#   training Early/Late backgrounds
#   confidence-interval ribbons
# ----------------------------------------------------------

scale_fill_manual(
  name = "Target ID",
  values = c(
    # Washout blocks
    "shade_1" = "grey75",
    "shade_2" = "grey92",
    
    # Training 1
    "training1_early" = "#9ecae8",
    "training1_late" = "#f2cf70",
    
    # Training 2
    "training2_early" = "#7fb9df",
    "training2_late" = "#e6bd4d",
    
    # Target CI ribbons
    target_fill_colors
  ),
  
  # Only targets should appear in the legend
  breaks = c(
    "L60",
    "L30",
    "R30",
    "R60"
  )
) +
  
  # ----------------------------------------------------------
# Water-speed label on the right side
# ----------------------------------------------------------

facet_grid(
  rows = vars(speed_label),
  labeller = labeller(
    speed_label = c(
      "-3" = "Water Speed = 3",
      "-2" = "Water Speed = 2"
    )
  )
) +
  
  # ----------------------------------------------------------
# Theme
# ----------------------------------------------------------

theme_minimal() +
  
  theme(
    # Plot title
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    ),
    
    # Legend
    legend.title = element_text(
      face = "bold"
    ),
    
    # Axis titles
    axis.title.x = element_text(
      face = "bold"
    ),
    
    axis.title.y = element_text(
      face = "bold"
    ),
    
    # Water-speed facet label
    strip.text.y = element_text(
      face = "bold"
    ),
    
    # Remove vertical grid lines
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    
    # Keep only major horizontal grid lines
    panel.grid.major.y = element_line(
      color = "lightgrey"
    ),
    
    panel.grid.minor.y = element_blank()
  )


# ------------------------------------------------------------
# 12. Display the graph
# ------------------------------------------------------------

print(line_plot)


# ------------------------------------------------------------
# 13. Save the graph
# ------------------------------------------------------------

ggsave(
  filename = "test_line.png",
  plot = line_plot,
  width = 9,
  height = 5,
  dpi = 300
)










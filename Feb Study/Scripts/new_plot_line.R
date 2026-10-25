# ============================================================
# SINGLE-TARGET WATER CURRENT EXPERIMENT
# MEAN MINIMUM ERROR ACROSS ALL TRIALS
# ============================================================

library(ggplot2)
library(dplyr)

source("function_call.R")


# ============================================================
# 1. LOAD DATA
# ============================================================

data <- read.csv("Feb Study/Data/new_data.csv")


# ============================================================
# 2. SELECT 3 m/s CONDITION
# ============================================================

chosen_speed <- -3

plot_data <- data %>%
  filter(speed_label == chosen_speed)


# ============================================================
# 3. CHECK PHASE / TARGET STRUCTURE
# ============================================================

print(table(plot_data$phase))

print(table(plot_data$target_x_label))

print(table(plot_data$set_order))

# Data used for the target lines and confidence intervals
line_data <- plot_data %>%
  filter(target_x_label %in% c("L60", "R60"))

# ============================================================
# 4. CALCULATE MEAN ERROR FOR EACH TRIAL
# ============================================================

summary_data <- line_data %>%
  
  group_by(
    speed_label,
    trial_num,
    target_x_label
  ) %>%
  
  summarise(
    
    Mean = mean(
      flip_min_distance_xPCA_mean_bc,
      na.rm = TRUE
    ),
    
    SD = sd(
      flip_min_distance_xPCA_mean_bc,
      na.rm = TRUE
    ),
    
    N = sum(
      !is.na(
        flip_min_distance_xPCA_mean_bc
      )
    ),
    
    SEM = SD / sqrt(N),
    
    CI_lo = Mean - 1.96 * SEM,
    
    CI_hi = Mean + 1.96 * SEM,
    
    .groups = "drop"
  )


# ============================================================
# 5. TARGET ORDER
# ============================================================

summary_data <- summary_data %>%
  
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


# ============================================================
# 6. BASELINE BACKGROUND
# ============================================================

baseline_block <- plot_data %>%
  
  filter(
    phase == "baseline"
  ) %>%
  
  summarise(
    
    xmin = min(
      trial_num,
      na.rm = TRUE
    ) - 0.5,
    
    xmax = max(
      trial_num,
      na.rm = TRUE
    ) + 0.5
  )


# ============================================================
# 7. TRAINING PHASE 1
# ============================================================

training1_block <- plot_data %>%
  
  filter(
    phase == "training_1"
  ) %>%
  
  summarise(
    
    xmin = min(
      trial_num,
      na.rm = TRUE
    ) - 0.5,
    
    xmax = max(
      trial_num,
      na.rm = TRUE
    ) + 0.5
  )


# ============================================================
# 8. TRAINING PHASE 2
# ============================================================

training2_block <- plot_data %>%
  
  filter(
    phase == "training_2"
  ) %>%
  
  summarise(
    
    xmin = min(
      trial_num,
      na.rm = TRUE
    ) - 0.5,
    
    xmax = max(
      trial_num,
      na.rm = TRUE
    ) + 0.5
  )


# ============================================================
# 9. WASHOUT PHASES
# ============================================================

washout_data <- plot_data %>%
  
  filter(
    phase %in% c(
      "washout_ST",
      "washout_1_AT",
      "washout_2_AT"
    )
  )


washout_blocks <- washout_data %>%
  
  group_by(
    phase
  ) %>%
  
  summarise(
    
    xmin = min(
      trial_num,
      na.rm = TRUE
    ) - 0.5,
    
    xmax = max(
      trial_num,
      na.rm = TRUE
    ) + 0.5,
    
    .groups = "drop"
  )


# ============================================================
# 10. TARGET COLORS
# ============================================================

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


# ============================================================
# 11. CREATE GRAPH
# ============================================================

line_plot <- ggplot(
  
  summary_data,
  
  aes(
    x = trial_num,
    y = Mean,
    color = target_x_label
  )
) +
  
  
  # ----------------------------------------------------------
# BASELINE
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
# TRAINING 1 BACKGROUND
# ----------------------------------------------------------

geom_rect(
  
  data = training1_block,
  
  aes(
    xmin = xmin,
    xmax = xmax,
    ymin = -Inf,
    ymax = Inf
  ),
  
  inherit.aes = FALSE,
  
  fill = "white",
  
  alpha = 0.10,
  
  color = NA
) +
  
  
  # ----------------------------------------------------------
# TRAINING 2 BACKGROUND
# ----------------------------------------------------------

geom_rect(
  
  data = training2_block,
  
  aes(
    xmin = xmin,
    xmax = xmax,
    ymin = -Inf,
    ymax = Inf
  ),
  
  inherit.aes = FALSE,
  
  fill = "white",
  
  alpha = 0.10,
  
  color = NA
) +
  
  
  # ----------------------------------------------------------
# WASHOUT BACKGROUND
# ----------------------------------------------------------

geom_rect(
  
  data = washout_blocks,
  
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
# ZERO ERROR REGION
# ----------------------------------------------------------

geom_rect(
  
  aes(
    xmin = -Inf,
    xmax = Inf,
    ymin = -5,
    ymax = 5
  ),
  
  inherit.aes = FALSE,
  
  fill = "cyan",
  
  alpha = 0.25,
  
  color = NA
) +
  
  
  # ----------------------------------------------------------
# ZERO ERROR LINE
# ----------------------------------------------------------

geom_hline(
  
  yintercept = 0,
  
  linetype = "dashed",
  
  color = "black",
  
  linewidth = 0.6
) +
  
  
  # ----------------------------------------------------------
# PHASE BOUNDARIES
# ----------------------------------------------------------

geom_vline(
  
  data = baseline_block,
  
  aes(
    xintercept = xmax
  ),
  
  inherit.aes = FALSE,
  
  linetype = "dashed",
  
  color = "black",
  
  linewidth = 0.5
) +
  
  
  geom_vline(
    
    data = training1_block,
    
    aes(
      xintercept = xmax
    ),
    
    inherit.aes = FALSE,
    
    linetype = "dashed",
    
    color = "black",
    
    linewidth = 0.5
  ) +
  
  
  geom_vline(
    
    data = training2_block,
    
    aes(
      xintercept = xmax
    ),
    
    inherit.aes = FALSE,
    
    linetype = "dashed",
    
    color = "black",
    
    linewidth = 0.5
  ) +
  
  
  # ----------------------------------------------------------
# CONFIDENCE INTERVAL
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
# MEAN LINE
# ----------------------------------------------------------

geom_line(
  
  linewidth = 0.8
) +
  
  
  # ----------------------------------------------------------
# MEAN POINTS
# ----------------------------------------------------------

geom_point(
  
  size = 0.5
) +
  
  
  # ----------------------------------------------------------
# COLORS
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
  
  
  scale_fill_manual(
    
    name = "Target ID",
    
    values = target_fill_colors,
    
    breaks = c(
      "L60",
      "L30",
      "R30",
      "R60"
    )
  ) +
  
  
  # ----------------------------------------------------------
# Y AXIS
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
# LABELS
# ----------------------------------------------------------

labs(
  
  title = "Mean Minimum Error to Target Across Trials",
  
  x = "Trials",
  
  y = "Min Error to Target (cm)"
) +
  
  
  # ----------------------------------------------------------
# THEME
# ----------------------------------------------------------

theme_minimal() +
  
  theme(
    
    plot.title = element_text(
      hjust = 0.5,
      face = "bold"
    ),
    
    axis.title.x = element_text(
      face = "bold"
    ),
    
    axis.title.y = element_text(
      face = "bold"
    ),
    
    legend.title = element_text(
      face = "bold"
    ),
    
    panel.grid.major.x = element_blank(),
    
    panel.grid.minor.x = element_blank(),
    
    panel.grid.major.y = element_line(
      color = "lightgrey"
    ),
    
    panel.grid.minor.y = element_blank()
  )


# ============================================================
# 12. SHOW GRAPH
# ============================================================

print(line_plot)


# ============================================================
# 13. SAVE GRAPH
# ============================================================

ggsave(
  
  filename = "single_target_overall_graph.png",
  
  plot = line_plot,
  
  width = 9,
  
  height = 5,
  
  dpi = 300
)

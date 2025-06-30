# Load required packages
library(readxl)
library(ggplot2)
library(dplyr)
library(car)
library(rstatix)
library(ggpubr)
library(rcompanion)
library(tidyverse)
library(FSA)
library(janitor)

# Define treatment order and colors (using RColorBrewer Set2 palette)
treatment_order <- c("MMS-2", "Sand", "MMS-2 + Mesorhizobium", "Sand + Mesorhizobium")
colors_set2 <- RColorBrewer::brewer.pal(4, "Set2")  # Get 4 colors from Set2 palette
treatment_colors <- setNames(colors_set2, treatment_order)

# 1. Prepare the data (long format)
growth_long <- PlantGrowthOverTime_Data |>
  pivot_longer(cols = `Day 13`:`Day 30`, 
               names_to = "Day", 
               values_to = "Growth") |>
  mutate(Day = as.numeric(str_remove(Day, "Day ")))


# 2. Create treatment blot of growth shoot over time with fixed Y-axis
y_min <- floor(min(growth_long$Growth, na.rm = TRUE) / 5) * 5
y_max <- ceiling(max(growth_long$Growth, na.rm = TRUE) / 5) * 5

create_treatment_plot <- function(treatment_name) {
  treatment_data <- filter(growth_long, Treatment == treatment_name)
  mean_data <- treatment_data |>
    group_by(Day) |>
    summarise(Mean_Growth = mean(Growth, na.rm = TRUE))
  
  ggplot(treatment_data, aes(x = Day, y = Growth)) +
    geom_line(aes(group = Replicate), 
              color = "grey60", 
              alpha = 0.6, 
              linewidth = 0.7) +
    geom_line(data = mean_data, 
              aes(y = Mean_Growth), 
              color = "red", 
              linewidth = 1.5) +
    labs(title = treatment_name,
         x = "Days",
         y = "Growth (mm)") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_x_continuous(breaks = unique(growth_long$Day)) +
    scale_y_continuous(limits = c(y_min, y_max),  # Fixed Y-axis range
                       breaks = seq(y_min, y_max, by = 5))  # Clean breaks every 5mm
}

# 3. Generate plots for each treatment
treatment_plots <- lapply(unique(growth_long$Treatment), create_treatment_plot)

# 4. Arrange the 4 treatment plots in a grid
grid.arrange(grobs = treatment_plots, ncol = 2)

# 5. Create comparison plot with just means
mean_growth <- growth_long |>
  group_by(Treatment, Day) |>
  summarise(Mean_Growth = mean(Growth, na.rm = TRUE))

ggplot(mean_growth, aes(x = Day, y = Mean_Growth, color = Treatment)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  scale_color_manual(values = treatment_colors)  +
  labs(title = "Mean Growth Across Treatments",
       x = "Days",
       y = "Growth (mm)",
       color = "Treatment") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom") +
  scale_x_continuous(breaks = unique(growth_long$Day))


#Loading neccessary packages
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

# Import the Excel file and create new dataframe
mars_data <- as.data.frame(PlantTreatment_Data)

# Define treatment order and colors 
treatment_order <- c("MMS-2", "Sand", "MMS-2 + Mesorhizobium", "Sand + Mesorhizobium")
colors_set2 <- RColorBrewer::brewer.pal(4, "Set2") 
treatment_colors <- setNames(colors_set2, treatment_order)

# Boxplot root fresh weight
ggplot(mars_data, aes(x = Treatment, y = `Root fresh weight (mg)`, fill = Treatment)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "white") +
  labs(
    title = "Root fresh weight Comparison Across Treatments",
    x = "Treatment Group",
    y = "Root fresh weight (mg)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = treatment_colors) +
  geom_jitter(width = 0.1, alpha = 0.6) 

# boxplot shoot length
ggplot(mars_data, aes(x = Treatment, y = `Shoot length (mm)`, fill = Treatment)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "white") +
  labs(
    title = "Shoot Length Comparison Across Treatments",
    x = "Treatment Group",
    y = "Shoot Length (mm)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = treatment_colors) +
  geom_jitter(width = 0.1, alpha = 0.6)  

#boxplot root length
ggplot(mars_data, aes(x = Treatment, y = `Root length (mm)`, fill = Treatment)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "white") +
  labs(
    title = "Root Length Comparison Across Treatments",
    x = "Treatment Group",
    y = "Root Length (mm)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = treatment_colors) +
  geom_jitter(width = 0.1, alpha = 0.6) 

#Boxplot shoot dry weight
ggplot(mars_data, aes(x = Treatment, y = `Shoot dry weight (mg)`, fill = Treatment)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "white") +
  labs(
    title = "Shoot dry weight (mg) Comparison Across Treatments",
    x = "Treatment Group",
    y = "Shoot dry weight (mg)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = treatment_colors) +
  geom_jitter(width = 0.1, alpha = 0.6) 

#Boxplot root dry weight
ggplot(mars_data, aes(x = Treatment, y = `Root dry weight (mg)`, fill = Treatment)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "white") +
  labs(
    title = "root dry weight (mg) Comparison Across Treatments",
    x = "Treatment Group",
    y = "root dry weight (mg)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = treatment_colors) +
  geom_jitter(width = 0.1, alpha = 0.6)  

#Boxplot total dry weight
ggplot(mars_data, aes(x = Treatment, y = `Total dry weight (mg)`, fill = Treatment)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "white") +
  labs(
    title = "Total Dry Weight (mg) Comparison Across Treatments",
    x = "Treatment Group",
    y = "Total dry weight (mg)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = treatment_colors) +
  geom_jitter(width = 0.1, alpha = 0.6)

#Boxplot mean lateral root length
ggplot(mars_data, aes(x = Treatment, y = `Mean lateral root length (mm)`, fill = Treatment)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "white") +
  labs(
    title = "Mean lateral root length (mm) Comparison Across Treatments",
    x = "Treatment Group",
    y = "Mean lateral root length (mm)"
  ) +
  theme_minimal() +
  scale_fill_manual(values = treatment_colors) +
  geom_jitter(width = 0.1, alpha = 0.6)

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
library(PMCMRplus)  # For Games-Howell
library(effectsize) # For effect sizes

Mars_data = PlantTreatment_Data
# 1. Create soil_type and inoculation variables
# Prepare data
Mars_data <- Mars_data %>%
  mutate(
    soil_type = ifelse(str_detect(Treatment, "MMS-2"), "MMS-2", "Sand"),
    inoculation = ifelse(str_detect(Treatment, "Mesorhizobium"), "Inoculated", "Control"),
    soil_inoc = paste(soil_type, inoculation, sep = "_")
  )

# 1. Root Fresh Weight Analysis ------------------------------------------
# Check assumptions for root fresh weight
shapiro.test(Mars_data$`Root fresh weight (mg)`[Mars_data$soil_type == "MMS-2"])
shapiro.test(Mars_data$`Root fresh weight (mg)`[Mars_data$soil_type == "Sand"])
leveneTest(`Root fresh weight (mg)` ~ soil_type, data = Mars_data)

# Choose test based on assumptions
if(any(shapiro.test(Mars_data$`Root fresh weight (mg)`[Mars_data$soil_type == "MMS-2"])$p.value < 0.05,
       shapiro.test(Mars_data$`Root fresh weight (mg)`[Mars_data$soil_type == "Sand"])$p.value < 0.05) |
   leveneTest(`Root fresh weight (mg)` ~ soil_type, data = Mars_data)$`Pr(>F)`[1] < 0.05) {
  # Non-parametric
  cat("\nUsing Kruskal-Wallis test\n")
  print(kruskal.test(`Root fresh weight (mg)` ~ soil_type, data = Mars_data))
  cat("\nDunn's post-hoc test:\n")
  print(dunn_test(`Root fresh weight (mg)` ~ soil_type, data = Mars_data, p.adjust.method = "bonferroni"))
} else {
  # Parametric
  cat("\nUsing ANOVA\n")
  anova_res <- aov(`Root fresh weight (mg)` ~ soil_type, data = Mars_data)
  print(summary(anova_res))
  cat("\nTukey post-hoc:\n")
  print(TukeyHSD(anova_res))
}

# 2. Root Dry Weight Analysis --------------------------------------------
# Check assumptions
shapiro.test(Mars_data$`Root dry weight (mg)`[Mars_data$soil_type == "MMS-2"])
shapiro.test(Mars_data$`Root dry weight (mg)`[Mars_data$soil_type == "Sand"])
leveneTest(`Root dry weight (mg)` ~ soil_type, data = Mars_data)

# Choose test
if(any(shapiro.test(Mars_data$`Root dry weight (mg)`[Mars_data$soil_type == "MMS-2"])$p.value < 0.05,
       shapiro.test(Mars_data$`Root dry weight (mg)`[Mars_data$soil_type == "Sand"])$p.value < 0.05) |
   leveneTest(`Root dry weight (mg)` ~ soil_type, data = Mars_data)$`Pr(>F)`[1] < 0.05) {
  cat("\nUsing Kruskal-Wallis test\n")
  print(kruskal.test(`Root dry weight (mg)` ~ soil_type, data = Mars_data))
  cat("\nDunn's post-hoc test:\n")
  print(dunn_test(`Root dry weight (mg)` ~ soil_type, data = Mars_data, p.adjust.method = "bonferroni"))
} else {
  cat("\nUsing ANOVA\n")
  anova_res <- aov(`Root dry weight (mg)` ~ soil_type, data = Mars_data)
  print(summary(anova_res))
  print(TukeyHSD(anova_res))
}

#Total dry weight
# 3. Choose test based on assumptions
shapiro.test(Mars_data$`Total dry weight (mg)`[Mars_data$soil_type == "MMS-2"])
shapiro.test(Mars_data$`Total dry weight (mg)`[Mars_data$soil_type == "Sand"])
leveneTest(`Total dry weight (mg)` ~ soil_type, data = Mars_data)

# Choose test
if(any(shapiro.test(Mars_data$`Total dry weight (mg)`[Mars_data$soil_type == "MMS-2"])$p.value < 0.05,
       shapiro.test(Mars_data$`Total dry weight (mg)`[Mars_data$soil_type == "Sand"])$p.value < 0.05) |
   leveneTest(`Total dry weight (mg)` ~ soil_type, data = Mars_data)$`Pr(>F)`[1] < 0.05) {
  cat("\nUsing Kruskal-Wallis test\n")
  print(kruskal.test(`Total dry weight (mg)` ~ soil_type, data = Mars_data))
  cat("\nDunn's post-hoc test:\n")
  print(dunn_test(`Total dry weight (mg)` ~ soil_type, data = Mars_data, p.adjust.method = "bonferroni"))
} else {
  cat("\nUsing ANOVA\n")
  anova_res <- aov(`Total dry weight (mg)` ~ soil_type, data = Mars_data)
  print(summary(anova_res))
  print(TukeyHSD(anova_res))
}


# 5. Inoculation Effects -------------------------------------------------
# MMS-2 inoculation effects
mms_data <- Mars_data %>% filter(soil_type == "MMS-2")

# Root fresh weight
cat("\nMMS-2 Root Fresh Weight Inoculation Effect:\n")
shapiro.test(mms_data$`Root fresh weight (mg)`[mms_data$inoculation == "Control"])
shapiro.test(mms_data$`Root fresh weight (mg)`[mms_data$inoculation == "Inoculated"])
leveneTest(`Root fresh weight (mg)` ~ inoculation, data = mms_data)
t.test(`Root fresh weight (mg)` ~ inoculation, data = mms_data, var.equal = TRUE)

# Root dry weight
cat("\nMMS-2 Root Dry Weight Inoculation Effect:\n")
shapiro.test(mms_data$`Root dry weight (mg)`[mms_data$inoculation == "Control"])
shapiro.test(mms_data$`Root dry weight (mg)`[mms_data$inoculation == "Inoculated"])
leveneTest(`Root dry weight (mg)` ~ inoculation, data = mms_data)
t.test(`Root dry weight (mg)` ~ inoculation, data = mms_data, var.equal = TRUE)

# Total dry weight
cat("\nMMS-2 Root Dry Weight Inoculation Effect:\n")
shapiro.test(mms_data$`Total dry weight (mg)`[mms_data$inoculation == "Control"])
shapiro.test(mms_data$`Total dry weight (mg)`[mms_data$inoculation == "Inoculated"])
leveneTest(`Total dry weight (mg)` ~ inoculation, data = mms_data)
t.test(`Total dry weight (mg)` ~ inoculation, data = mms_data, var.equal = TRUE)

# Sand inoculation effects
sand_data <- Mars_data %>% filter(soil_type == "Sand")

# Root fresh weight
cat("\nSand Root Fresh Weight Inoculation Effect:\n")
shapiro.test(sand_data$`Root fresh weight (mg)`[sand_data$inoculation == "Control"])
shapiro.test(sand_data$`Root fresh weight (mg)`[sand_data$inoculation == "Inoculated"])
leveneTest(`Root fresh weight (mg)` ~ inoculation, data = sand_data)
t.test(`Root fresh weight (mg)` ~ inoculation, data = sand_data, var.equal = TRUE)

# Root dry weight
cat("\nSand Root Dry Weight Inoculation Effect:\n")
shapiro.test(mms_data$`Root dry weight (mg)`[mms_data$inoculation == "Control"])
shapiro.test(mms_data$`Root dry weight (mg)`[mms_data$inoculation == "Inoculated"])
leveneTest(`Root dry weight (mg)` ~ inoculation, data = mms_data)
t.test(`Root dry weight (mg)` ~ inoculation, data = mms_data, var.equal = TRUE)

# Total dry weight
cat("\nSand Root Dry Weight Inoculation Effect:\n")
shapiro.test(mms_data$`Total dry weight (mg)`[mms_data$inoculation == "Control"])
shapiro.test(mms_data$`Total dry weight (mg)`[mms_data$inoculation == "Inoculated"])
leveneTest(`Total dry weight (mg)` ~ inoculation, data = mms_data)
t.test(`Total dry weight (mg)` ~ inoculation, data = mms_data, var.equal = TRUE)

# 6. Summary Statistics --------------------------------------------------
weight_stats <- Mars_data %>%
  group_by(soil_type, inoculation) %>%
  summarise(
    `Root fresh (mg)` = sprintf("%.1f ± %.1f", mean(`Root fresh weight (mg)`), sd(`Root fresh weight (mg)`)),
    `Root dry (mg)` = sprintf("%.1f ± %.1f", mean(`Root dry weight (mg)`), sd(`Root dry weight (mg)`)),
    `Shoot fresh (mg)` = sprintf("%.1f ± %.1f", mean(`Shoot fresh weight (mg)`), sd(`Shoot fresh weight (mg)`)),
    `Shoot dry (mg)` = sprintf("%.1f ± %.1f", mean(`Shoot dry weight (mg)`), sd(`Shoot dry weight (mg)`)),
    `Total dry weight (mg)` = sprintf("%.1f ± %.1f", mean(`Total dry weight (mg)`), sd(`Total dry weight (mg)`)),
    .groups = 'drop'
  )


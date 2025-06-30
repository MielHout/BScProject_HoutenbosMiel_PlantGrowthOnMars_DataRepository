library(tidyverse)
library(car)       # For Levene's test
library(multcomp)  # For Tukey HSD
library(ggpubr)    # For visualization
library(rstatix)   # For Dunn test
library(readxl)
library(ggplot2)
library(dplyr)
library(rcompanion)
library(FSA)
library(janitor)


Mars_data = PlantTreatment_Data
# 1. Create soil_type variable
Mars_data <- Mars_data %>%
  mutate(soil_type = ifelse(str_detect(Treatment, "MMS-2"), "MMS-2", "Sand"))

# 1. Primary Root Length Analysis ------------------------------------------
# Check assumptions for primary root length
shapiro.test(Mars_data$`Root length (mm)`[Mars_data$soil_type == "MMS-2"]) #not normal p-value = 0.02827
shapiro.test(Mars_data$`Root length (mm)`[Mars_data$soil_type == "Sand"])
leveneTest(`Root length (mm)` ~ soil_type, data = Mars_data) #unequal variance p-value = 0.02109

#Kruskal-Wallis test (non-parametric alternative to ANOVA)
kruskal_result <- kruskal.test(`Root length (mm)` ~ soil_type, data = Mars_data)
print(kruskal_result)

#Dunn's post-hoc test with Bonferroni correction
dunn_result <- dunn_test(`Root length (mm)` ~ soil_type, 
                         data = Mars_data,
                         p.adjust.method = "bonferroni") #Small number of comparisons
print(dunn_result)

# 2. Lateral Root Analysis ------------------------------------------------
# Check assumptions for lateral roots
shapiro.test(Mars_data$`Number of lateral roots`[Mars_data$soil_type == "MMS-2"])
shapiro.test(Mars_data$`Number of lateral roots`[Mars_data$soil_type == "Sand"])
leveneTest(`Number of lateral roots` ~ soil_type, data = Mars_data)

# Kruskal-Wallis if non-normal, ANOVA if normal
if(any(shapiro.test(Mars_data$`Number of lateral roots`[Mars_data$soil_type == "MMS-2"])$p.value < 0.05,
       shapiro.test(Mars_data$`Number of lateral roots`[Mars_data$soil_type == "Sand"])$p.value < 0.05)) {
  kruskal.test(`Number of lateral roots` ~ soil_type, data = Mars_data)
  dunnTest(`Number of lateral roots` ~ soil_type, data = Mars_data, method = "bonferroni")
} else {
  lateral_anova <- aov(`Number of lateral roots` ~ soil_type, data = Mars_data)
  summary(lateral_anova)
  TukeyHSD(lateral_anova)
}

# 3. Inoculation Effects --------------------------------------------------
# Create inoculation variable
Mars_data <- Mars_data %>%
  mutate(
    inoculation = ifelse(str_detect(Treatment, "Mesorhizobium"), "Inoculated", "Control"),
    soil_inoc = paste(soil_type, inoculation, sep = "_")  # Combined variable
  )

# MMS-2 inoculation effect on root length
mms_root <- Mars_data %>% filter(soil_type == "MMS-2")
t.test(`Root length (mm)` ~ inoculation, data = mms_root, var.equal = TRUE)

# Sand inoculation effect on root length
sand_root <- Mars_data %>% filter(soil_type == "Sand")
t.test(`Root length (mm)` ~ inoculation, data = sand_root, var.equal = TRUE)

# MMS-2 inoculation effect on lateral roots
mms_root <- Mars_data %>% filter(soil_type == "MMS-2")
t.test(`Number of lateral roots` ~ inoculation, data = mms_root, var.equal = TRUE)

# Sand inoculation effect on lateral roots
sand_root <- Mars_data %>% filter(soil_type == "Sand")
t.test(`Number of lateral roots` ~ inoculation, data = sand_root, var.equal = TRUE)

# 5. Generate Summary Tables ----------------------------------------------
root_stats <- Mars_data %>%
  group_by(soil_type) %>%
  summarise(
    `Primary root length (mm)` = sprintf("%.1f ± %.1f", 
                                         mean(`Root length (mm)`, na.rm = TRUE), 
                                         sd(`Root length (mm)`, na.rm = TRUE)),
    `Lateral roots (count)` = sprintf("%.1f ± %.1f", 
                                      mean(`Number of lateral roots`, na.rm = TRUE), 
                                      sd(`Number of lateral roots`, na.rm = TRUE)),
    .groups = 'drop'
  )

inoc_stats <- Mars_data %>%
  group_by(soil_type, inoculation) %>%
  summarise(
    `Root length (mm)` = sprintf("%.1f ± %.1f", 
                                 mean(`Root length (mm)`, na.rm = TRUE), 
                                 sd(`Root length (mm)`, na.rm = TRUE)),
    .groups = 'drop'
  )

# Print tables
knitr::kable(root_stats, caption = "Root Growth by Soil Type")
knitr::kable(inoc_stats, caption = "Root Length by Inoculation Status")
knitr::kable(nodulation_counts, caption = "Nodulation Frequency")

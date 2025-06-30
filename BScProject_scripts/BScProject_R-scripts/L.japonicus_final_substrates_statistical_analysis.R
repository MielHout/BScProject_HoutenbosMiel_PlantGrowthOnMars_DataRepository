# Load packages
library(tidyverse)
library(ggpubr)
library(car)        # For Levene's test
library(PMCMRplus)  # For Games-Howell
library(effectsize) # For effect sizes

Mars_data = PlantTreatment_Data
# 1. Create soil_type variable
Mars_data <- Mars_data %>%
  mutate(soil_type = ifelse(str_detect(Treatment, "MMS-2"), "MMS-2", "Sand"))

# 2. Check normality by group---------------------------------------------------
shapiro_results <- Mars_data %>%
  group_by(soil_type) %>%
  summarise(
    p_value = shapiro.test(`Shoot length (mm)`)$p.value
  )
print(shapiro_results)

# 3. Check homogeneity of variance
levene_result <- leveneTest(`Shoot length (mm)` ~ soil_type, data = Mars_data)
print(levene_result)

# 4. Choose test based on assumptions-------------------------------------------
#Shoot length between soil type
if(levene_result$`Pr(>F)`[1] > 0.05) {
  # If variances equal
  cat("\nUsing standard ANOVA + Tukey HSD\n")
  anova_res <- aov(`Shoot length (mm)` ~ soil_type, data = Mars_data)
  print(summary(anova_res))
  print(TukeyHSD(anova_res))
} else {
  # If variances unequal (your case)
  cat("\nUsing Welch's ANOVA + Games-Howell (unequal variances)\n")
  welch_res <- oneway.test(`Shoot length (mm)` ~ soil_type, data = Mars_data)
  print(welch_res)
  print(gamesHowellTest(`Shoot length (mm)` ~ factor(soil_type), data = Mars_data))
}


#Same test between soil types (with and without Mesorhizobium)
#First MMS-2--------------------------------------------------------------------
# Create inoculation variable
Mars_data <- Mars_data %>%
  mutate(
    inoculation = ifelse(str_detect(Treatment, "Mesorhizobium"), "Inoculated", "Control"),
    soil_inoc = paste(soil_type, inoculation, sep = "_")  # Combined variable
  )
# Subset MMS-2 data only
mms_data <- Mars_data %>% filter(soil_type == "MMS-2")

# Check assumptions
shapiro.test(mms_data$`Shoot length (mm)`[mms_data$inoculation == "Control"])
shapiro.test(mms_data$`Shoot length (mm)`[mms_data$inoculation == "Inoculated"])
leveneTest(`Shoot length (mm)` ~ inoculation, data = mms_data)

# Welch's t-test (recommended for small samples)
mms_test <- t.test(`Shoot length (mm)` ~ inoculation, 
                   data = mms_data,
                   var.equal = TRUE) #TRUE when met, FALSE is Welch's correction
print(mms_test)

#second sand--------------------------------------------------------------------
# Subset sand data only
sand_data <- Mars_data %>% filter(soil_type == "Sand")

# Check assumptions
shapiro.test(sand_data$`Shoot length (mm)`[sand_data$inoculation == "Control"]) #not normal
shapiro.test(sand_data$`Shoot length (mm)`[sand_data$inoculation == "Inoculated"])
leveneTest(`Shoot length (mm)` ~ inoculation, data = sand_data)

# Welch's t-test
wilcox_test <- wilcox.test(`Shoot length (mm)` ~ inoculation, 
                           data = sand_data,
                           exact = FALSE,  # For small sample approximation
                           conf.int = TRUE)
print(wilcox_test)


#Calculate plant growth statistics----------------------------------------------
growth_stats <- Mars_data %>%
  group_by(soil_type, inoculation) %>%
  summarise(
    Mean = mean(`Shoot length (mm)`, na.rm = TRUE),
    SD = sd(`Shoot length (mm)`, na.rm = TRUE),
    Median = median(`Shoot length (mm)`, na.rm = TRUE),
    Min = min(`Shoot length (mm)`, na.rm = TRUE),
    Max = max(`Shoot length (mm)`, na.rm = TRUE),
    N = n(),
    .groups = 'drop'
  ) %>%
  mutate(
    `Mean ± SD` = sprintf("%.1f ± %.1f", Mean, SD)
  )

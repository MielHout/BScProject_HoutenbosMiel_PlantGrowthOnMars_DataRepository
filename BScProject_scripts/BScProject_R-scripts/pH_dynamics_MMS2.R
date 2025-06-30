library(ggplot2)

#Get Excel file "pH_Treatment_Data", sheet "titration_curve"
titration_data = pHTreatment_Data

ggplot(titration_data, aes(x = volume_ul, y = pH)) +
  geom_point(size = 3, color = "blue") +
  geom_line(color = "red") +
  labs(title = "Titration Curve",
       x = "Volume HCl (uL)",
       y = "pH") +
  theme_minimal()

#MMS-2 pH lowering
#Get Excel file "pH_Treatment_Data", sheet "MMS2_titration"
mms2_titration = pH_Treatment_Data
mms2_titration$Timepoint <- factor(
  mms2_titration$Timepoint,
  levels = c("Baseline", "1 mL HCl", "5 mL HCl", "1 h venting", "24 h venting")
)

timepoint_colors <- c(
  "Baseline" = "#F8766D", 
  "1 mL HCl" = "#7CAE00", 
  "5 mL HCl" = "#00BFC4", 
  "1 h venting" = "#C77CFF",
  "24 h venting" = "#FF61C3"
)

ggplot(mms2_titration, aes(x = HCl_added_ml, y = pH, color = Timepoint)) +
  geom_point(size = 4) +
  geom_line(data = mms2_titration[1:3, ], aes(x = HCl_added_ml, y = pH), 
            inherit.aes = FALSE, color = "black", size = 0.2) +
  geom_segment(aes(x = 5, y = 5.85, xend = 5, yend = 7.30),
               linetype = "dotted", color = "black", size = 0.8) +
  geom_segment(aes(x = 5, y = 7.30, xend = 5, yend = 7.85),
               linetype = "dotted", color = "black", size = 0.8) +
  annotate("text", x = 5.3, y = (5.85 + 7.30)/2, 
           label = "pH after 1 hour", hjust = 0, size = 4) +
  annotate("text", x = 5.3, y = (7.30 + 7.85)/2, 
           label = "pH after 24 hours", hjust = 0, size = 4) +
  labs(title = "MMS-2 pH Dynamics With HCl Addition",
       x = "2 M HCl added (mL)", y = "pH") +
  theme_bw() +
  xlim(0, 6) +
  scale_color_manual(values = timepoint_colors)


#MES buffer dynamics
#Get Excel file "pH_Treatment_Data", sheet "MES_Buffer"
mes_data = pH_Treatment_Data

ggplot(mes_data, aes(x = Time, y = pH, color = Concentration)) +
  geom_point(size = 4) +
  geom_line(size = 1) +
  labs(title = "pH Change Over Time for MES pH 5.6 Buffers",
       x = "Time (hours)",
       y = "pH") +
  theme_bw()

This readme.txt file was generated on 2025-06-30 by Miel Houtenbos

GENERAL INFORMATION

1. Title of the report: Growth and Nitrogen Fixation In Lotus Japonicus Under Simulated Martian Soil- and Atmospheric Conditions

2. Author Information
	A. Principal Investigator Contact Information
		Name: Miel Houtenbos
		Institution: Institute for Biodiversity and Ecosystem Dynamics
		Address: X
		Email: miel.houttenbos@student.uva.nl

	B. Supervisor Contact Information
		Name: Dr Thomas Blankers
		Institution: Institute for Biodiversity and Ecosystem Dynamics
		Address: X
		Email: t.blankers@uva.nl

	
3. Date of data collection 2025-03-01 -- 2025-06-01

DATA & FILE OVERVIEW

1. File List: 

- Readme.txt (this file)
- BScProject_R-scripts
	The R scripts used to analyse the field data and generate figures and tables


METHODOLOGICAL INFORMATION
See report


DATA-SPECIFIC INFORMATION FOR: [BScProject_R-scripts]

1. Variable List: 
	Statistical analysis was performed based on assumptions measured with testing. All 	reasoning has been provided with statistical analysis or included within code 	string
	Example not included (Visual analysis performed based on values ):
		shapiro.test(Mars_data$`Root fresh weight (mg)`[Mars_data$soil_type == 			"MMS-2"])
		shapiro.test(Mars_data$`Root fresh weight (mg)`[Mars_data$soil_type == 			"Sand"])
		leveneTest(`Root fresh weight (mg)` ~ soil_type, data = Mars_data)
	Example included:
		if(any(shapiro.test(Mars_data$`Root fresh weight (mg)`[Mars_data$soil_type 		== "MMS-2"])$p.value < 0.05,
     		  shapiro.test(Mars_data$`Root fresh weight (mg)`[Mars_data$soil_type == 		"Sand"])$p.value < 0.05) |
  		 leveneTest(`Root fresh weight (mg)` ~ soil_type, data = Mars_data)$`Pr(>		F)`[1] < 0.05) {
 		 # Non-parametric
 		 cat("\nUsing Kruskal-Wallis test\n")
 		 print(kruskal.test(`Root fresh weight (mg)` ~ soil_type, data = 			Mars_data))
 		 cat("\nDunn's post-hoc test:\n")
 		 print(dunn_test(`Root fresh weight (mg)` ~ soil_type, data = Mars_data, 		p.adjust.method = "bonferroni"))
		} else {
 		 # Parametric
  		cat("\nUsing ANOVA\n")
 		 anova_res <- aov(`Root fresh weight (mg)` ~ soil_type, data = Mars_data)
  		print(summary(anova_res))
  		cat("\nTukey post-hoc:\n")
  		print(TukeyHSD(anova_res))


2. Missing data codes: 
No missing data


3. Specialized formats or other abbreviations used: 
Special code used from packages. All packages needed and more are present in first strings in code under (library)
All additional information is present in brackets within code (#)


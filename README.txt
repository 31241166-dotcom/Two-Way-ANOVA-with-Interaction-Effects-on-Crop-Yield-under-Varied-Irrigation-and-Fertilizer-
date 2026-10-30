Crop Yield Advanced Shiny Dashboard

Files:
- app.R
- crop_yield_two_way_anova_dataset.xlsx

Run in VS Code / R:
1. Put both files in the same folder.
2. Open that folder in VS Code.
3. In the R terminal run:
   setwd("YOUR_FOLDER_PATH")
   install.packages(c("shiny","readxl","DT","ggplot2"))
   shiny::runApp()

The dashboard includes:
- Colored KPI cards
- Interactive/filterable dataset
- Download filtered CSV
- Mean-yield comparison chart
- Yield distribution boxplot
- Irrigation x nitrogen interaction graph
- ANOVA results
- Treatment comparison
- Summary and key findings

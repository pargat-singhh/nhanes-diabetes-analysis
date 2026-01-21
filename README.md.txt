# NHANES Diabetes & Lifestyle Analysis (2021–2023)

This project analyzes the association between lifestyle factors and
diabetes status using data from the National Health and Nutrition
Examination Survey (NHANES) 2021–2023.

## Data Source
NHANES public-use datasets provided by the CDC:
https://wwwn.cdc.gov/nchs/nhanes/

## Files Included
- `analysis.R` – Main data cleaning, analysis, and visualization script
- `report/` – Final project report (PDF)
- `output/` – Generated figures and plots
- `renv.lock` – Package version lock file for reproducibility

## Variables Studied
- Age
- Gender
- Race/Ethnicity
- BMI
- Fasting Plasma Glucose
- HbA1c
- Sedentary Behavior
- Smoking Status
- Carbohydrate Intake

## Methods
- Data merging and cleaning using `dplyr`
- Wilcoxon rank-sum tests for numeric variables
- Chi-square tests for categorical variables
- Kruskal-Wallis and Dunn’s post-hoc tests
- Visualization using `ggplot2`

## Reproducibility
This project uses `renv` to manage package versions.
To reproduce the environment:

```r
renv::restore()

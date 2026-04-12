# Examining Lifestyle Factors and Metabolic Health Markers Associated with Diabetes Using NHANES Data (2021–2023)

Data sourced from [NHANES 2021–2023](https://wwwn.cdc.gov/nchs/nhanes/) public-use datasets provided by the [CDC](https://www.cdc.gov/).

## Variables

Age, Gender, Race/Ethnicity, BMI, Fasting Plasma Glucose, HbA1c, Sedentary Behavior, Smoking Status, Carbohydrate Intake

## Methods

- Wilcoxon rank-sum tests (numeric variables by diabetes status)
- Chi-square tests (Gender, Smoking, Race × Diabetes)
- Pairwise proportion tests with BH correction (Race post-hoc)
- Kruskal-Wallis tests (HbA1c, Glucose, BMI across Race)
- Logistic regression (predictors of diabetes)
- Correlation heatmap and boxplot visualizations

## Files

| File | Description |
|------|-------------|
| `analysis.R` | Data cleaning, statistical analysis, and visualization |
| `output/` | Generated figures |
| `renv.lock` | Package version lock for reproducibility |

## Reproducibility

```r
renv::restore()
```

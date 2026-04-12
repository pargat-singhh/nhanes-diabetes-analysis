############################################################
# NHANES (2021–2023) Diabetes & Lifestyle Analysis
# Data source: https://wwwn.cdc.gov/nchs/nhanes/
############################################################


############################
# 1. Load Required Packages
############################
library(dplyr)
library(tidyr)
library(ggplot2)
library(haven)
library(rstatix)
library(ggpubr)
library(ggcorrplot)


####################################
# 2. Load & Prepare Demographic Data
####################################
demo <- read_xpt("data/DEMO_L.XPT")

df_demo <- demo %>%
  select(SEQN, RIDAGEYR, RIAGENDR, RIDRETH3) %>%
  rename(
    Age    = RIDAGEYR,
    Gender = RIAGENDR,
    Race   = RIDRETH3
  ) %>%
  mutate(
    Gender = recode(Gender,
                    `1` = "Male",
                    `2` = "Female"),
    Race = recode(Race,
                  `1` = "Mexican American",
                  `2` = "Other Hispanic",
                  `3` = "Non-Hispanic White",
                  `4` = "Non-Hispanic Black",
                  `6` = "Non-Hispanic Asian",
                  `7` = "Other / Multiracial")
  )


#########################
# 3. Diabetes Status
#########################
diabetes <- read_xpt("data/DIQ_L.XPT") %>%
  select(SEQN, DIQ010) %>%
  rename(Have_Diabetes = DIQ010)


####################################
# 4. Laboratory Measurements
####################################
# Fasting Plasma Glucose
fasting_glucose <- read_xpt("data/GLU_L.XPT") %>%
  select(SEQN, LBXGLU) %>%
  rename(Fasting_Glucose = LBXGLU)

# HbA1c (Glycohemoglobin)
HbA1c <- read_xpt("data/GHB_L.XPT") %>%
  select(SEQN, LBXGH) %>%
  rename(HbA1c_level = LBXGH)


#########################
# 5. Lifestyle Factors
#########################
# Sedentary Activity (minutes/day)
physical_activity <- read_xpt("data/PAQ_L.XPT") %>%
  select(SEQN, PAD680) %>%
  rename(Sedentary_activity_mins = PAD680)

# Smoking Status
smoking <- read_xpt("data/SMQ_L.XPT") %>%
  select(SEQN, SMQ040) %>%
  rename(Current_Smoker = SMQ040) %>%
  mutate(
    Current_Smoker = recode(Current_Smoker,
                            `1` = "Every Day",
                            `2` = "Some Days",
                            `3` = "Not at All")
  )

# Body Mass Index (BMI)
BMI <- read_xpt("data/BMX_L.XPT") %>%
  select(SEQN, BMXBMI) %>%
  rename(BMI = BMXBMI)

# Dietary Carbohydrate Intake (Day 1)
diet <- read_xpt("data/DR1TOT_L.XPT") %>%
  select(SEQN, DR1TCARB) %>%
  rename(Carb_intake = DR1TCARB)


############################
# 6. Merge All Datasets
############################
merged_df <- df_demo %>%
  left_join(diabetes, by = "SEQN") %>%
  left_join(fasting_glucose, by = "SEQN") %>%
  left_join(HbA1c, by = "SEQN") %>%
  left_join(physical_activity, by = "SEQN") %>%
  left_join(smoking, by = "SEQN") %>%
  left_join(BMI, by = "SEQN") %>%
  left_join(diet, by = "SEQN")


############################
# 7. Data Cleaning
############################
final_clean <- merged_df %>%
  drop_na(Fasting_Glucose, HbA1c_level, Sedentary_activity_mins,
          Current_Smoker, BMI, Carb_intake) %>%
  filter(Sedentary_activity_mins != 9999,
         Have_Diabetes != 3) %>%
  mutate(
    diabetes = case_when(
      Have_Diabetes == 1 ~ "Diabetes",
      Have_Diabetes == 2 ~ "No_Diabetes"
    ),
    diabetes = factor(diabetes, levels = c("No_Diabetes", "Diabetes")),
    diabetes_num = ifelse(diabetes == "Diabetes", 1, 0),
    Gender = factor(Gender),
    Race = factor(Race),
    Current_Smoker = factor(Current_Smoker)
  ) %>%
  select(-Have_Diabetes)

str(final_clean)


########################################
# 8. Descriptive Statistics (EDA)
########################################
summary_table <- final_clean %>%
  group_by(diabetes) %>%
  summarise(
    across(
      c(Age, BMI, Fasting_Glucose, HbA1c_level,
        Sedentary_activity_mins, Carb_intake),
      list(mean = ~mean(.x, na.rm = TRUE),
           sd   = ~sd(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    ),
    .groups = "drop"
  )

print(summary_table)


########################################
# 9. Statistical Testing
########################################

# --- Wilcoxon Tests (H1, H3, H4) ---
num_vars <- c("Age", "BMI", "Fasting_Glucose",
              "HbA1c_level", "Sedentary_activity_mins", "Carb_intake")

stat_tests <- final_clean %>%
  pivot_longer(cols = all_of(num_vars),
               names_to = "variable",
               values_to = "value") %>%
  group_by(variable) %>%
  wilcox_test(value ~ diabetes)

print(stat_tests)

# --- Chi-square Tests (H2, H5) ---
cat("\n--- Chi-square: Gender x Diabetes ---\n")
chisq.test(table(final_clean$Gender, final_clean$diabetes))

cat("\n--- Chi-square: Smoking x Diabetes (H2) ---\n")
chisq.test(table(final_clean$Current_Smoker, final_clean$diabetes))

cat("\n--- Chi-square: Race x Diabetes (H5) ---\n")
chisq.test(table(final_clean$Race, final_clean$diabetes))

# --- Pairwise Chi-square for Race x Diabetes (H5 post-hoc) ---
cat("\n--- Pairwise comparisons: Race x Diabetes (H5 post-hoc) ---\n")
race_diabetes_table <- table(final_clean$Race, final_clean$diabetes)
pairwise_prop_test(race_diabetes_table, p.adjust.method = "BH") %>%
  print(n = Inf)


########################################
# 10. Racial Disparity Analysis (H6)
########################################

# --- Kruskal-Wallis Tests ---
cat("\n--- Kruskal-Wallis: HbA1c ~ Race ---\n")
kruskal.test(HbA1c_level ~ Race, data = final_clean)

cat("\n--- Kruskal-Wallis: Fasting Glucose ~ Race ---\n")
kruskal.test(Fasting_Glucose ~ Race, data = final_clean)

cat("\n--- Kruskal-Wallis: BMI ~ Race ---\n")
kruskal.test(BMI ~ Race, data = final_clean)




########################################
# 11. Logistic Regression (RQ7)
########################################
cat("\n--- Logistic Regression: Predictors of Diabetes ---\n")
logit_model <- glm(
  diabetes_num ~ Age + Gender + Race + BMI +
    Sedentary_activity_mins + Current_Smoker + Carb_intake,
  data = final_clean,
  family = binomial
)

summary(logit_model)


########################################
# 12. Visualizations
########################################

# Metabolic Indicators by Diabetes Status
p1 <- ggboxplot(final_clean, x="diabetes", y="BMI", color="diabetes", add="jitter")
p2 <- ggboxplot(final_clean, x="diabetes", y="Fasting_Glucose", color="diabetes", add="jitter")
p3 <- ggboxplot(final_clean, x="diabetes", y="HbA1c_level", color="diabetes", add="jitter")

ggsave("Metabolic_Indicators.png",
       ggarrange(p1, p2, p3, ncol=3),
       width = 12, height = 5, dpi = 300)

# Age & Sedentary Time
p4 <- ggboxplot(final_clean, x="diabetes", y="Age", color="diabetes", add="jitter")
p5 <- ggboxplot(final_clean, x="diabetes", y="Sedentary_activity_mins",
                color="diabetes", add="jitter")

ggsave("Metabolic_Indicators2.png",
       ggarrange(p4, p5, ncol=2),
       width = 8, height = 5, dpi = 300)

# Correlation Heatmap
corr_data <- final_clean %>%
  select(Age, BMI, Fasting_Glucose, HbA1c_level,
         Sedentary_activity_mins, Carb_intake)

ggsave("Correlation.png",
       ggcorrplot(cor(corr_data, use="complete.obs"),
                  lab = TRUE, type = "lower"),
       width = 6, height = 5, dpi = 300)

# HbA1c Distribution by Race
p_race <- ggboxplot(final_clean, x="Race", y="HbA1c_level",
                    fill="Race", add="jitter") +
  theme(axis.text.x = element_text(angle=45, hjust=1))

ggsave("HbA1cDistribution.png",
       p_race,
       width = 7, height = 5, dpi = 300)

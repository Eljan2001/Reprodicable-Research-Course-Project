# =========================================================
# DATA SCIENCE SALARY PROJECT — ECONOMETRICS PART
# PERSON 2 — REGRESSION & ROBUSTNESS
# =========================================================


install.packages("tidyverse")
install.packages("fixest")
install.packages("modelsummary")


# -----------------------------
# 1. LIBRARIES
# -----------------------------
library(tidyverse)
library(fixest)
library(modelsummary)


# -----------------------------
# 2. DATA
# -----------------------------
df <- read.csv("data_science_salaries.csv")
df
# -----------------------------
# 3. SELECT VARIABLES
# -----------------------------
df <- df %>%
  select(
    salary_in_usd,
    experience_level,
    employment_type,
    work_models,
    work_year,
    employee_residence,
    company_location,
    company_size,
    job_title
  )

# -----------------------------
# 4. CLEAN DATA
# -----------------------------

# Remove missing values
df <- na.omit(df)

# Keep only positive salaries
df <- df %>%
  filter(salary_in_usd > 0)

# Create log salary
df <- df %>%
  mutate(log_salary = log(salary_in_usd))

# Convert categorical variables to factors
df <- df %>%
  mutate(
    experience_level = as.factor(experience_level),
    employment_type = as.factor(employment_type),
    work_models = as.factor(work_models),
    company_size = as.factor(company_size),
    company_location = as.factor(company_location),
    employee_residence = as.factor(employee_residence),
    work_year = as.factor(work_year)
  )

# -----------------------------
# 5. MAIN REGRESSION MODEL
# -----------------------------

model1 <- feols(
  log_salary ~
    experience_level +
    work_models +
    company_size +
    work_year |
    company_location,
  
  data = df,
  
  vcov = "hetero"
)

summary(model1)

# -----------------------------
# 6. ROBUSTNESS CHECK 1
# ONLY FULL-TIME EMPLOYEES
# -----------------------------

df_fulltime <- df %>%
  filter(employment_type == "Full-time")

model2 <- feols(
  log_salary ~
    experience_level +
    work_models +
    company_size +
    work_year |
    company_location,
  
  data = df_fulltime,
  
  vcov = "hetero"
)

summary(model2)

# -----------------------------
# 7. ROBUSTNESS CHECK 2
# USE EMPLOYEE RESIDENCE
# INSTEAD OF COMPANY LOCATION
# -----------------------------

model3 <- feols(
  log_salary ~
    experience_level +
    work_models +
    company_size +
    work_year |
    employee_residence,
  
  data = df,
  
  vcov = "hetero"
)

summary(model3)

# -----------------------------
# 8. ROBUSTNESS CHECK 3
# REMOVE TOP 1% SALARIES
# -----------------------------

salary_cutoff <- quantile(df$salary_in_usd, 0.99)

df_trimmed <- df %>%
  filter(salary_in_usd < salary_cutoff)

model4 <- feols(
  log_salary ~
    experience_level +
    work_models +
    company_size +
    work_year |
    company_location,
  
  data = df_trimmed,
  
  vcov = "hetero"
)

summary(model4)

#“Does remote work affect senior workers differently?”

model5 <- feols(
  log_salary ~
    experience_level * work_models +
    company_size +
    work_year |
    company_location,
  
  data = df,
  
  vcov = "hetero"
)

summary(model5)
# -----------------------------
# 9. REGRESSION TABLE
# -----------------------------

modelsummary(
  list(
    "Main Model" = model1,
    "Full-Time Only" = model2,
    "Residence FE" = model3,
    "Trimmed Sample" = model4,
    "Remote Work " = model5,
  ),
  
  stars = TRUE,
  output = "markdown"
)



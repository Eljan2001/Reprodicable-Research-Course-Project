# Project - RR (5 Plots)

#Step1 : Load the dataset and dependencies
library(tidyverse)
library(here)
library(scales)

data <- read_csv("data_science_salaries.csv")

#Step2 : Clean the data
clean_data <- data %>%
  # Ensure factors are in a logical order (important for plots!)
  mutate(
    experience_level = factor(experience_level, 
                              levels = c("Entry-level", "Mid-level", "Senior-level", "Executive-level")),
    company_size = factor(company_size, 
                          levels = c("Small", "Medium", "Large")),
    # Treat work_year as a factor so it doesn't show 2023.5 on graphs
    work_year = as.factor(work_year)
  ) %>%
#Step3 : Simplify Job Titles (Categorization)
  # There are too many titles; let's group them for cleaner bar charts
  mutate(job_category = case_when(
    str_detect(job_title, "Data Scientist") ~ "Data Science",
    str_detect(job_title, "Data Engineer") ~ "Data Engineering",
    str_detect(job_title, "Analyst") ~ "Data Analysis",
    str_detect(job_title, "Machine Learning|ML") ~ "Machine Learning",
    str_detect(job_title, "Manager|Lead|Head") ~ "Management",
    TRUE ~ "Other"
  )) %>%
  
#Filter out original 'salary' and 'currency' to avoid confusion
# Person 2 only needs salary_in_usd for the regression
  select(-salary, -salary_currency)

#Save the clean version for the team
# This saves the file in your main 'Project' folder instead of a sub-folder
write_csv(clean_data, "data_clean.csv")
print("Cleaning complete! Use 'data_clean.csv' for all plots.")

#Step4: Plotting

#1. Experience Level vs. Salary (Boxplot)
# Create the Experience vs Salary Boxplot
ggplot(clean_data, aes(x = experience_level, y = salary_in_usd, fill = experience_level)) +
  geom_boxplot(alpha = 0.7) +
  # Use 'scales' to make the numbers look like currency
  scale_y_continuous(labels = label_dollar()) +
  labs(
    title = "Does Experience Actually Pay Off?",
    subtitle = "Salary distribution across different experience levels (2024)",
    x = "Level of Experience",
    y = "Salary (USD)",
    fill = "Experience"
  ) +
  theme_minimal() +
  # Remove the legend because the x-axis already tells us the levels
  theme(legend.position = "none")

#2. Salary Distribution (Histogram)
# Create the Salary Distribution Histogram
ggplot(clean_data, aes(x = salary_in_usd)) +
  geom_histogram(fill = "#2d5a3f", color = "white", bins = 30) + 
  scale_x_continuous(labels = label_dollar()) +
  labs(
    title = "The Spread of Data Science Salaries",
    subtitle = "Most salaries cluster between $100k and $200k",
    x = "Salary in USD",
    y = "Number of Employees"
  ) +
  theme_minimal()

#3. Top 10 Job Titles by Salary (Bar Chart)
# Calculate the average salary per category first
category_summary <- clean_data %>%
  group_by(job_category) %>%
  summarise(avg_salary = mean(salary_in_usd)) %>%
  arrange(desc(avg_salary))

# Plot
ggplot(category_summary, aes(x = reorder(job_category, avg_salary), y = avg_salary, fill = job_category)) +
  geom_col() +
  coord_flip() + # Makes it easier to read the labels
  scale_y_continuous(labels = label_dollar()) +
  labs(
    title = "Which Field Pays the Best?",
    x = "Job Category",
    y = "Average Salary (USD)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#4. Remote Work Impact (Violin Plot)
ggplot(clean_data, aes(x = work_models, y = salary_in_usd, fill = work_models)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.1, color = "black", outlier.shape = NA) +
  scale_y_continuous(labels = label_dollar()) +
  labs(
    title = "Remote vs. On-site: Is there a Pay Gap?",
    x = "Work Model",
    y = "Salary (USD)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

#5. Company Size vs. Pay (Boxplot)
ggplot(clean_data, aes(x = company_size, y = salary_in_usd, fill = company_size)) +
  geom_boxplot() +
  scale_y_continuous(labels = label_dollar()) +
  labs(
    title = "Does Company Size Matter?",
    x = "Company Size",
    y = "Salary (USD)"
  ) +
  theme_minimal()

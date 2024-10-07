library(readxl)
library(dplyr)
library(tidyr)  # For drop_na()
library(ggplot2)  # For plotting

# 1. Data cleaning: Load data and remove invalid entries
national_salaries <- read_excel("NationalSalaries.xlsx")
national_salaries <- national_salaries %>% select(-c(GROUP, EMP_PRSE, MEAN_PRSE, H_PCT10, H_PCT25, H_MEDIAN, H_PCT75, H_PCT90, A_PCT10, A_PCT25, A_MEDIAN, A_PCT75,A_PCT90, ANNUAL, HOURLY))
clean_data<-na.omit(national_salaries)
print("Original NationalSalaries data:")
head(clean_data)

# 2. Select columns from Salaries.xlsx and keep only those columns in the cleaned data
salaries <- read_excel("Salaries.xlsx")
salaries <- salaries %>% select(-c(Group))
head(salaries)

clean_data <- clean_data %>%
  rename(ID = "AREA", 
         State = "ST", 
         StateName = "STATE", 
         JobCode = "OCC_CODE", 
         JobName = "OCC_TITLE", 
         TotalEmployment = "TOT_EMP", 
         AverageHourlySalary = "H_MEAN", 
         AverageYearlySalary = "A_MEAN")


common_columns <- intersect(colnames(clean_data), colnames(salaries))
selected_columns <- clean_data %>% select(all_of(common_columns))
print("Selected columns from clean_data that match salaries:")
print(head(selected_columns))

#selected_columns <- subset(selected_columns, TotalEmployment !="**", AverageHourlySalary !="*",AverageHourlySalary !="#", AverageYearlySalary !="*", AverageYearlySalary !="#")
selected_columns <- selected_columns %>%
  filter(TotalEmployment != "**", 
         AverageHourlySalary != "*", 
         AverageHourlySalary != "#", 
         AverageYearlySalary != "*", 
         AverageYearlySalary != "#")

print(selected_columns)
selected_columns <- selected_columns %>%
  mutate(
    TotalEmployment = as.integer(TotalEmployment),
    AverageHourlySalary = as.numeric(AverageHourlySalary),
    AverageYearlySalary = as.numeric(AverageYearlySalary)
  )
print(selected_columns)
# Save the result to a new file for later use
write.csv(selected_columns, "selected_columns.csv")

# 3. Randomly select 1500 rows
print(paste("Number of rows in selected_columns:", nrow(selected_columns)))
random_sample <- selected_columns %>% sample_n(1500)
print("Randomly selected 1500 rows:")
print(head(random_sample))

# 4. Create a data frame with jobs having average hourly salary < 15
low_salary_jobs <- selected_columns %>% filter(AverageHourlySalary < 15)
print("Jobs with average hourly salary less than 15:")
print((low_salary_jobs))

# 5. Create a data frame with Indiana jobs and bin yearly salaries into 10 intervals
indiana_jobs <- random_sample %>% filter(State == "IN")
indiana_jobs <- indiana_jobs %>%
  mutate(Salary_Bin = cut(AverageYearlySalary, breaks = 10))

# Print the first few rows of the indiana_jobs data frame
print("Indiana jobs with yearly salary bins:")
print(head(indiana_jobs))



# 6. Find total employment for each state
total_employment <- selected_columns %>%
  group_by(State) %>%
  summarise(TotalEmployment = sum(TotalEmployment, na.rm = TRUE))
print("Total employment by state:")
print((total_employment))

# 7. Find the average yearly salary of jobs in Indiana
avg_indiana_salary <- indiana_jobs %>% summarise(Average_Salary = mean(AverageYearlySalary))
print("Average yearly salary in Indiana:")
print(avg_indiana_salary)

# 8. Compare average yearly salaries of 'Computer and mathematical occupations' across three states
computer_jobs <- selected_columns %>% filter(grepl("^15", JobCode))
selected_states <- computer_jobs %>% filter(State %in% c("IN", "CA", "NY"))
print("Computer and mathematical occupations")
print(selected_states)


# Create a bar chart comparing the average salaries
ggplot(selected_states, aes(x=State, y=AverageYearlySalary, fill=State)) +
  geom_bar(stat="identity") +
  labs(title="Comparison of Computer and Mathematical Occupations") +
  theme_minimal()


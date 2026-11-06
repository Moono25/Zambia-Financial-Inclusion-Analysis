#=================================================
# Finscope Zambia 2020
# 04_data_cleaning.R
# Purpose: Prepare and Validate the analysis dataset
#===================================================

# Define project Location
project_dir <- "C:/Users/user/Documents/DATA ANALYTICS/Zambia_Financial_Inclusion_Analysis"

data_file <- file.path( project_dir, "Data", "2020 Finscope data 7 July 2022_anonymised.sav")

# Loaded packages
library(haven)
library(dplyr)

# Load the original data and Kept this object unchanged throughout the project
Finscope <- read_sav(data_file)

# Created a new copy for cleaning using the original
Finscope_clean <- Finscope

# Record of the original dataset size
cat("Starting respondents:", nrow(Finscope),"\n")
cat("Starting variables:", ncol(Finscope),"\n")
cat("Unique Clusters:", n_distinct(Finscope$Cluster),"\n")


# Looking for key variables
Required_variables <- c("FORMAL_INCLUSION", "INFORMAL_INCLUSION", "Region", "Province",
                        "B1_5", "Edu_cate", "Final_Indivi_weight", "Cluster")

Missing_Variables <- setdiff(Required_variables, names(Finscope_clean))

if(length(Missing_Variables) > 0){ stop("Required variables not found: ", 
                                        paste(Missing_Variables, collapse = ","))}
cat("\nAll required variables are present.\n")

variable_to_check <- c("FORMAL_INCLUSION", "INFORMAL_INCLUSION", "Region", "B1_5", "Edu_cate")

for (variable in variable_to_check) {
  cat("\n-----------------------------------\n")
  cat("Variable:", variable, "\n")
  cat("Description:", attr(Finscope_clean[[variable]], "label"), "\n")
  cat("Value labels:\n")
  print(attr(Finscope_clean[[variable]], "labels"))
  cat("Missing values:", sum(is.na(Finscope_clean[[variable]])), "\n")
}

# Initial diagnostic checks - no records changed
cat("\nAge values below 16:\n")
print(Finscope_clean %>% filter(!is.na(B1_5), B1_5 < 16) %>% 
        select(B1_5, Province, Region, Cluster))

cat("\nMissing education values:\n")
print(sum(is.na(Finscope_clean$Edu_cate)))

cat("\nInclusion indicator combinations:\n")
print(
  Finscope_clean %>%
    count(FORMAL_INCLUSION, INFORMAL_INCLUSION, .drop = FALSE)
)

cat("\nMissing or invalid individual weights:\n")
print(
  Finscope_clean %>%
    summarise(
      missing_weights = sum(is.na(Final_Indivi_weight)),
      zero_or_negative_weights = sum(
        !is.na(Final_Indivi_weight) &
          Final_Indivi_weight <= 0)))

# Checking for anything missing in the inclusion indicators
Finscope_clean %>% summarise(missing_formal = sum(is.na(FORMAL_INCLUSION)),
                             missing_informal = sum(is.na(INFORMAL_INCLUSION)),
                             missing_region = sum(is.na(Region)))

# Checking rural/Urban codes
Finscope_clean %>% count(Region, .drop = FALSE)

# Inspecting respondents aged 15
Finscope_clean %>% filter(!is.na(B1_5), B1_5 < 16) %>% select(B1_5, Province, Region, Cluster)

# Examining missing education by region
Finscope_clean %>% group_by(Region) %>% summarise(respondents = n(),
                                                  missing_education = sum(is.na(Edu_cate)),
                                                  percent_missing_education = round(
                                                  mean(is.na(Edu_cate)) * 100, 2),
                                                   .groups = "drop" 
                                                  )

# Checking the observed age range
Finscope_clean %>% summarise(minimum_age = min(B1_5, na.rm = TRUE),
                    maximum_age = max(B1_5, na.rm = TRUE),
                    missing_age = sum(is.na(B1_5))
                     )
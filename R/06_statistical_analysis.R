# ============================================================
# 06_statistical_analysis.R
# FinScope Zambia 2020
# Survey-adjusted statistical analysis
# ============================================================

# Loading required packages
library(dplyr)
library(survey)
library(haven)


# Loading Fincope_EDA Dataset 
Finscope_EDA <- readRDS("C:/Users/user/Documents/DATA ANALYTICS/Zambia_Financial_Inclusion_Analysis/Data/Finscope_EDA.rds")

# Recreating survey design
Finscope_design <- svydesign(
  ids = ~Cluster,
  weights = ~Final_Indivi_weight,
  data = Finscope_EDA,
  nest = TRUE
)


# ------------------------------------------------------------
# 1. Residence and financial inclusion
# ------------------------------------------------------------

Residence_test <- svychisq(
  ~ residence + inclusion_category,
  design = FinScope_design,
  statistic = "F"
)
print(Residence_test)

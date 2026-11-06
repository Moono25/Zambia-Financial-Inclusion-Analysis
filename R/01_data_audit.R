#Install Packages
install.packages(c(
  "haven",
  "dplyr",
  "tidyr",
  "janitor",
  "skimr",
  "ggplot2",
  "scales",
  "labelled",
  "survey",
  "readr",
  "tibble"
))
intsall.packages("haven")
#Install Libraries
library(haven)
library(dplyr)
library(tidyr)
library(janitor)
library(skimr)
library(labelled)
library(scales)
library(ggplot2)
library(readr)
library(tibble)

#Importing data 

library(haven)
Finscope <- read_sav("DATA ANALYTICS/Zambia_Financial_Inclusion_Analysis/Data/2020 Finscope data 7 July 2022_anonymised.sav")
View(Finscope)

#Inspecting data
dim(Finscope) #rows and Columns
names(Finscope) #names of columns
glimpse(Finscope) # giving summary of the responses 
skim(Finscope)
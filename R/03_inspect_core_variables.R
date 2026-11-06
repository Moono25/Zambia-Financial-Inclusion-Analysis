library(haven)
library(dplyr)
library(tibble)

core_variables <- c(
  "Province",
  "Region",
  "B1_2_Sex",
  "B1_5",
  "Edu_cate",
  "FORMAL_INCLUSION",
  "INFORMAL_INCLUSION",
  "ANY_SAVE",
  "INFORMAL_BORROWING",
  "INFORMAL_TRANSFERS",
  "Final_Indivi_weight",
  "HH_Final_weight",
  "Cluster"
)

# Check which candidate variables exist
core_variables %in% names(Finscope)

# Inspect their labels, where available
for (v in core_variables) {
  if (v %in% names(Finscope)) {
    
    x <- Finscope[[v]]
    
    cat("\n\n---", v, "---\n")
    cat("Variable label:\n")
    print(attr(x, "label"))
    
    if (v %in% c("Final_Indivi_weight", "HH_Final_weight")) {
      cat("Summary:\n")
      print(summary(x))
      
    } else if (v == "Cluster") {
      cat("Unique clusters:", dplyr::n_distinct(x), "\n")
      cat("Missing:", sum(is.na(x)), "\n")
      
    } else {
      cat("Value labels:\n")
      print(attr(x, "labels"))
      cat("Observed frequencies:\n")
      print(table(x, useNA = "ifany"))
    }
  }
}



project_dir <- "C:/Users/user/Documents/DATA ANALYTICS/Zambia_Financial_Inclusion_Analysis"

data_file <- file.path(
  project_dir,
  "Data",
  "2020 Finscope data 7 July 2022_anonymised.sav"
)

Finscope <- read_sav(data_file)
#Inspecting inclusion variables ans survey design  fields

#Variables labels for the main analysis variables
main_variables <- c("FORMAL_INCLUSION","INFORMAL_INCLUSION", "Region", 
                    "Final_Indivi_weight", "HH_Final_weight", "Cluster")

for (variable in main_variables) {
  cat("\nVariable:", variable, "\n")
  cat("Label:", attr(Finscope[[variable]], "label"), "\n")
  cat("Value labels:\n")
  print(attr(Finscope[[variable]], "labels"))
}

#Searching variable names for possible strata or sampling-design fields
design_candidates <- grep(
  "strat|startum|cluster|weight|psu|ea|enumeration",
  names(Finscope),
  ignore.case = TRUE,
  value = TRUE
)

cat("\nPossible survey-design variable:\n")
print(design_candidates)

#Searching for stratification variables
grep("strat|startum|strata|sampling",
     names(Finscope),
     ignore.case = TRUE,
     value = TRUE
     )

#Inspecting variable that may identify geographic or sampling areas
grep(
  "Province|region|district|urban|rural|cluster|weight|psu|ea",
  names(Finscope),
  ignore.case = TRUE,
  value = TRUE
)

# Check the classes of the survey-design variables
sapply(
  Finscope[c(
    "Cluster",
    "Province",
    "District",
    "Region",
    "Final_Indivi_weight",
    "HH_Final_weight"
  )],
  class
)

#Checking whether each cluster belongs to province or to rural/urban 
library(dplyr)
cluster_locations <- Finscope %>% distinct(Cluster,
                                           Province, Region)
Cluster_check <- cluster_locations %>% 
  count(Cluster, name = "n_province_region") %>% filter(n_province_region > 1)

cat("Clusters linked to multiple province-region combinations:\n")
print(Cluster_check)

cat("\nNumber of distinct cluster locations:\n")
print(nrow(cluster_locations))

cat("\nClusters by province and rural/urban area:\n")
print(cluster_locations %>% count(Province, Region, name = "number_of_clusters"))

#Checking the Cluster variable
cat("Number of respondents:", nrow(Finscope),"\n")

cat("Number of unique clusters:", dplyr::n_distinct(Finscope$Cluster),
    "\n")

cat("\nRespondents per cluster:\n")

cluster_sizes <- Finscope %>% count(Cluster, name = "respondents_per_cluster")

print(summary(cluster_sizes$respondents_per_cluster))

cat("\nCluster with few or morethan 15 repondents:\n")

print(cluster_sizes %>% filter(respondents_per_cluster !=15) %>% 
        arrange(respondents_per_cluster))

# Summarise cluster sizes

cluster_sizes %>%
  summarise(
    total_clusters = n(),
    clusters_below_15 = sum(respondents_per_cluster < 15),
    clusters_at_15 = sum(respondents_per_cluster == 15),
    clusters_above_15 = sum(respondents_per_cluster > 15),
    total_respondents = sum(respondents_per_cluster)
  )
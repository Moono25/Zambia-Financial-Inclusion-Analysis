#Libraries loaded
library(dplyr)
library(haven)
library(ggplot2)
ls()

if(!exists("Finscope_clean")) 
{stop("Finscope_clean is not loaded. Run 04_data_cleaning.R first")}

# Create output folders is they exist
project_dir <- "C:/Users/user/Documents/DATA ANALYTICS/Zambia_Financial_Inclusion_Analysis"

dir.create(file.path(project_dir,"Outputs","Summary_tables"), recursive = TRUE, 
           showWarnings = FALSE)

dir.create(file.path(project_dir,"Outputs","Figures"), recursive = TRUE, 
           showWarnings = FALSE)

# Creating the four financial inclusion categories
# Verified codes from inspections 1 = Yes, 2 = No

Finscope_EDA <- Finscope_clean %>% mutate( inclusion_category = case_when(
                                          FORMAL_INCLUSION == 1 & INFORMAL_INCLUSION == 1 ~
                                            "Formal and Informal",
                                          FORMAL_INCLUSION == 1 & INFORMAL_INCLUSION == 2 ~
                                            "Formal only",
                                          FORMAL_INCLUSION == 2 & INFORMAL_INCLUSION == 1 ~
                                            "Informal only",
                                          FORMAL_INCLUSION == 2 & INFORMAL_INCLUSION == 2 ~
                                            "Neither",
                                          TRUE ~ NA_character_
                                          ),
                                          
                                          residence = haven::as_factor(Region),
                                          sex = haven::as_factor(B1_2_Sex)
                                          )
# Check that every respondent received a category
Finscope_EDA %>% count(inclusion_category, .drop = FALSE)

# Distribution of inclusion categories
Overall_inclusion <- Finscope_EDA %>%
  filter(!is.na(inclusion_category)) %>%
  count(inclusion_category, name = "respondents") %>%
  mutate(percent = round(respondents / sum(respondents) * 100, 2))
print(Overall_inclusion)

# Exporting the Summary table
write.csv(Overall_inclusion, file.path(project_dir,"Outputs","Summary_tables",
          "Overall_inclusion.csv"
          ), row.names = FALSE
          )

# Compare inclusion categories by residence
Residence_inclusion <- Finscope_EDA %>% filter(!is.na(inclusion_category), 
                                               !is.na(residence)) %>% 
                                                count(residence, inclusion_category, name = "respondents") %>% 
                                                group_by(residence) %>% mutate(percent = round(respondents / sum(respondents) * 100, 2)) %>% 
                                                ungroup()

print(Residence_inclusion)

write.csv(Residence_inclusion, file.path(project_dir,"Outputs","Summary_tables",
                                         "Inclusion_by_residence.csv"
                                          ), row.names = FALSE
                                          )

# Compare inclusion  categories by sex 
Sex_inclusion <- Finscope_EDA %>% filter(!is.na(inclusion_category), 
                                               !is.na(sex)) %>% 
  count(sex, inclusion_category, name = "respondents")%>% 
  group_by(sex) %>% mutate(
  percent = round( respondents / sum(respondents) * 100, 2)) %>% 
  ungroup()

print(Sex_inclusion)

write.csv(Sex_inclusion, file.path(project_dir,"Outputs","Summary_tables",
                                         "Inclusion_by_sex.csv"
), row.names = FALSE
)


# Visualize the inclusion categories by residence
Residence_plot <- ggplot(Residence_inclusion, aes(x = residence, y = percent, fill = inclusion_category)) +
                        geom_col(width = 0.7) + labs( title = " Finacial Inclusion by Residence in Zambia",
                                                        subtitle = "Distribution of respondents across four inclusion categories",
                                                        x = "Residence",
                                                        y = "Respondents within each residence group (%)",
                                                        fill = "Inclusion category") +
                                    scale_y_continuous(breaks = seq(0,100, by = 20)) +
                                                        coord_cartesian(ylim = c(0, 100)) +           
                                                        theme_minimal() + 
                                                        theme(plot.title = element_text(face = "bold"),
                                                              legend.position = "bottom")

print(Residence_plot)

ggsave(filename = file.path(project_dir, 
                            "Outputs",
                            "Figures",
                            "Inclusion_by_residence.png"),
        plot = Residence_plot,
        width = 10,
        height = 6,
        dpi = 300)

# Visualise formal and informal imclusion overlap
Overlap_plot <- ggplot(Overall_inclusion, aes(x = reorder(inclusion_category, -respondents), y = percent)) +
geom_col(fill = "steelblue" ,width = 0.7) + geom_text(aes(label = paste0(percent,"%")),
                                                        vjust = -0.4, size = 4) +
  labs( title = "Overlap Between Formal and Informal Inclusion",
                                x = "Inclusion category",
                                y = "Respondents (%)") +
  expand_limits(y = max(Overall_inclusion$percent) + 5) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"),
        axis.title.x = element_text(angle = 15, hjust = 1))

print(Overlap_plot)

ggsave(filename = file.path(project_dir, 
                            "Outputs",
                            "Figures",
                            "Inclusion_by_overlap.png"),
plot = Overlap_plot,
width = 10,
height = 6,
dpi = 300)


# Check that the analysis dataset exists
exists("Finscope_EDA")

# Check the variables available
names(Finscope_EDA)

# Inspect the inclusion indicators
table(Finscope_EDA$FORMAL_INCLUSION, useNA = "ifany")
table(Finscope_EDA$INFORMAL_INCLUSION, useNA = "ifany")

# Inspect the individual survey weight
summary(Finscope_EDA$Final_Indivi_weight)

# Check missing weights
sum(is.na(Finscope_EDA$Final_Indivi_weight))

# Check whether any weights are zero or negative
sum(Finscope_EDA$Final_Indivi_weight <= 0, na.rm = TRUE)

# Inspect our four inclusion categories
table(Finscope_EDA$inclusion_category, useNA = "ifany")

# Calculating the weighted financial access strands
Weighted_overall <- Finscope_EDA %>% group_by(inclusion_category) %>% 
                                      summarize(respondents = n(), 
                                                weight_population = sum(Final_Indivi_weight),
                                                .groups = "drop") %>%
                                                mutate(percentage = weight_population / sum(weight_population) * 100) %>%
                                                arrange(desc(percentage))
print(Weighted_overall)


# Calculating weighted financial access strands by residence
Weighted_residence <- Finscope_EDA %>% mutate(residence = haven::as_factor(Region)) %>%
                                        group_by(residence, inclusion_category) %>%
                                        summarise(respondents = n(), weighted_population = sum(Final_Indivi_weight),
                                                  .groups = "drop") %>% group_by(residence) %>%
                                                  mutate(percentage = weighted_population / sum(weighted_population) * 100) %>%
                                                  ungroup()
print(Weighted_residence)

# Inspecting rural and urban weighted results
print(Weighted_residence, n = Inf, width = Inf)

# Inspect the labels of our two inclusion indicators
attr(Finscope_EDA$FORMAL_INCLUSION, "label")
attr(Finscope_EDA$FORMAL_INCLUSION, "labels")

attr(Finscope_EDA$INFORMAL_INCLUSION, "label")
attr(Finscope_EDA$INFORMAL_INCLUSION, "labels")

# Inspect possible survey design variables

grep("strat|weight|cluster|psu|ea", names(Finscope_EDA), ignore.case = TRUE, value = TRUE)

Weighted_residence %>% select(residence, inclusion_category, weighted_population, percentage) %>%
                        print(n = Inf)

# Search the data dictionary for sampling design information

dictionary <- read.csv("C:/Users/user/Documents/DATA ANALYTICS/Zambia_Financial_Inclusion_Analysis/Docs/data_dictionary.csv")

dictionary[grepl("strat|cluster|sample|weight|enumeration area|selection", 
                 paste(dictionary$variable_name, dictionary$variable_label), ignore.case = TRUE),
           ]
names(dictionary)

# Inspect the cluster variable

summary(Finscope_EDA$Cluster)
length(unique(Finscope_EDA$Cluster))

# Check whether cluster IDs are repeated across provinces

Finscope_EDA %>%
  distinct(Cluster, Province) %>%
  count(Cluster) %>%
  filter(n > 1)

# Install survey packages 
install.packages("survey")
library(survey)

# Create Finscope_design and summary
FinScope_design <- svydesign(ids = ~Cluster, weights = ~Final_Indivi_weight, 
                             data = Finscope_EDA, nest = TRUE
                              )

summary(FinScope_design)

# Chi-square test on Financial inclusion and resisdence

Residence_test <- svychisq(~ residence + inclusion_category, design = FinScope_design, statistic = "F")

print(Residence_test)

# Dashboard Overall
Dashboard_overall <- Finscope_EDA %>%
  group_by(inclusion_category) %>%
  summarise(
    weighted_population = sum(Final_Indivi_weight),
    .groups = "drop"
  ) %>%
  mutate(
    percentage = weighted_population /
      sum(weighted_population) * 100
  )

print(Dashboard_overall)

# Dashboard Residence
Dashboard_residence <- Finscope_EDA %>%
  mutate(residence = haven:: as_factor(Region))%>%
  group_by(residence, inclusion_category)%>%
  summarise(weighted_population = sum(Final_Indivi_weight),.groups = "drop")%>%
  group_by(residence) %>% mutate(percentage = weighted_population / sum(weighted_population) * 100)%>%
  ungroup() 

print(Dashboard_residence, widith = Inf)

# Dashboard Sex
Dashboard_sex <- Finscope_EDA %>%
  mutate(sex = haven:: as_factor(B1_2_Sex))%>%
  group_by(sex, inclusion_category) %>% 
  summarise(weighted_population = sum(Final_Indivi_weight), .groups = "drop")%>%
  group_by(sex)%>% mutate(percentage = weighted_population / sum(weighted_population) * 100)%>%
  ungroup()

print(Dashboard_sex, width = Inf)

# EXPORT DASHBOARD DATA

# Create dashboard export folder

Dashboard_export_dir <- file.path(project_dir,"Outputs", "Summary_tables")

dir.create(Dashboard_export_dir, recursive = TRUE, showWarnings = FALSE)

# Export national overview

write.csv(Dashboard_overall, file.path(Dashboard_export_dir, "Dashboard_overall.csv"), 
          row.names = FALSE)

# Export rural and urban comparison

write.csv(Dashboard_residence, file.path(Dashboard_export_dir, "Dashboard_residence.csv"),
          row.names = FALSE)

# Export male and female comparison

write.csv(Dashboard_sex, file.path(Dashboard_export_dir, "Dashboard_sex.csv"),
          row.names = FALSE)

# Confirming csv files were created

list.files(Dashboard_export_dir, full.names = TRUE)

# Checking to see if csv files contain the right data

Residence_check <- read.csv(file.path(Dashboard_export_dir, "Dashboard_residence.csv"))

print(Residence_check)

Sex_check <- read.csv(file.path(Dashboard_export_dir, "Dashboard_sex.csv"))

print(Sex_check)

Overall_check <- read.csv(file.path(Dashboard_export_dir, "Dashboard_overall.csv"))

print(Overall_check)

# Saved cleaned EDA  dataset for later scripts
saveRDS(
  Finscope_EDA,
  "C:/Users/user/Documents/DATA ANALYTICS/Zambia_Financial_Inclusion_Analysis/Data/Finscope_EDA.rds"
)









library(haven)
library(tibble)
library(readr)

#Paths: defined once, independent of the working directory
project_dir <- "C:/Users/user/Documents/DATA ANALYTICS/Zambia_Financial_Inclusion_Analysis"

data_file <- file.path(
  project_dir, "Data", "2020 Finscope data 7 July 2022_anonymised.sav"
)

output_file <- file.path(project_dir, "Docs", "data_dictionary.csv")

# Create the Docs folder if it doesn't exist yet
dir.create(dirname(output_file), showWarnings = FALSE, recursive = TRUE)

#Load data
Finscope <- read_sav(data_file)
stopifnot(is.data.frame(Finscope))


# Pulling out the question text for each column
get_variable_label <- function(x) {
  label <- attr(x, "label", exact = TRUE)
  if (is.null(label)) NA_character_ else as.character(label)
}

# Pulling out what each code means within a column
get_value_labels <- function(x) {
  labels <- attr(x, "labels", exact = TRUE)
  if (is.null(labels)) {
    NA_character_
  } else {
    paste0(unname(labels), " = ", names(labels), collapse = "; ")
  }
}

# Build the data dictionary
data_dictionary <- tibble(
  variable_name  = names(Finscope),
  
  variable_label = vapply(
    Finscope, get_variable_label, character(1), USE.NAMES = FALSE
  ),
  
  data_type = vapply(
    Finscope, function(x) paste(class(x), collapse = ", "),
    character(1), USE.NAMES = FALSE
  ),
  
  value_labels = vapply(
    Finscope, get_value_labels, character(1), USE.NAMES = FALSE
  ),
  
  n_missing = vapply(
    Finscope, function(x) sum(is.na(x)), numeric(1), USE.NAMES = FALSE
  ),
  
  pct_missing = round(100 * n_missing / nrow(Finscope), 1),
  
  # Columns for you to fill in by hand
  research_topic  = NA_character_,
  use_in_analysis = NA_character_,
  notes           = NA_character_
)

# Safety guard
# Once you start filling in research_topic / use_in_analysis / notes, re-running
# this script would overwrite your work. Stop instead of overwriting.
if (file.exists(output_file)) {
  stop(
    "data_dictionary.csv already exists and may contain your notes. ",
    "Rename or delete it on purpose before re-running."
  )
}

# Export
# write_excel_csv() adds a UTF-8 marker so Excel displays special characters properly
write_excel_csv(data_dictionary, output_file, na = "")

#Checks
file.exists(output_file)
dim(data_dictionary)
sum(!is.na(data_dictionary$variable_label))  # should be well above 0
head(data_dictionary[, c("variable_name", "variable_label")], 10)


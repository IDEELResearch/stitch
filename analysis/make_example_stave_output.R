# make_example_stave_output.R
#
# Author: Bob Verity
# Date: 2024-09-24
#
# Inputs: analysis/data/mock_stave.xlsx
#
# Outputs: analysis/data_derived/mock_stave_prevalence.rds
#
# Purpose:
# Creates example STAVE output. Reads in example data, converts to stave,
# calculates prevalence and saves to file.
#
# ------------------------------------------------------------------

# install STAVE
#devtools::install_github("mrc-ide/STAVE")
library(STAVE)

# import mock data
file_path <- "analysis/data/mock_stave.xlsx"
input_studies <- readxl::read_excel(file_path, sheet = "studies")
input_surveys <- readxl::read_excel(file_path, sheet = "surveys")
input_counts <- readxl::read_excel(file_path, sheet = "counts")

# load into STAVE
s <- STAVE_object$new()
s$append_data(studies_dataframe = input_studies,
              surveys_dataframe = input_surveys,
              counts_dataframe = input_counts)

# get prevalence
p <- s$get_prevalence("crt:72:C")

# save to file
if (FALSE) {
  saveRDS(p, file = "analysis/data_derived/mock_stave_prevalence.rds")
}

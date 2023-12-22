library(readxl)
library(dplyr)
library(openxlsx)
library(tidyverse)
getwd()

# set wd to ("./Data/occurrence per species")
# 1. 
excel_files <- list.files(pattern = "^[^~$].*\\.xlsx$")

combined_data <- data.frame()

for (file in excel_files) {
  # Read data from the current Excel file
  data <- read_excel(file)
  combined_data <- bind_rows(combined_data, data)
}

View(combined_data)

write.xlsx(combined_data, "combined_data.xlsx", rowNames = FALSE)

# 2.

getwd()



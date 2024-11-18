rm(list=ls())
library(readxl)
library(tidyverse)
library(openxlsx)
library(lubridate)

#  read in outliers shared ---------
outliers <- read_excel("HQ_feedback/removed_prices_outliers.xlsx")

outliers_NWS <- read_excel("HQ_feedback/removed_prices_outliers.xlsx")%>%
  filter(region == "NWS")

outliers_NES <- read_excel("HQ_feedback/removed_prices_outliers.xlsx")%>%
  filter(region == "NES")
cleaned_data <- read_excel("cleaned_data/jmmi_clean_data_2024-07-17_shared.xlsx", sheet = "Cleaned dataset")
cleaned_data_NES_NWS <- read_excel("cleaned_data/jmmi_clean_data_2024-07-17_shared.xlsx", sheet = "clean_df_NES_SRP_NWS_TRY")
cleaned_data_NES <- read_excel("cleaned_data/jmmi_clean_data_2024-07-17_shared.xlsx", sheet = "clean_df_NES_SRP")
cleaned_data_NWS <- read_excel("cleaned_data/jmmi_clean_data_2024-07-17_shared.xlsx", sheet = "clean_df_NWS_TRY")

###apply changes into cleaned_data
for (i in 1:nrow(outliers)){

  id <- as.character(outliers[i, "uuid"])
  col <- as.character(outliers[i, "question"])
  new_value <- outliers[i, "New_value"]
  # Find rows in cleaned_data where X_uuid matches id
  rows_to_update <- which(cleaned_data$X_uuid == id)

  # Check if there are rows to update
  if (length(rows_to_update) > 0) {
    # Check if col exists in cleaned_data
    if (col %in% colnames(cleaned_data)) {
      # Update each row found
      cleaned_data[rows_to_update, col] <- new_value
    } else {
      warning(paste("Column", col, "does not exist in cleaned_data. Skipping update."))
    }
  } else {
    warning(paste("No rows found in cleaned_data where X_uuid =", id, ". Skipping update."))
  }
}
write.csv(cleaned_data,"test.csv")

###apply changes into cleaned_data
for (i in 1:nrow(outliers_NES)){

  id <- as.character(outliers_NES[i, "uuid"])
  col <- as.character(outliers_NES[i, "question"])
  new_value <- outliers_NES[i, "New_value"]
  # Find rows in cleaned_data where X_uuid matches id
  rows_to_update <- which(cleaned_data_NES$X_uuid == id)

  # Check if there are rows to update
  if (length(rows_to_update) > 0) {
    # Check if col exists in cleaned_data
    if (col %in% colnames(cleaned_data_NES)) {
      # Update each row found
      cleaned_data_NES[rows_to_update, col] <- new_value
    } else {
      warning(paste("Column", col, "does not exist in cleaned_data. Skipping update."))
    }
  } else {
    warning(paste("No rows found in cleaned_data where X_uuid =", id, ". Skipping update."))
  }
}
write.csv(cleaned_data_NES,"test_NES.csv")

###apply changes into cleaned_data
for (i in 1:nrow(outliers_NWS)){

  id <- as.character(outliers_NWS[i, "uuid"])
  col <- as.character(outliers_NWS[i, "question"])
  new_value <- outliers_NWS[i, "New_value"]
  # Find rows in cleaned_data where X_uuid matches id
  rows_to_update <- which(cleaned_data_NWS$X_uuid == id)

  # Check if there are rows to update
  if (length(rows_to_update) > 0) {
    # Check if col exists in cleaned_data
    if (col %in% colnames(cleaned_data_NWS)) {
      # Update each row found
      cleaned_data_NWS[rows_to_update, col] <- new_value
    } else {
      warning(paste("Column", col, "does not exist in cleaned_data. Skipping update."))
    }
  } else {

    warning(paste("No rows found in cleaned_data where X_uuid =", id, ". Skipping update."))
  }
}
write.csv(cleaned_data_NWS,"test_NWS.csv")

#####################
###apply changes into cleaned_data
for (i in 1:nrow(outliers)){

  id <- as.character(outliers[i, "uuid"])
  col <- as.character(outliers[i, "question"])
  new_value <- outliers[i, "New_value"]
  # Find rows in cleaned_data where X_uuid matches id
  rows_to_update <- which(cleaned_data_NES_NWS$X_uuid == id)

  # Check if there are rows to update
  if (length(rows_to_update) > 0) {
    # Check if col exists in cleaned_data
    if (col %in% colnames(cleaned_data_NES_NWS)) {
      # Update each row found
      cleaned_data_NES_NWS[rows_to_update, col] <- new_value
    } else {
      warning(paste("Column", col, "does not exist in cleaned_data. Skipping update."))
    }
  } else {
    warning(paste("No rows found in cleaned_data where X_uuid =", id, ". Skipping update."))
  }
}
write.csv(cleaned_data_NES_NWS,"test_nes_nws.csv")


### add all sheets together

# Load the openxlsx package
library(openxlsx)

# Load your existing workbook
wb <- loadWorkbook("cleaned_data/jmmi_clean_data_2024-07-17_shared.xlsx")

# Add a new sheet
new_sheet_name <- "cleaned_data_new"
addWorksheet(wb, sheetName = new_sheet_name)
writeData(wb, sheet = new_sheet_name, x = cleaned_data, startCol = 1, startRow = 1)
nes_new_sheet  <-"cleaned_nes_new"
addWorksheet(wb, sheetName = nes_new_sheet)
writeData(wb, sheet = nes_new_sheet, x = cleaned_data_NES, startCol = 1, startRow = 1)
nws_new_sheet  <- "cleaned_nws_new"
addWorksheet(wb, sheetName = nws_new_sheet)
writeData(wb, sheet = nws_new_sheet, x = cleaned_data_NWS, startCol = 1, startRow = 1)
nes_nws_new_sheet <- "cleaned_nws_nes_new"
addWorksheet(wb, sheetName = nes_nws_new_sheet)
writeData(wb, sheet = nes_nws_new_sheet, x = cleaned_data_NES_NWS, startCol = 1, startRow = 1)

# Save the workbook
saveWorkbook(wb, "cleaned_data/new_cleaned.xlsx", overwrite = TRUE)


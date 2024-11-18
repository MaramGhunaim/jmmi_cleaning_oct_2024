rm(list=ls())
library(readxl)
library(tidyverse)
library(openxlsx)
library(lubridate)

#  read in outliers shared ---------

outliers_shared_notes <- read_excel("log_outputs/Oct_2024/outliers/all_outliers_checked_with_notes.xlsx")%>%
  mutate(ruuid = paste(uuid,question))

outliers_shared_notes$new_value <- NULL
outliers_shared_notes$change_type <- NULL



outliers_shared <- read_excel("log_outputs/Oct_2024/outliers/all_outliers_checked.xlsx")%>%
  mutate(ruuid = paste(uuid,question))


# read in the outliers from partners------
directory.responses <- "log_outputs/Oct_2024/filled_logs/follow_up/"

# load and combine response files
response.filenames <- list.files(directory.responses, pattern="*", recursive=TRUE, full.names=TRUE)
response.filenames <- response.filenames[!grepl(".ini", response.filenames)]
combined_data <- purrr::map_df(response.filenames,~read_xlsx(.x, col_types = "text"))
unique(combined_data$new_value)
combined_data <- combined_data %>%
  dplyr::mutate(new_value = case_when(grepl("onfirm", new_value) ~ old_value,
                                      TRUE ~ new_value))
write.xlsx(combined_data, file = "log_outputs/Oct_2024/filled_logs/combined_data_filled_outliers_June.xlsx")

responses <- combined_data %>%
  mutate(ruuid = paste(uuid,question))

outliers_shared_df <- outliers_shared %>%
  mutate(received_feedback = case_when(outliers_shared$ruuid %in% responses$ruuid ~ "yes",
                                       TRUE ~ "no"))

outliers_shared_df<- outliers_shared_df %>%
  filter(received_feedback == "no")


all_outliers_checked <- plyr::rbind.fill(responses,outliers_shared_df)
all_outliers_checked <- all_outliers_checked %>% dplyr::group_by(ruuid) %>% dplyr::mutate(n=n())

all_outliers_checked <- all_outliers_checked %>%
  mutate(duplicate_remove = case_when(explanation != "internet data review"& n == 2 ~ "remove",
                                      TRUE ~ "Keep")) %>%
  filter(duplicate_remove == "Keep")
all_outliers_checked$duplicate_remove<- NULL
all_outliers_checked$n<- NULL

all_outliers_checked <- all_outliers_checked %>% dplyr::group_by(ruuid) %>% dplyr::mutate(n=n())
write.xlsx(all_outliers_checked, file = "log_outputs/Oct_2024/filled_logs/combined_data_filled_outliers_Oct.xlsx")

subset_all_outliers_checked <-all_outliers_checked %>%
  select(ruuid,new_value,change_type,explanation)

outliers_shared_notes <- outliers_shared_notes %>%
  left_join(subset_all_outliers_checked, by= c("ruuid"))

write.xlsx(outliers_shared_notes, file = "log_outputs/Oct_2024/filled_logs/combined_data_filled_outliers_Sep_with_notes.xlsx")

# check for duplicates in the combined response file -----
duplicates <- all_outliers_checked %>% dplyr::group_by(ruuid) %>% dplyr::mutate(n=n()) %>% filter(n>1)


error.message <- paste("There are duplicates in the combined response file")
if (dim(duplicates)[1] > 0) stop(error.message)
print(duplicates)
all_outliers_checked <- distinct(all_outliers_checked, uuid, question, .keep_all=TRUE)

all_outliers_checked <- all_outliers_checked %>%  filter(!is.na(uuid))
# outliers_shared_df <- outliers_shared %>%
#   mutate(followed_up = ifelse(ruuid %in% new_responses$ruuid, "yes", "no"))

write.xlsx(duplicates, (file = sprintf("log_outputs/Oct_2024/outliers/outliers_duplicate_%s.xlsx", today())))

write.xlsx(all_outliers_checked, (file = sprintf("log_outputs/Oct_2024/outliers/outliers_shared_received_%s.xlsx", today())))


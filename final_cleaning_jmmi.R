
### Cleaning Scripts Oct 2024
#######################################################################
# Set the work environment, packages, date, file names, directories,
# Read the data sets and load all the sources
#######################################################################


# Clear r environment
rm(list=ls())

# Set wd to this script's locations
this_script_path <-setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# Install packages
if (!require("pacman")) install.packages("pacman")

pacman::p_load(tidyverse, readxl, writexl, openxlsx, httr, gsheet, gridExtra,
               magrittr, sf,leaflet, mapview, anytime, lubridate, data.table,
               cleaningtools, lubridate,openxlsx,
               Hmisc, rstatix)

options(scipen=999)

# directories ----------------------------------------------------------------
month_dir <- "Oct_2024"
# cleaning log directory
cleaning_log_dir <- paste("log_outputs", month_dir, "cleaning_log", sep =  "/")
dir.create(cleaning_log_dir, recursive = TRUE)
# duplicates cleaning log
duplicates_dir <- paste("log_outputs", month_dir,"duplicates",sep = "/")
dir.create(duplicates_dir, recursive = TRUE)

# enumerator checks
enumerator_checks_dir <- paste("log_outputs", month_dir,"enumerator_checks",sep = "/")
dir.create(enumerator_checks_dir, recursive = TRUE)
# filled cleaning log
filled_logs_dir <- paste("log_outputs", month_dir,"filled_logs",sep = "/")

dir.create(filled_logs_dir, recursive = TRUE)
# item checks
item_checks_dir <- paste("log_outputs", month_dir,"item_checks",sep = "/")
dir.create(item_checks_dir, recursive = TRUE)
# converted data
raw_data_converted_dir <- paste("log_outputs", month_dir,"raw_data_converted",sep = "/")
dir.create(raw_data_converted_dir, recursive = TRUE)
# outliers
outliers_dir <- paste("log_outputs", month_dir,"outliers",sep = "/")
dir.create(outliers_dir, recursive = TRUE)
# filled outliers review
outlier_checks_dir <- paste("log_outputs", month_dir,"outlier_checks",sep = "/")
dir.create(outliers_dir, recursive = TRUE)


# Define needed variables
# User defined months
#'*CHANGE EVERY MONTH*
date <- as.Date("2024-10-01")
this_month <- as.character(format(date, "%Y-%m"))
last_month <- as.character(format(date %m-% months(1), "%Y-%m"))
next_month <- as.character(format(date %m+% months(1), "%Y-%m"))
six_months <- as.character(format(date %m-% months(6), "%Y-%m"))
one_year <- as.character(format(date %m-% months(12), "%Y-%m"))

#######################################
#Load sources
########################################

source("src/utils.R")
source("src/syria_detect_data_falsification.R")
source("funs_cleaning/currency_conversion.R")


#############################################################################################################
# READ data before checking outliers
#############################################################################################################

filename.dataset <- "inputs/raw_data/REACH_SYR_SYR1702_JMMI_Oct24_-_all_versions_-_False_-_2024-10-13-07-15-37.xlsx"
raw <- read_excel(filename.dataset, col_types="text") %>%
  mutate_at(c("start", "end","date"), ~as.character(convertToDateTime(as.numeric(.)))) %>%
  mutate_at(c("_submission_time"), ~as.character(as.Date(as.numeric(.), origin = "1899-12-30"))) %>%
  dplyr::rename(X_uuid = '_uuid')
# load tool

koboToolPath = "resources/kobo_form.xlsx"
tool.survey <- read_xlsx(koboToolPath,
                         guess_max = 50000,
                         na = c("NA", "", " ", "#N/A", "N/A", "n/a"),
                         sheet = "survey") %>% filter(!is.na(name)) %>%
  mutate(q.type=as.character(lapply(type, function(x) str_split(x, " ")[[1]][1])),
         list_name=as.character(lapply(type, function(x) str_split(x, " ")[[1]][2])),
         list_name=ifelse(str_starts(type, "select_"), list_name, NA))

tool.choices <- read_xlsx(koboToolPath,
                          guess_max = 50000,
                          na = c("NA", "", " ", "#N/A", "N/A", "n/a"),
                          sheet = "choices")
# initialize logs
cleaning.log.cols <- c("uuid", "rel.index", "check.id", "variable", "issue", "old.value", "new.value")
cleaning.log.outliers <-data.frame()
cleaning.log <- data.frame()
deletion.log <- data.frame()

#######################################33
#############################################################################################################
# 5) OTHER RESPONSES --> other_responses_log
#############################################################################################################
other.db <- get.other.db()
other.db.raw <- filter(other.db, name %in% colnames(raw))

# export "other" responses from RAW dataset and join with other.db
other.responses.raw <- raw[,c("_uuid", other.db.raw$name)] %>%
  pivot_longer(cols=c(other.db.raw$name), names_to="question.name", values_to="response.ar") %>%
  filter(!is.na(response.ar)) %>%
  dplyr::mutate(rel.index=NA) %>% select('_uuid', rel.index, question.name, response.ar)

other.responses <- other.responses.raw %>%
  mutate(response.en=translateR::translate(content.vec = response.ar,
                                           microsoft.api.key = source("resources/microsoft.api.key.syria.R")$value,
                                           microsoft.api.region = "switzerlandnorth",
                                           source.lang="ar", target.lang="en"))


#raw <- dplyr::rename(raw, '_submission_time' ='X_submission_time')
other.responses.j <- other.responses %>%
  left_join(other.db, by=c("question.name"="name")) %>% dplyr::rename(name="question.name") %>%
  left_join(select(raw,'_uuid',organisation,country_area_label,  enumerator), by="_uuid") %>%
  dplyr::rename(enumerator.code=enumerator) %>%
  select("_uuid", "rel.index", "enumerator.code", "name", "ref.question",
         "q.type", "list_name", "choices.label", "response.ar", "response.en") %>%
  mutate("TRUE other (provide a better translation if response.en is not correct)"=NA,
         "EXISTING other (copy the exact wording from the options in column H)"=NA,
         "INVALID other (insert yes or leave blank)"=NA) %>%
  left_join(select(raw, '_uuid', '_submission_time'), by="_uuid") %>%
  arrange(list_name, name)

save.other.responses()

#------------------------------------------------------------------------------------------------------------
# --> edit the file (fill in TRUE vs EXISTING vs INVALID)
# --> follow-up with field for unclear responses/translations
# --> save new file as ..._checked.xlsx
#------------------------------------------------------------------------------------------------------------

# The following will be run after the feedback, waiting for feedback to run

or <- read_excel("output/responses/2024-10-13_other_responses_edited.xlsx") %>% filter(`_uuid` %in% raw$`_uuid`)
or <- or %>%  rename(uuid =`_uuid` )
colnames(or)[str_starts(colnames(or), "TRUE")] <- "true.other"
colnames(or)[str_starts(colnames(or), "EXISTING")] <- "existing.other"
colnames(or)[str_starts(colnames(or), "INVALID")] <- "invalid.other"
or$check <- rowSums(is.na(select(or, true.other, existing.other, invalid.other)))
t <- filter(or, !is.na(choices.label) & check!=2)
if (nrow(t)>0) stop("Missing entries or multiple columns selected")
or.true <- filter(or, !is.na(true.other))
or.recode <- filter(or, !is.na(existing.other))
or.remove <- filter(or, !is.na(invalid.other))
if (nrow(bind_rows(or.true, or.recode, or.remove))!=nrow(or)) stop()

cleaning.log.other <- data.frame()
or.remove <- or.remove %>%  filter(!is.na(ref.question))
# 1) handle invalid
print(paste("Number of responses to be deleted:", nrow(or.remove)))
if (nrow(or.remove)>0) for (r in 1:nrow(or.remove)) add.to.cleaning.log.other.remove(or.remove[r,])

# 2) handle recoding
print(paste("Number of responses to be recoded:", nrow(or.recode)))
if (nrow(or.recode)>0) for (r in 1:nrow(or.recode)) add.to.cleaning.log.other.recode(or.recode[r,])

# 3) handle true
print(paste("Number of responses to be translated:", nrow(or.true)))
t <- or.true %>%
  dplyr::rename(variable=name, old.value=response.ar, new.value=true.other) %>%
  select(uuid, variable, old.value, new.value) %>%
  mutate(issue="Translation of other response",
         rel.index=NA)
cleaning.log.other <- rbind(cleaning.log.other, t)

#------------------------------------------------------------------------------------------------------------
# apply cleaning log other to the original data frame
for (r in 1:nrow(cleaning.log.other)){
  uuid <- cleaning.log.other$uuid[r]
  variable <- cleaning.log.other$variable[r]
  new.value <- cleaning.log.other$new.value[r]
  raw[raw$`_uuid` == uuid, variable] <- new.value
}


### write the data set after amending the other responses
write.xlsx(raw,"inputs/cleaned_after_changing_others.xlsx")


#### write cleaning log other
write.xlsx(cleaning.log.other,"log_outputs/Oct_2024/cleaning_log/cleaning.log.others.csv")
################################################################################

### amend the cleaning log

cleaning.log <- cleaning.log %>%  bind_rows(cleaning.log, cleaning.log.other)

#####
## Second step cleaning after other responses
#########################################

#############################################################################################################
# read the data after adding other responses and read the tool

#############################################################################################################
# LOAD DATA and Prepare the Data set
#############################################################################################################
#### change the file name depending on the files after other responses
filename.dataset <- "inputs/cleaned_after_changing_others.xlsx"
# load data
raw <- read_excel(filename.dataset, col_types="text") %>%
  mutate_at(c("start", "end","date"), ~as.character(convertToDateTime(as.numeric(.)))) %>%
  mutate_at(c("_submission_time"), ~as.character(as.Date(as.numeric(.), origin = "1899-12-30"))) %>%
  dplyr::rename(X_uuid = '_uuid')
unique(raw$`_submission_time`)

# loading the tool--------------------------------------------------------------
koboToolPath = "inputs/tools_variable/kobo_form.xlsx"
questions <- read_xlsx(koboToolPath,
                       guess_max = 50000,
                       na = c("NA", "", " ", "#N/A", "N/A", "n/a"),
                       sheet = "survey") %>% filter(!is.na(name)) %>%
  mutate(q.type=as.character(lapply(type, function(x) str_split(x, " ")[[1]][1])),
         list_name=as.character(lapply(type, function(x) str_split(x, " ")[[1]][2])),
         list_name=ifelse(str_starts(type, "select_"), list_name, NA))
choices <- read_xlsx(koboToolPath,
                     guess_max = 50000,  na = c("NA", "", " ", "#N/A", "N/A", "n/a"),
                     sheet = "choices")
notes <- questions %>% select(name,type) %>% filter(str_detect(type, "note"))
name_list <- list(notes$name)
#######################################33

#preparing the data set  -----------------------------------------------------------

#### show the number of raws
nrow(raw)
df <- raw
# Loading the smeb items that we will use for data checks-----------------------
smeb_items <- read_excel("inputs/smeb_item_lists/smeb_items.xlsx")

list_smeb_items <- smeb_items$smeb_items[smeb_items$smeb_items != ""]

################################################################################
#  Adding the geographical names
################################################################################
### check the community
unique(df$admin4_label) # any written in arabic
table(is.na(df$admin4_label)) #check if there any missing qcodes

#add geographic locations
geocodes <- read_excel("inputs/geo_codes/SYR_REACH_admin4_February2024.xlsx")

#select the columns we need
geocodes_col <- geocodes %>%
  select(Join_Key,
         region,
         AoI,
         admin1Name_en,
         admin1Name_ar,
         admin2Name_en,
         admin2Name_ar,
         admin3Name_en,
         admin3Name_ar,
         admin4Name_en = LocationName_en,
         admin4Name_ar = LocationName_ar)

R_Pcode_check <- read_excel("inputs/geo_codes/1_Geopoints_Pcode_check.xlsx") %>%
  select(Join_Key = Location_Pcode,
         region,
         AoI,
         admin1Name_en,
         admin1Name_ar,
         admin2Name_en,
         admin2Name_ar,
         admin3Name_en,
         admin3Name_ar,
         admin4Name_en = LocationName_en,
         admin4Name_ar = LocationName_ar)

geocodes_df <- rbind(geocodes_col,R_Pcode_check)
###############
# edit the data upon the first look
raw <- raw %>% rename('_uuid' = X_uuid)

#######

## amend the admin 4 level
raw <- raw %>%
  dplyr::mutate(wrong_community = case_when(
    admin4_label %in% c("N0478", "C5929", "قرية بهيرة", "قرية الجابرية",
                        "تل فارس", "ام الربيع", "تل حبش", "N0370", "N0371",
                        "N0383", "N0379", "N0376", "N0377", "N0378","C1455",
                        "C4296","C4689","C4724","C4961") ~ TRUE,
    TRUE ~ FALSE
  ))
raw$admin4_label
unique(raw$wrong_community)
for (i in 1:nrow(raw)){

  if (raw[i,'wrong_community']==TRUE){
    # Store old value before updating

    print(raw[i,"admin4_label"])

    string_value = as.character(raw[i, "admin4_label"])

    old <- string_value

    new_value <- case_when(old == "N0478" ~ "C5710",
                           old == "C5929" ~ "C8745",
                           old == "قرية بهيرة" ~ "C8329",
                           old == "قرية الجابرية" ~ "C4729",
                           old == "تل فارس" ~ "C4690",
                           old == "ام الربيع" ~ "C4717",
                           old == "تل حبش" ~ "C4693",
                           old %in% c("N0370","N0371","N0383","N0379","N0376","N0377","N0378") ~ "C4360",
                           old == "C1455" ~ "C1488",
                           old == "C4296" ~ "C4295",
                           old == "C4689" ~ "C4688",
                           old == "C4724" ~ "C4722",
                           old == "C4961" ~ "C4969",
                             TRUE ~ old )


    raw[i, "admin4_label"] <-new_value  # Update the value to 1

    uuid_value <- raw[i, "_uuid"]
    issue <- "entered the wrong community number"
    # Log the change
    new_row <- data.frame(uuid = uuid_value, variable = "admin4_label", old.value = old, new.value = new_value, issue = issue)
    cleaning.log <- bind_rows(cleaning.log, new_row)

  }
}
raw <- raw %>%  select(-"wrong_community")
write.csv(raw, paste0("cleaned_data/Archive/data_after_changing",Sys.Date(),"_community.csv"))
write.csv(cleaning.log,"log_outputs/Oct_2024/cleaning.log/change_log_community.csv")

#####
#amend admin3 level
raw <- raw %>%
  dplyr::mutate(wrong_sbd = case_when(
    admin3_label == "عامودا" ~ TRUE,
    TRUE ~ FALSE
  ))

unique(raw$wrong_sbd)
for (i in 1:nrow(raw)){

  if (raw[i,'wrong_sbd']==TRUE){
    # Store old value before updating

    print(raw[i,"admin3_label"])
    string_value = as.character(raw[i, "admin3_label"])

    old <- string_value
    new_value <- case_when(old == "عامودا" ~ "SY080202",
                                          TRUE ~ old)

    raw[i, "admin3_label"] <-new_value  # Update the value to 1

    uuid_value <- raw[i, "_uuid"]
    issue <- "entered the wrong community number"
    # Log the change
    new_row <- data.frame(uuid = uuid_value, variable = "admin3_label", old.value = old, new.value = new_value, issue = issue)
    cleaning.log <- bind_rows(cleaning.log, new_row)

  }
}
raw <- raw %>%  select(-"wrong_sbd")
write.csv(raw, paste0("cleaned_data/Archive/data_after_changing",Sys.Date(),"_sbd.csv"))
write.csv(cleaning.log,"log_outputs/Oct_2024/cleaning.log/change_log_sbd.csv")

####
# amend the organization  for H15 should be reach
raw <- raw %>%
  dplyr::mutate(wrong_org = case_when(
    enumerator == "H15" & organisation == "CARE_Shafak" ~ TRUE,
    TRUE ~ FALSE
  ))

unique(raw$wrong_org)
for (i in 1:nrow(raw)){

  if (raw[i,'wrong_org']==TRUE){
    # Store old value before updating

    print(raw[i,"organisation"])
    string_value = as.character(raw[i, "organisation"])

    old <- string_value
    new_value <- "REACH"
    raw[i, "organisation"] <-new_value  # Update the value to 1

    uuid_value <- raw[i, "_uuid"]
    issue <- "entered the wrong org"
    # Log the change

    new_row <- data.frame(uuid = uuid_value, variable ="organisation", old.value = old, new.value = new_value, issue = issue)
    cleaning.log <- bind_rows(cleaning.log, new_row)

  }
}
raw <- raw %>%  select(-"wrong_org")
write.csv(raw, paste0("cleaned_data/Archive/data_after_changing",Sys.Date(),"_org.csv"))
write.csv(cleaning.log,"log_outputs/Oct_2024/cleaning.log/change_log_org.csv")

# amend the organization if its care shafa then enumerator c1

raw <- raw %>%
  dplyr::mutate(wrong_org = case_when(
    organisation == "CARE_Shafak" ~ TRUE,
    TRUE ~ FALSE
  ))

unique(raw$wrong_org)
for (i in 1:nrow(raw)){

  if (raw[i,'wrong_org']==TRUE){
    # Store old value before updating

    print(raw[i,"enumerator"])
    string_value = as.character(raw[i, "enumerator"])

    old <- string_value
    new_value <- "Care1"
    raw[i, "enumerator"] <-new_value  # Update the value to 1

    uuid_value <- raw[i, "_uuid"]
    issue <- "entered the wrong enumerator"
    # Log the change
    new_row <- data.frame(uuid = uuid_value, variable ="enumerator", old.value = old, new.value = new_value, issue = issue)
    cleaning.log <- bind_rows(cleaning.log, new_row)

  }
}
raw <- raw %>%  select(-"wrong_org")
write.csv(raw, paste0("cleaned_data/Archive/data_after_changing",Sys.Date(),"_enum.csv"))
write.csv(cleaning.log,"log_outputs/Oct_2024/cleaning.log/change_log_enum.csv")
#amend the region
###
raw <- raw %>%
  left_join(geocodes_df, by = c("admin4_label"= "Join_Key"))

raw<- raw %>%
  mutate(region = case_when(region == "Northwest Syria" ~ "Northwest",
                            region == "Northeast Syria" ~ "Northeast",
                            TRUE ~ region))


#### we check the column values by example community, enumerator  then we do any amendment upon what we found
unique(raw$enumerator)
#####################

#"Si 003"
raw <- raw %>%
  dplyr::mutate(contains_v = str_detect(enumerator, "Si 003"))
unique(raw$contains_v)
for (i in 1:nrow(raw)){

  if (raw[i,'contains_v']==TRUE){
    # Store old value before updating

    print(raw[i,"enumerator"])
    string_value = as.character(raw[i, "enumerator"])

    old <- string_value
    raw[i, "enumerator"] <- "Si003"  # Update the value to 1
    new_value <- "Si003"
    uuid_value <- raw[i, "_uuid"]

    issue <- "entered the wrong enumerator"
    # Log the change
    new_row <- data.frame(uuid = uuid_value, variable ="enumerator", old.value = old, new.value = new_value, issue = issue)
    cleaning.log <- bind_rows(cleaning.log, new_row)

  }
}
raw <- raw %>%  select(-"contains_v")
write.csv(raw, paste0("cleaned_data/Archive/data_after_changing",Sys.Date(),"_enum.csv"))
write.csv(cleaning.log,"log_outputs/Oct_2024/cleaning.log/change_log_enum.csv")
#####

### amend the organization if its violet

# Initialize a data frame to store changes
#changes_log_org <- data.frame()
unique(raw$enumerator)
# Update the values based on the conditions
typeof(raw$enumerator)
library(dplyr)
raw <- raw %>%
  dplyr::mutate(contains_v = str_detect(enumerator, "[vV]"))
unique(raw$contains_v)
for (i in 1:nrow(raw)){

  if (raw[i,'contains_v']==TRUE){
    # Store old value before updating

    print(raw[i,"organisation"])
    string_value = as.character(raw[i, 'organisation'])

    old <- string_value
    raw[i, 'organisation'] <- "Violet"  # Update the value to 1
    new_value <- "Violet"
    uuid_value <- raw[i, "_uuid"]

    issue <- "entered the wrong organization"
    # Log the change
    new_row <- data.frame(uuid = uuid_value, variable ="organisation", old.value = old, new.value = new_value, issue = issue)
    cleaning.log <- bind_rows(cleaning.log, new_row)

  }
}
raw <- raw %>%  select(-"contains_v")
write.csv(raw, paste0("cleaned_data/Archive/data_after_changing",Sys.Date(),"_org.csv"))
write.csv(cleaning.log,"log_outputs/Oct_2024/cleaning.log/change_log_org.csv")
#####
library(dplyr)
raw <- raw %>%
  dplyr::mutate(contains_REACH = str_detect(enumerator, "منظمة كير/شفق"))
unique(raw$contains_REACH)
table(raw$contains_REACH)
for (i in 1:nrow(raw)){

  if (raw[i,'contains_REACH']==TRUE){
    # Store old value before updating

    print(raw[i,"enumerator"])
    string_value = as.character(raw[i, 'enumerator'])

    old <- string_value
    raw[i, 'enumerator'] <- "care"  # Update the value to 1
    new_value <- "care"
    uuid_value <- raw[i, "_uuid"]

    issue <- "entered the wrong enumerator code"
    # Log the change
    new_row <- data.frame(uuid = uuid_value, variable ="enumerator", old.value = old, new.value = new_value, issue = issue)
    cleaning.log <- bind_rows(cleaning.log, new_row)

  }
}
raw <- raw %>%  select(-"contains_REACH")
write.csv(raw, paste0("cleaned_data/Archive/data_after_changing",Sys.Date(),"_enumerator_wrong_enum.csv"))
write.csv(cleaning.log,"log_outputs/Oct_2024/cleaning.log/change_log_region.csv")
#####
#####
library(dplyr)
raw <- raw %>%
  dplyr::mutate(contains_REACH = str_detect(enumerator, "Si 003"))
unique(raw$contains_REACH)
table(raw$contains_REACH)
for (i in 1:nrow(raw)){

  if (raw[i,'contains_REACH']==TRUE){
    # Store old value before updating

    print(raw[i,"enumerator"])
    string_value = as.character(raw[i, 'enumerator'])

    old <- string_value
    raw[i, 'enumerator'] <- "Si003"  # Update the value to 1
    new_value <- "Si003"
    uuid_value <- raw[i, "_uuid"]

    issue <- "entered the wrong enumerator code"
    # Log the change
    new_row <- data.frame(uuid = uuid_value, variable ="enumerator", old.value = old, new.value = new_value, issue = issue)
    cleaning.log <- bind_rows(cleaning.log, new_row)

  }
}
raw <- raw %>%  select(-"contains_REACH")
write.csv(raw, paste0("cleaned_data/Archive/data_after_changing",Sys.Date(),"_enumerator_wrong_enum.csv"))
write.csv(cleaning.log,"log_outputs/Oct_2024/cleaning.log/change_log_region.csv")
#####
library(dplyr)
raw <- raw %>%
  dplyr::mutate(contains_REACH = str_detect(enumerator, "HH30ج"))
unique(raw$contains_REACH)
table(raw$contains_REACH)
for (i in 1:nrow(raw)){

  if (raw[i,'contains_REACH']==TRUE){
    # Store old value before updating

    print(raw[i,"enumerator"])
    string_value = as.character(raw[i, 'enumerator'])

    old <- string_value
    raw[i, 'enumerator'] <- "HH30"  # Update the value to 1
    new_value <- "HH30"
    uuid_value <- raw[i, "_uuid"]

    issue <- "entered the wrong enumerator code"
    # Log the change
    new_row <- data.frame(uuid = uuid_value, variable ="enumerator", old.value = old, new.value = new_value, issue = issue)
    cleaning.log <- bind_rows(cleaning.log, new_row)

  }
}
raw <- raw %>%  select(-"contains_REACH")
write.csv(raw, paste0("cleaned_data/Archive/data_after_changing",Sys.Date(),"_enumerator_wrong_enum.csv"))
write.csv(cleaning.log,"log_outputs/Oct_2024/cleaning.log/change_log_region.csv")

unique(raw$enumerator)

# change Samaritans_Purse to REACH
library(dplyr)
raw <- raw %>%
  dplyr::mutate(contains_REACH = str_detect(organisation, "Samaritans_Purse"))
unique(raw$contains_REACH)
table(raw$contains_REACH)
for (i in 1:nrow(raw)){

  if (raw[i,'contains_REACH']==TRUE){
    # Store old value before updating

    print(raw[i,"organisation"])
    string_value = as.character(raw[i, 'organisation'])

    old <- string_value
    raw[i, 'organisation'] <- "REACH" # Update the value to 1
    new_value <- "REACH"
    uuid_value <- raw[i, "_uuid"]

    issue <- "entered the wrong enumerator code"
    # Log the change
    new_row <- data.frame(uuid = uuid_value, variable ="organisation", old.value = old, new.value = new_value, issue = issue)
    cleaning.log <- bind_rows(cleaning.log, new_row)

  }
}
raw <- raw %>%  select(-"contains_REACH")
write.csv(raw, paste0("cleaned_data/Archive/data_after_changing",Sys.Date(),"_enumerator_wrong_org.csv"))
write.csv(cleaning.log,"log_outputs/Oct_2024/cleaning.log/final_cleaning.csv")

##################
### check for NA in currency_shop

deletion.log <- raw %>% filter(is.na(shop_currency)) %>% select(`_uuid`, enumerator, `_index`) %>%
  mutate(issue = "no currency")

raw <- raw %>%  filter(!is.na(shop_currency))

##########################################################################


# check the region which is NA which means we do not have those pcodes and filter out the region
deletion.region <- raw %>%  filter(!is.na(region)) %>%
  select(`_uuid`, enumerator, `_index`) %>%
  mutate(issue = "blanks region")

raw <- raw %>%
  filter(!is.na(region))

### apend the deletion log
deletion.log <- rbind(deletion.log,deletion.region )

# check the region which is NA which means we do not have those pcodes and filter out the region
deletion.gos <- raw %>%  filter(region == "GoS") %>%
  select(`_uuid`, enumerator, `_index`) %>%
  mutate(issue = "blanks region")

raw <-raw %>%
  filter(region != "GoS")

### apend the deletion log
deletion.log <- rbind(deletion.log,deletion.gos )


### check region after the amendment
table(is.na(raw$region))
table(raw$region)

######################################
#check duplicate
###################################
duplicates <- raw %>%
  group_by(`_uuid`) %>%
  filter(n() > 1) %>%
  ungroup()


write.csv(raw, "cleaned_data/temporary/final_cleaned_before_outlier.csv")


# start join the admins


## keep a copy of raw
df <- raw
#######
df <- read.xlsx("cleaned_data/cleaned_final_data.xlsx", sheet = "Cleaned dataset")
# create the summary table for BHA report
summarise_table_shop <- df %>%
  group_by(region) %>%
  dplyr::summarise(shop_count = n(), .groups = "drop")

summarise_table_org_without_reach <- df %>% filter(organisation != "REACH") %>%
  dplyr::group_by(region) %>%
  dplyr::summarise(org_count = n_distinct(organisation), .groups = "drop")

summarise_table_community <- df %>%
  dplyr::group_by(region) %>%
  dplyr::summarise(community_count = n_distinct(admin4_label), .groups = "drop")

summarise_table_gov <- df %>%
  dplyr::group_by(region) %>%
  dplyr::summarise(gov_count = n_distinct(admin1_label), .groups = "drop")

summarise_table_district <- df %>%
  dplyr::group_by(region) %>%
  dplyr::summarise(gov_count = n_distinct(admin2_label), .groups = "drop")

summarise_table_sub_district <- df %>%
  dplyr::group_by(region) %>%
  dplyr::summarise(gov_count = n_distinct(admin3_label), .groups = "drop")

summarise_table_org_name <- df %>%
  dplyr::group_by(region) %>%
  dplyr::summarise(unique_organisations = paste(unique(organisation), collapse = ", "), .groups = "drop")


summary_table <-list(
  "raw_first_clean" = df,
  "shop_count" = summarise_table_shop,
  "organization_count" = summarise_table_org_without_reach,
  "gov_count" = summarise_table_gov,
  "district_count" = summarise_table_district,
  "sub_district_count" = summarise_table_sub_district,
  "community_count" = summarise_table_community,
  "Org_name_region" = summarise_table_org_name
)

write.xlsx(summary_table, "output/summary/summary_table.xlsx")
###########################################################################

# START::COVERAGE CHECKS #######################################################################
# Coverage function
summarize_coverage <- function(df, grouping_columns) {
  grouped_df <- df %>%
    dplyr::group_by(across(all_of(grouping_columns))) %>%
    dplyr::summarize(number_surveys = n(), .groups = 'drop') %>%
    dplyr::arrange(number_surveys)
  return(grouped_df)
}

coverage_org <- summarize_coverage(df, c("region", "organisation", "admin2Name_en", "admin3Name_en"))
coverage_org_community <- summarize_coverage(df, c("region", "organisation", "admin2Name_en", "admin3Name_en", "admin4Name_en"))
coverage_org_subdistrict <- summarize_coverage(df, c("region", "organisation", "admin1Name_en", "admin2Name_en", "admin3Name_en"))

# 1: Surveys done per enumerator per partner per surveys done.

enum_id_survey_checks <- df %>%
  dplyr::group_by(Enumerator = enumerator, Organisation = organisation) %>%
  dplyr::summarise(num_surveys = n()) %>%
  dplyr::mutate(less_than_5 = case_when(num_surveys < 4 ~ "yes",
                                 TRUE ~ "no"),
         less_than_10 = case_when(num_surveys < 10 ~ "yes",
                                  TRUE ~ "no"))


# 2: Surveys per day per enumerator per organisation per date

enum_date_survey_checks <- df %>%
  group_by(Enumerator = enumerator,
           Organisation = organisation,
           Date = date) %>%
  dplyr::summarise(num_surveys = n()) %>%
  dplyr::mutate(less_than_2 = case_when(num_surveys < 2 ~ "yes",
                                 TRUE ~ "no"))

# 3 : count how many prices per item per community



# Select relevant columns from df
items.list.data <- df %>% dplyr::select(
  ends_with("_price_item"),
  water_truck_liter_price_unit_item,
  exchange_rate_buy_usd_syp,
  exchange_rate_sell_usd_syp,
  exchange_rate_buy_usd_try,
  exchange_rate_sell_usd_try,
  internet_data_unit_item
)

# Get column names for later reference
items.list.dft.names <- names(items.list.data)

# Convert all relevant columns to numeric, handling non-numeric values gracefully
df_numeric <- df %>%
  dplyr::mutate(across(all_of(items.list.dft.names), ~ as.numeric(as.character(.)))) %>%
  select(admin4_label,organisation, country_area_label,all_of(items.list.dft.names))
typeof(df_numeric$onion_price_item)
df_org <- df_numeric %>%  select(admin4_label, organisation, country_area_label)
# Calculate the sum for each numeric column
df_long <- df_numeric %>%
  pivot_longer(
    cols = all_of(items.list.dft.names),
    names_to = "item",
    values_to = "price"
  )

# Count prices per item per community
df_column_sums <- df_long %>%
  group_by(admin4_label,item) %>%
  dplyr::summarise(price_count = n(), .groups = 'drop') %>% filter(price_count < 4)

df_column_sums_with_org <- df_column_sums %>%  left_join(df_org, by = "admin4_label") %>%
  select(country_area_label,organisation, admin4_label, everything()) %>%  distinct()
# Save the column sums to an Excel file
write.xlsx(df_column_sums_with_org, file.path(item_checks_dir, "df_column_sums.xlsx"))

#
# save the files
enumerator_productivity <- list(
  "Surveys_Partner_Enumerator" = enum_id_survey_checks,
  "Surveys_Enumerator_Date" = enum_date_survey_checks,
  "Organisation_coverage" = coverage_org,
  "Community_coverage" = coverage_org_community,
  "Subdistrict_coverage" = coverage_org_subdistrict,
  "Prices_less_than_4" = df_column_sums_with_org)

write.xlsx(enumerator_productivity, file.path(enumerator_checks_dir, "overall_productivity_check.xlsx"))

###############
### amend anything in the data set upon the needs

##### split the follow ups less than 4 prices by organization
## NWS
df_column_sums_with_org_NWS <- df_column_sums_with_org %>%  filter(region == "Northwest")
org_list <- unique(df_column_sums_with_org_NWS$organisation)

for(org in org_list) {
  df <- df_column_sums_with_org_NWS

  df <- df %>%
    filter(organisation == org)

  if (nrow(df) > 0) {
    output_dir <- "log_outputs/Oct_2024/enumerator_checks/Northwest"
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }

    write.csv(df, paste0(output_dir, "/",org, "_price_less_than_4.csv"))
  } else {
    message(paste("No data for organization:", org))
  }
}

## NES
df_column_sums_with_org_NES <- df_column_sums_with_org %>%  filter(country_area_label == "Northeast")
org_list <- unique(df_column_sums_with_org_NWS$organisation)

for(org in org_list) {
  df <- df_column_sums_with_org_NES

  df <- df %>%
    filter(organisation == org)

  if (nrow(df) > 0) {
    output_dir <- "log_outputs/Oct_2024/enumerator_checks/Northeast"
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }

    write.csv(df, paste0(output_dir, "/",org, "_price_less_than_4.csv"))
  } else {
    message(paste("No data for organization:", org))
  }
}



# START::CURRECNY CONVERSION #######################################################################

#'*EXCHANGE RATES USING MEDIANS OF THE EXCAHNGE RATE REPORTED FOR NORTHEAST AND NORTHWEST SYRIA*
# calculate medians of exchange rates: Turkish_lira-Syrian_pound & US_dollar-Syrian_pound for each region
# Any calculate ending with "_price_item" is the price in standard unit KG/L.
# getting the prices that standard.
# there was a mistake on the soap_bar_price_item calculation therefore we need
# to delete it and calculate it again
# soap_price_item: This should give the price based on the unit selected
# soap_bar_price_item: This should give the price in KG
#df$soap_bar_price_item <- NULL
############## amend the soap_price
df <- df %>%
  dplyr::mutate(soap_bar_price_item = (as.numeric(soap_price_item) * 6.67)) %>%
  relocate(soap_bar_price_item, .after = soap_price_item)
######## prepare the item.list
items.list.data <- df %>% dplyr::select(ends_with("_price_item"),
                                        water_truck_liter_price_unit_item,
                                         exchange_rate_buy_usd_syp,
                                        exchange_rate_sell_usd_syp,
                                        exchange_rate_buy_usd_try,
                                        exchange_rate_sell_usd_try,
                                        internet_data_unit_item)

write.xlsx(items.list.data, file.path(item_checks_dir, "items.list.data.xlsx"))

items.list <- names(items.list.data)

# we do not want to convert the exchange rates
items.list <- items.list[!str_detect(items.list,"exchange_rate_") &
                           !str_detect(items.list,"internet_data_modem_unit_item") &
                           !str_detect(items.list,"internet_data_unit_item")]

df <- df %>%
  mutate_at(items.list, as.numeric)


############
### check the currency and count the currency
###########
# currency.calc.count <- df_currency %>%
#   dplyr::group_by(region, shop_currency) %>%  dplyr::summarise(n=n())
#
# write.csv(currency.calc.count, "currency.calc.count.csv")
# currency.exchange.rate <- df_currency %>%  select(region, exchange_rate_sell_usd_syp,
#                                                   exchange_rate_sell_usd_try) %>%
#   dplyr::group_by(region, exchange_rate_sell_usd_syp, exchange_rate_sell_usd_try)%>%
#   dplyr::summarise(n = n())
# write.csv(currency.exchange.rate, "currency.exchange.rate.csv")
# df$exchange_rate_buy_usd_try
# df$exchange_rate_sell_usd_syp
#
# syrian_NWS_follow_up <- df_currency %>%  filter(region == "Northwest" & shop_currency =="Syrian_pound")
#
# write.csv(syrian_NWS_follow_up, "syrian_NWS_follow_up.csv")
#### always check he new currency
df_currency <- df %>%
  dplyr::mutate(
    exchange_rate_medians_sell = case_when(

      region == "Northeast" & shop_currency == "US_dollar" ~ median(exchange_rate_sell_usd_syp, na.rm = TRUE),
      region == "Northeast" & shop_currency == "Syrian_pound" ~ 1,
      region == "Northwest" & shop_currency == "US_dollar" ~ median(exchange_rate_sell_usd_try, na.rm = TRUE),
      region == "Northwest" & shop_currency == "Syrian_pound" ~ (1/480),
      region == "Northwest" & shop_currency == "Turkish_lira" ~ 1,
      TRUE ~ NA_real_
    )
  )

unique(df_currency$exchange_rate_medians_sell)


### check the added median exchange rate
write.xlsx(df_currency, file.path(item_checks_dir, "currency.calc.xlsx"))

###  create the conversion function
Currency_change <- function(df, items.list) {
  for (col.price in items.list) {
    df <- df %>%
      dplyr::mutate(!!sym(col.price) := (!!sym(col.price) * exchange_rate_medians_sell))
  }
  return(df)
}

#### we can use the source conversion function
currency.converted <- Currency_change(df_currency, items.list)

## test the conversion
write.csv(currency.converted, "converted.test.csv")


nes_converted_Syrian_pound <- currency.converted %>% filter(region == "Northeast")
nws_converted_Turkish_lira <- currency.converted %>% filter(region == "Northwest")

##Saving all the datasets
market_monitoring_raw_data <-
  list(
    "Raw_data" = df,
    "Data_NES_SRP_NWS_TRY" = currency.converted,
    "Converted_NES_SRP" = nes_converted_Syrian_pound,
    "Converted_NWS_TRY" = nws_converted_Turkish_lira
  )

write.xlsx(market_monitoring_raw_data, file.path(raw_data_converted_dir, "market_monitoring_raw_data.xlsx"))


# START::LOGICAL CHECKS #######################################################################


# Actual logical checks -----------------------------------------------------
#reading in the checklist
check_list <- read_excel("inputs/check_list.xlsx")

#creating the outlier function to use in the logical checks
check_outlier <- function(df,x,var_name,aggregation_var=NULL,coef = 1.5) {

  if(is.null(aggregation_var)){
    return(is_outlier(x,coef = coef))
  } else {
    df %>% group_by(!!sym(aggregation_var)) %>% mutate(
      is_outlier = is_outlier(!!sym(var_name),coef = coef)
    ) %>% pull(is_outlier)

  }
}

# function for checking outliers in the fuel and water prices
# the function uses the dataframe df_currency and df
# if it generates an error debug but it should work
df_currency <- df_currency %>% rename(X_uuid =`_uuid`)
devtools::install_github("impact-initiatives/cleaningtools")
source("3_outliers_fuel.R")
library(cleaningtools)
# obtaining the outliers with the function above
library(dplyr)
table_dft <- outliers_fuel_water(df_currency)
table_dft_west <- table_dft %>% filter(region == "Northwest") #outliers for nws
table_dft_east <- table_dft %>% filter(region == "Northeast") #outliers for nes

# questions preparation for the data merge
df_translation <- questions %>%
  dplyr::select(`label::arabic`,name)%>%
  na.omit() %>%
  dplyr::rename("question" = name)

# checks for northwest----------------------------------------------------------
df_counts <- df_currency %>%
  filter(region == "Northwest")

df_check_northwest <- df_counts %>%
  cleaningtools::check_duplicate(uuid_column = "X_uuid") %>%
  cleaningtools::check_pii(uuid_column = "X_uuid") %>%
  cleaningtools::check_logical_with_list(uuid_column = "X_uuid",
                          list_of_check = check_list,
                          check_id_column = "check_id",
                          check_to_perform_column = "check",
                          columns_to_clean_column = "variables_to_clean",
                          description_column = "description"
  )


df_check_northwest_list <- cleaningtools::create_combined_log(df_check_northwest) %>%
  cleaningtools::add_info_to_cleaning_log( dataset_uuid_column = "X_uuid",
                            cleaning_log_uuid_column = "uuid",
                            information_to_add = c("region","organisation","deviceid",
                                                   "enumerator","admin4_label","admin4Name_en","admin4Name_ar","shop_currency"))
########shouldcheck
df_check_northwest_list$cleaning_log <- df_check_northwest_list$cleaning_log %>%
  mutate(old_value = case_when(old_value == "NA" ~ NA_character_,
                               TRUE ~ old_value)) %>%
  group_by(check_binding) %>%
  mutate(Note = case_when(
    any(str_detect(old_value, "[:alpha:]")) ~ "has_note",
    TRUE ~ "no_note"
  )) %>%
  ungroup()
# df_2 <- df_check_northwest_list$cleaning_log
# df_3 <- df_check_northwest_list$cleaning_log
#'*SAVing the all the cleaning logs for northeast with the notes NOTES*
df_check_northwest_list %>%
  cleaningtools::create_xlsx_cleaning_log(output_path = paste0("log_outputs/Oct_2024/cleaning_log/northwest/logical_outlier_checks_northwest.xlsx"),
                           change_type_col = "change_type",
                           cleaning_log_name = "cleaning_log",
                           kobo_survey = questions,
                           kobo_choices = choices,
                           sm_dropdown_type = "logical",
                           use_dropdown = TRUE)

# filtering out the prices with no notes and the questions we want to share with thw partners
northwest_log_notes <- df_check_northwest_list$cleaning_log

df_check_northwest_list$cleaning_log <- df_check_northwest_list$cleaning_log %>%
  dplyr::filter(Note == "no_note") %>%
  filter(str_ends(question, "price_unit_item")|
           question %in% c("country_area_label",
                           "shop_currency",
                           "admin4_label",
                           "internet_data_modem_std_unit",
                           "internet_data_std_unit",
                           "internet_data_unit_item",
                           "internet_data_std_unit",
                           "exchange_rate_buy_usd_syp",
                           "exchange_rate_buy_try_syp",
                           "exchange_rate_sell_try_syp",
                           "exchange_rate_buy_usd_try",
                           "exchange_rate_sell_usd_try"
           ))

# deleting the note column we added
# df_check_northwest_list$cleaning_log$Note <- NULL

# binding the logical checks and the fuel cheks generated by the outlier fuel function
df_check_northwest_list$cleaning_log <- plyr::rbind.fill(df_check_northwest_list$cleaning_log, table_dft_west)

df_check_northwest_list$cleaning_log <- left_join(df_check_northwest_list$cleaning_log, df_translation, by = "question")

df_check_northwest_list$cleaning_log <- df_check_northwest_list$cleaning_log %>%
  mutate(explanation = " ") %>%
  relocate("issue",explanation,"old_value","new_value","change_type", .after = "label::arabic")


#Saving the cleaning logs for each organisation in northwest--------------------
library(dplyr)

org_list <- unique(df_check_northwest_list$cleaning_log$organisation)

# Iterate over each organization
for(org in org_list) {
  data_check_2 <- df_check_northwest_list

  data_check_2$cleaning_log <- data_check_2$cleaning_log %>%
    filter(organisation == org)

  if (nrow(data_check_2$cleaning_log) > 0) {
    output_dir <- "log_outputs/Oct_2024/cleaning_log/northwest/"
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }

    cleaningtools::create_combined_log(data_check_2) %>%
      cleaningtools::create_xlsx_cleaning_log(output_path = paste0(output_dir, "northwest_data_checks_", org, ".xlsx"),
                               change_type_col = "change_type",
                               cleaning_log_name = "cleaning_log",
                               kobo_survey = questions,
                               kobo_choices = choices,
                               sm_dropdown_type = "logical",
                               use_dropdown = TRUE)
  } else {
    message(paste("No data for organization:", org))
  }
}

#'*NOTE THAT NO data for organisation NA are the PII checks that will be save in the file for all checks for northwest*

# checks for northeast----------------------------------------------------------
df_counts <- df_currency %>%
  filter(region == "Northeast")

library(cleaningtools)
df_check_northeast <- df_counts %>%
  check_duplicate(uuid_column = "X_uuid") %>%
  check_pii(uuid_column = "X_uuid") %>%
  check_logical_with_list(uuid_column = "X_uuid",
                          list_of_check = check_list,
                          check_id_column = "check_id",
                          check_to_perform_column = "check",
                          columns_to_clean_column = "variables_to_clean",
                          description_column = "description"
  )

df_check_northeast_list <- create_combined_log(df_check_northeast) %>%
  add_info_to_cleaning_log( dataset_uuid_column = "X_uuid",
                            cleaning_log_uuid_column = "uuid",
                            information_to_add = c("region","organisation","deviceid",
                                                   "enumerator","admin4_label","admin4Name_en","admin4Name_ar","shop_currency"))

df_check_northeast_list$cleaning_log <- df_check_northeast_list$cleaning_log %>%
  mutate(old_value = case_when(old_value == "NA" ~ NA_character_,
                               TRUE ~ old_value)) %>%
  group_by(check_binding) %>%
  mutate(Note = case_when(
    any(str_detect(old_value, "[:alpha:]")) ~ "has_note",
    TRUE ~ "no_note"
  )) %>%
  ungroup()

#'*SAVing the all the cleaning logs for northeast with the notes NOTES*

df_check_northeast_list %>%
  create_xlsx_cleaning_log(output_path = paste0("log_outputs/Oct_2024/cleaning_log/northeast/logical_outlier_checks_northeast.xlsx"),
                           change_type_col = "change_type",
                           cleaning_log_name = "cleaning_log",
                           kobo_survey = questions,
                           kobo_choices = choices,
                           sm_dropdown_type = "logical",
                           use_dropdown = TRUE)
northeast_log_notes <- df_check_northeast_list$cleaning_log
# filtering out the prices with no notes and the questions we want to share with thw partners
df_check_northeast_list$cleaning_log <- df_check_northeast_list$cleaning_log %>%
  dplyr::filter(Note == "no_note") %>%
  filter(str_ends(question, "price_unit_item")|
           question %in% c("country_area_label",
                           "shop_currency",
                           "admin4_label",
                           "internet_data_modem_std_unit",
                           "internet_data_std_unit",
                           "internet_data_unit_item",
                           "internet_data_std_unit",
                           "exchange_rate_buy_usd_syp",
                           "exchange_rate_buy_try_syp",
                           "exchange_rate_sell_try_syp",
                           "exchange_rate_buy_usd_try",
                           "exchange_rate_sell_usd_try"
           ))

# deleting the note column
# df_check_northeast_list$cleaning_log$Note <- NULL

# binding the logical checks and the fuel cheks generated by the outlier fuel function
df_check_northeast_list$cleaning_log <- plyr::rbind.fill(df_check_northeast_list$cleaning_log, table_dft_east)

# adding the question labels
df_check_northeast_list$cleaning_log <- left_join(df_check_northeast_list$cleaning_log, df_translation, by = "question")
df_check_northeast_list$cleaning_log <- df_check_northeast_list$cleaning_log %>%
  mutate(explanation = " ") %>%
  relocate("issue",explanation,"old_value","new_value","change_type", .after = "label::arabic")


#Saving the cleaning logs for each organisation in northeast--------------------

org_list <- unique(df_check_northeast_list$cleaning_log$organisation)

for(org in org_list) {
  data_check_2 <- df_check_northeast_list

  data_check_2$cleaning_log <- data_check_2$cleaning_log %>%
    filter(organisation == org)

  if (nrow(data_check_2$cleaning_log) > 0) {
    output_dir <- "log_outputs/Oct_2024/cleaning_log/northeast/"
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE)
    }

    create_combined_log(data_check_2) %>%
      create_xlsx_cleaning_log(output_path = paste0(output_dir, "northeast_data_checks_", org, ".xlsx"),
                               change_type_col = "change_type",
                               cleaning_log_name = "cleaning_log",
                               kobo_survey = questions,
                               kobo_choices = choices,
                               sm_dropdown_type = "logical",
                               use_dropdown = TRUE)
  } else {
    message(paste("No data for organization:", org))
  }
}

#Saving all outliers:

northwest_outliers <- df_check_northwest_list$cleaning_log
northeast_outliers <- df_check_northeast_list$cleaning_log

#northwest_outliers <- northwest_outliers %>% select(-c("label::arabic.x","label::arabic.y"))
all_outliers <- rbind(northwest_outliers,northeast_outliers)
all_outliers_notes <- rbind(northeast_log_notes, northwest_log_notes)



write.xlsx(df_currency, "cleaned_data/temporary/temporary_data_before_adding_outlier.xlsx")
write.xlsx(all_outliers, file = "log_outputs/Oct_2024/outliers/all_outliers_checked.xlsx")
write.xlsx(all_outliers_notes, file = "log_outputs/Oct_2024/outliers/all_outliers_checked_with_notes.xlsx")
#'*STOP*
#'*START Here after receiving feedback from the partner*
#'*Scripts for binding all the cleaning logs together from the different partners are in script 2a_outlier_review*
#'#'*Next step is to *
#'*Review outliers received (review on daily basis) integrate also the logical logs*
# START::INTERGRATING THE FEEDBACK INTO THE DATA #############################################################
# rm(list=ls())
# this_script_path <-setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
# getwd()
# open outlier review source

library(cleaningtools)
library(tidyverse)
library(readxl)
library(readxl)
library(Hmisc)
source("funs_cleaning/calculations.R")
source("funs_cleaning/currency_conversion.R")
# item checks
month_dir <- "Oct_2024"
item_checks_dir <- paste("log_outputs", month_dir,"item_checks",sep = "/")
dir.create(item_checks_dir, recursive = TRUE)
#read in the previous saved data  that is the raw data. # check for the correction of the region in the cleaning logs

dft <- read.csv("cleaned_data/temporary/final_cleaned_before_outlier.csv")
# open source "2a_outlier_review.R" and run it and change the month
# # read in the filled responses:
responses <- read_excel("log_outputs/Oct_2024/filled_logs/combined_data_filled_outliers_oct_with_notes.xlsx" )

responses$change_type <- NULL

responses <- responses %>%
  mutate(changed.type = case_when( old_value != new_value ~ "change_response",
                                   old_value == new_value ~ "no_action",
                                   is.na(new_value) ~ "blank_response")
  )
responses <- responses %>%  filter(uuid != "all") %>% filter(!is.na(uuid)) %>% filter(uuid != "ad4df46e-f8f2-40be-94a9-a98853455d67")
# function for reviewing the cleaning log whether it is okay
cleaningtools::review_cleaning_log( raw_dataset = dft,
                                    raw_data_uuid_column = "X_uuid",
                                    cleaning_log = responses,
                                    cleaning_log_change_type_column = "changed.type",
                                    change_response_value = "change_response",
                                    cleaning_log_question_column = "question",
                                    cleaning_log_uuid_column = "uuid",
                                    cleaning_log_new_value_column = "new_value")

unique(responses$uuid)
responses_tab<- responses %>%
dplyr::group_by(ruuid) %>% dplyr::mutate(n=n())
write.xlsx(responses_tab,"responses_tab.xlsx")
# function for actually doing data cleaning

df_clean <- create_clean_data(raw_dataset = dft, # name of your dataset
                              raw_data_uuid_column = 'X_uuid', #nameof uuid column in dataset
                              cl = responses, #nameof your cleaning loh
                              cleaning_log_change_type_column = "changed.type",# add a column in your cleaning log called "change_type"
                              #for all the values that have change write "change_response"
                              change_response_value = "change_response", # this reads then change_type column and tells the function to update the dataset
                              NA_response_value = "blank_response", # this reads then change_type column and tells the function to update to NA
                              no_change_value = "no_action", # this reads then change_type column and tells the function not to update the dataset
                              remove_survey_value = "Delete",# this reads then change_type column and tells the function to delete the row
                              cleaning_log_question_column =  "question",# the variable name in the cleaning log
                              cleaning_log_uuid_column = 'uuid',# uuid of the cleaning log
                              cleaning_log_new_value_column = "new_value") #the column of the new value


#save the clean dataset

write.xlsx(df_clean, file = "log_outputs/Oct_2024/df_clean_changed.xlsx")

# redoing all the calculations in the data: read through the UDF:
#source("funs_cleaning/calculations.R")
calculations_data <- read_excel("funs_cleaning/calculations_function.xlsx")

df_recalculated <- apply_calculations(df_clean)

# save the recalculated df and compare with previously saved df to see if the calcuations worked
write.xlsx(df_recalculated, file = "cleaned_data/temporary/df_recalculated.xlsx")


#*This next codes are for preparing the following things please modify the codes according to the needs of the new tool*
#'#'*Note that we were using this for Octtember data cleaning so somethings are already in the cleaning log for Oct*
#'#'#'*Deletion log*
#'#'#'*Cleaning log*
#'#'#'#'*Variable tracker*
#'#'#'#'#'*Data extract*
#  Preparing the deletion log -------
deletion_log <- df_recalculated %>%
  filter(region == "GoS") %>%
  select(uuid = X_uuid, deviceid, Enumerator_id ="enumerator") %>%
  mutate(Issue = "Wrong location of data collection",
         `Type of Issue` = "We do not collect data in GOS areas",
         Feedback = NA_character_)

deletion_log_final <- deletion_log

library(readxl)
####
# use this when we have decided to delete some forms
# Under3_file_2024_06 <- read_excel("review_feedback/Under3_file_2024-06.xlsx",
#                                   sheet = "To delete")
# df_clean <- df_clean %>%
#   left_join(Under3_file_2024_06, by = c("X_uuid"))
#
# deletion_log_C <- df_clean %>%
#   filter(decision == "delete") %>%
#   select(uuid = X_uuid, deviceid, Enumerator_id ="enumerator") %>%
#   mutate(Issue = "the locations where there is only one price/location",
#          `Type of Issue` = "We do not collect data in GOS areas",
#          Feedback = NA_character_)
#
#
# deletion_log_final <- rbind(deletion_log_A,deletion_log,deletion_log_C)

### check that all the deleted rows are deleted from the raw
df_clean_final <- df_recalculated %>%
  mutate(delete = case_when(df_clean$X_uuid %in% deletion_log_final$uuid ~ "delete",
                            TRUE ~ "keep")
  ) %>%
  filter(delete == "keep")

# organising the cleaned data -------
df_clean_final <- df_clean_final %>%
  dplyr::select(-c("organisation","organisation_other","organisation_name_label",
                   "enumerator","country_area_label","specific_location",
                   "audit",	"audit_URL",	"deviceid",
                   "specific_location",	"X_specific_location_latitude",
                   "X_specific_location_longitude",	"X_specific_location_altitude",
                   "X_specific_location_precision",

                   "SYR_note_feedback_tx", "delete"))



df_clean_final <- df_clean_final %>%
  dplyr::select(c('X_uuid'),everything()) %>%
  relocate(region,	AoI,	admin1Name_en,	admin1Name_ar,	admin2Name_en,admin2Name_ar,
           admin3Name_en,admin3Name_ar,admin4Name_en,admin4Name_ar,
           .after = X_uuid)

# Preapring the logbook  and orgnizing it---------
cleaning.log.wrong.final <- read.csv("log_outputs/Oct_2024/cleaning_log/final_cleaning.csv")
cleaning.log.wrong.final <- cleaning.log.wrong.final %>%
  dplyr::rename(uuid = X_uuid,
   question = Question) %>%
   mutate(changed.type = "change_response")

colnames(cleaning.log.wrong.final)
colnames(responses)
responses_final <- responses %>%
  relocate(old_value, new_value, .after = changed.type )

responses_final <- responses_final %>%
  mutate(delete = case_when(uuid == "ad4df46e-f8f2-40be-94a9-a98853455d67"~ "delete",
                            uuid %in% deletion_log_final$X_uuid ~ "delete",
                            TRUE ~ "keep")) %>%
  filter(delete == "keep") %>%
  select(-delete)
responses_final <- responses_final %>%
  filter(question != "country_area_label")
responses_final <- plyr::rbind.fill(responses_final)
responses_final <- responses_final %>%  dplyr::rename(Issue = issue, Old_Value = old_value,
        New_Value = new_value) %>%
  bind_rows(cleaning.log.wrong.final) %>%  select(-X)
#  creating the variable tracker varaible tracker ---------

variable_tracker <- data.frame(
  Variable = c("X_,uuid","region",	"AoI",	"admin1Name_en",	"admin1Name_ar",
               "admin2Name_en","admin2Name_ar",
               "admin3Name_en","admin3Name_ar","admin4Name_en","admin4Name_ar",
    "organisation","organisation_other","organisation_name_label",
               "enumerator","country_area_label","specific_location",
               "_specific_location_latitude","_specific_location_longitude",
               "_specific_location_altitude","_specific_location_precision",
               "SYR_note_feedback_tx"),
  Action = "",
  Rationale = ""
)


# Preparing the  data_extract ---------
df_extract <- dft %>% filter(!uuid %in% deletion_log_final$X_uuid) %>%
  select(uuid = X_uuid, Enumerator_id ="enumerator")


# saving the  files --------
check_list <- read_excel("inputs/check_list.xlsx")

ignored_questions<- questions %>%
  filter( (str_detect(type, "audit") == T  | str_detect(type, "acknowledge") == T)  &
            name %in% colnames(dft) ) %>%
  pull(name) %>%
  append(
    grep("^SYR_note_", colnames(dft), value = TRUE)
  )

df_clean_final <- df_clean_final %>% select(!any_of(ignored_questions))
# End of data cleaning:

# converting all data NES to SRP and NWS to TRY
df_clean_final_converted <- currency_conversion(df_clean_final)

jmmi_month_data <- list(
  "Raw dataset" = dft,
  "Cleaned dataset" = df_clean_final,
  "clean_df_NES_SRP_NWS_TRY" = df_clean_final_converted$Data_NES_SRP_NWS_TRY,
  "clean_df_NES_SRP"= df_clean_final_converted$Converted_NES_SRP,
  "clean_df_NWS_TRY"=df_clean_final_converted$Converted_NWS_TRY,
  "Varaible tracker"= variable_tracker,
  "Data Extract" = df_extract,
  "Logbook" = responses_final,
  "Deletion log" = deletion_log_final,
  "logical checks" = check_list
)

write.xlsx(jmmi_month_data, (file = sprintf("cleaned_data/jmmi_clean_data_%s.xlsx", today())))

######################
### after manual outlier checks
#################
##add deletion uuid
# add other log
# add manual cleaning log

#read the manual cleaing
manual_log <-read.xlsx("log_outputs/Oct_2024/cleaning_log/manual_cleaning_log.xlsx")
deletion_log_final <-read.xlsx("log_outputs/Oct_2024/cleaning_log/manual_deletion_log.xlsx")


raw <- df_clean_final %>% filter(!X_uuid %in% deletion_log_final$X_uuid)

for (r in 1:nrow(manual_log)){

  uuid <- manual_log$uuid[r]
  variable <- manual_log$question[r]
  new.value <- manual_log$new_value[r]
  manual_log$old_value[r] <- raw[raw$X_uuid == uuid, variable]
  raw[raw$X_uuid == uuid, variable] <- new.value
}

cleaned_data <- raw
######
# merge the manual test with automayic
manual_log <- manual_log %>%  dplyr::rename(Old_Value = old_value,
                                            New_Value = new_value) %>% mutate(Issue = "outlier",
                                                                              changed.type= case_when(!is.na(New_Value) ~ "change_response",
                                                                                                      is.na(New_Value) ~ "blank_response",
                                                                                                      TRUE ~ "change_response"))
manual_log <- manual_log %>%
  mutate(Old_Value = as.character(Old_Value), New_Value = as.character(New_Value))
responses_final <- bind_rows(responses_final, manual_log) %>% distinct()
# prepare the deletion log

cleaning.log.other <- read.csv("log_outputs/Oct_2024/cleaning_log/cleaning.log.others.csv") %>%
  rename(Old_Value = old.value, New_Value = new.value, question = variable, Issue = issue)

responses_final <- bind_rows(responses_final, cleaning.log.other) %>% distinct()
responses_final <- responses_final %>%  select(-X, -rel.index, -ruuid)
# add them to delete log

#convert the data again
df_clean_final_converted <- currency_conversion(raw)



jmmi_month_data <- list(
  "Raw dataset" = dft,
  "Cleaned dataset" = cleaned_data ,
  "clean_df_NES_SRP_NWS_TRY" = df_clean_final_converted$Data_NES_SRP_NWS_TRY,
  "clean_df_NES_SRP"= df_clean_final_converted$Converted_NES_SRP,
  "clean_df_NWS_TRY"=df_clean_final_converted$Converted_NWS_TRY,
  "Varaible tracker"= variable_tracker,
  "Data Extract" = df_extract,
  "Logbook" = responses_final,
  "Deletion log" = deletion_log_final,
  "logical checks" = check_list
)

write.xlsx(jmmi_month_data, (file = sprintf("cleaned_data/jmmi_clean_data_%s_after_manual_cleaning.xlsx", today())))
responses_final <- read.xlsx("cleaned_data/jmmi_clean_data_2024-10-29_after_manual_cleaning.xlsx", sheet = "Logbook")
####### compare the data with log files
compared_df <- review_cleaning(
  raw_dataset = dft,
  raw_dataset_uuid_column = "X_uuid",
  clean_dataset = cleaned_data,
  clean_dataset_uuid_column = "X_uuid",
  cleaning_log = responses_final,
  cleaning_log_uuid_column = "uuid",
  cleaning_log_change_type_column = "changed.type",
  cleaning_log_question_column = "question",
  cleaning_log_new_value_column = "New_Value",
  cleaning_log_old_value_column = "Old_Value",
  cleaning_log_no_change_value = c("no_action", "no_change"),
  deletion_log = deletion_log_final,
  deletion_log_uuid_column = "X_uuid",
  check_for_deletion_log = T
)

write.xlsx(compared_df,"compared_df.xlsx")

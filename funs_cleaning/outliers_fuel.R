
outliers_fuel_water <- function(dft){


df_counts_outliers <- df_currency %>%
  select(X_uuid, region,organisation,deviceid,
         enumerator,admin4_label,admin4Name_en,admin4Name_ar,shop_currency,
         diesel_blackmarket_price_item,
         petrol_blackmarket_price_item,
         gas_blackmarket_price_item,
         kerosene_blackmarket_price_item,
         diesel_local_subsidised_price_item,
         diesel_local_price_item,
         petrol_local_subsidised_price_item,
         petrol_local_price_item,
         diesel_imported_price_item,
         petrol_imported_price_item,
         lpg_subsidised_price_item,
         kerosene_subsidised_price_item,
         lpg_price_item,
         kerosene_price_item,
         water_truck_liter_price_unit_item)

df_nes <- df_counts_outliers %>% filter(region == "Northeast")

outliers_nes <- cleaningtools::check_outliers(dataset = df_nes, uuid_column = "X_uuid", sm_separator = "/")

outliers_nes <- cleaningtools::add_info_to_cleaning_log(
  list_of_log = outliers_nes,
  dataset = "checked_dataset",
  cleaning_log = "potential_outliers",
  dataset_uuid_column = "X_uuid",
  cleaning_log_uuid_column = "uuid",
  information_to_add = c("region","organisation","deviceid",
                         "enumerator","admin4_label","admin4Name_en","admin4Name_ar","shop_currency")
)

outliers_log_nes <- outliers_nes$cleaning_log


#'*NWS OUTLIER CHECK*

df_nws <- df_counts_outliers %>% filter(region == "Northwest")

outliers_nws <-  cleaningtools::check_outliers(dataset = df_nws, uuid_column = "X_uuid", sm_separator = "/")

outliers_nws <- cleaningtools::add_info_to_cleaning_log(
  list_of_log = outliers_nws,
  dataset = "checked_dataset",
  cleaning_log = "potential_outliers",
  dataset_uuid_column = "X_uuid",
  cleaning_log_uuid_column = "uuid",
  information_to_add = c("region","organisation","deviceid",
                         "enumerator","admin4_label","admin4Name_en","admin4Name_ar","shop_currency"))

outliers_log_nws <- outliers_nws$cleaning_log

#'*BIND OUTLIER CHECK*
potential_outliers <- rbind(outliers_log_nes,outliers_log_nws)

potential_outliers <- potential_outliers %>%
  filter(question %nin% c("index","_submission_time","_id"))

potential_outliers <- potential_outliers %>%
  filter(!is.na(old_value))

#'*Check outliers in the orginal uncoverted data using the potential_outliers df*
df <- df %>%
  mutate_at(items.list, as.numeric)


for (i in 1:nrow(potential_outliers)){

  id <- as.character(potential_outliers[i, "uuid"])
  col <- as.character(potential_outliers[i, "question"])
  new.value <- df[df$X_uuid == id, col]
  potential_outliers[potential_outliers$uuid == id & potential_outliers$question == col, "value"] <- new.value
}
# view(potential_outliers)
potential_outliers <- potential_outliers %>%  filter(value !=0)

# IMPORTANT FOR CONVERTING
# "old_converted" ="old_value" ,"old_value" = "value"

potential_outliers$old_value <- NULL #it the converted value.

potential_outliers_final <- potential_outliers %>%
  dplyr::rename("old_value" = "value")

return(potential_outliers_final)

}
table_dft <- outliers_fuel_water(df_currency)
table_dft_west <- table_dft %>% filter(region == "Northwest")
table_dft_east <- table_dft %>% filter(region == "Northeast")
# df_check_northwest_list$cleaning_log <- plyr::rbind.fill(df_check_northwest_list$cleaning_log, table_dft_west)
#
# # questions preparation for the data merge
# df_translation <- questions %>%
#   dplyr::select(`label::arabic`,name)%>%
#   na.omit() %>%
#   dplyr::rename("question" = name)

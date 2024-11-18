#'*EXCHANGE RATES USING MEDIANS OF THE EXCAHNGE RATE REPORTED FOR NORTHEAST AND NORTHWEST SYRIA*
# calculate medians of exchange rates: Turkish_lira-Syrian_pound & US_dollar-Syrian_pound for each region
# Any calculate ending with "_price_item" is the price in standard unit KG/L.
# getting the prices that standard.
# there was a mistake on the soap_bar_price_item calculation therefore we need
# to delete it and calculate it again
# soap_price_item: This should give the price based on the unit selected
# soap_bar_price_item: This should give the price in KG
#df$soap_bar_price_item <- NULL
currency_conversion <- function(df){

df <- df %>%
  dplyr::mutate(soap_bar_price_item = (as.numeric(soap_price_item) * 6.67)) %>%
  relocate(soap_bar_price_item, .after = soap_price_item)



items.list.data <- df %>% dplyr::select(ends_with("_price_item"),
                                        water_truck_liter_price_unit_item,
                                        exchange_rate_buy_usd_syp,
                                        exchange_rate_sell_usd_syp,
                                        exchange_rate_buy_usd_try,
                                        exchange_rate_sell_usd_try,
                                        internet_data_unit_item)


items.list <- names(items.list.data)

df <- df %>%
  mutate_at(items.list, as.numeric)



# we do not want to convert the exchange rates
items.list <- items.list[!str_detect(items.list,"exchange_rate_") &
                           !str_detect(items.list,"internet_data_modem_unit_item") &
                           !str_detect(items.list,"internet_data_unit_item")]




df_currency <- df %>%
  dplyr::mutate(
    exchange_rate_medians_sell = case_when(

      region == "Northeast" & shop_currency == "US_dollar" ~ median(exchange_rate_sell_usd_syp, na.rm = TRUE),
      region == "Northeast" & shop_currency == "Syrian_pound" ~ 1,
      region == "Northwest" & shop_currency == "US_dollar" ~ median(exchange_rate_sell_usd_try, na.rm = TRUE),
      region == "Northwest" & shop_currency == "Syrian_pound" ~ (1/440),
      region == "Northwest" & shop_currency == "Turkish_lira" ~ 1,
      TRUE ~ NA_real_
    )
  )

unique(df_currency$exchange_rate_medians_sell)

Currency_change <- function(df, items.list) {
  for (col.price in items.list) {
    df <- df %>%
      dplyr::mutate(!!sym(col.price) := (!!sym(col.price) * exchange_rate_medians_sell))
  }
  return(df)
}


currency.converted <- Currency_change(df_currency, items.list)



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
write.xlsx(market_monitoring_raw_data, "test.convert.xlsx")
 return(market_monitoring_raw_data)
}

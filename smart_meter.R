smart_meter_data_raw <- read_csv("data-raw/sm_raw.csv", n_max = 3e6)

sm_cust <- smart_meter_data_raw %>% 
  distinct(CUSTOMER_ID) %>%
  .$CUSTOMER_ID %>% 
  sample(size= 50)

smart_meter_data <- smart_meter_data_raw %>%
  dplyr::filter(CUSTOMER_ID %in% sm_cust) %>%
  dplyr::rename_all(tolower) %>%
  dplyr::arrange(customer_id, reading_datetime) %>%
  dplyr::group_by(customer_id) %>%
  dplyr::mutate(reading_datetime = case_when(
    duplicated(reading_datetime) ~ reading_datetime + lubridate::hours(1),
    TRUE ~ reading_datetime
  ))

smart_meter_data$customer_id <- as.character(smart_meter_data$customer_id)

sm_cust50 <- smart_meter_data %>%
  as_tsibble(index = reading_datetime, key = customer_id) %>%
  ungroup() %>%
  dplyr::select(-calendar_key)

save(sm_cust50, file = "data/sm_cust50.rds", compress = "xz")

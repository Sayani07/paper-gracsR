### This code creates a table of data for different simulations scenarios ###
library(readr)
library(dplyr)
library(tsibble)
#devtools::install_github("jimhester/archive")
library(archive)
library(tibble)

# Raw data obtained from Electricity Use Interval Reading of Smart-Grid Smart-City Customer Trial Data available in
# https://data.gov.au/dataset/ds-dga-4e21dea3-9b87-4610-94c7-15a8a77907ef/distribution/dist-dga-b71eb954-196a-4901-82fd-69b17f88521e/details?q=smart%20meter
# file name:CD_INTERVAL_READING_ALL_NO_QUOTES.csv

# if (!file.exists("data-raw/smart-metre.7z")) {
#   download.file(
#     "http://datagovau.s3-ap-southeast-2.amazonaws.com/CDINTERVALREADINGALLNOQUOTES.csv.7z",
#     "data-raw/smart-metre.7z",
#     mode = "wb"
#   )
# }
# 
# smart_meter_data_raw <- archive_read("data-raw/smart-metre.7z") %>%
#   read_csv(n_max = 3e6)


smart_meter_data_raw <-  read_csv("paper/data-raw/CD_INTERVAL_READING_ALL_NO_QUOTES-1.csv", 
                                  n_max = 3e6)

set.seed(12345)
sm_cust <- smart_meter_data_raw %>%
  distinct(CUSTOMER_ID) %>%
  pull(CUSTOMER_ID) %>%
  sample(size = 100)

sm_cust_data <- smart_meter_data_raw %>%
  filter(CUSTOMER_ID %in% sm_cust) %>%
  rename_all(tolower) %>%
  arrange(customer_id, reading_datetime) %>%
  group_by(customer_id) %>%
  mutate(
    reading_datetime = case_when(
      duplicated(reading_datetime) ~ reading_datetime + lubridate::hours(1),
      TRUE ~ reading_datetime
    )
  ) %>%
  ungroup() %>%
  mutate(customer_id = as.character(customer_id)) %>%
  as_tsibble(index = reading_datetime, key = customer_id) %>%
  select(-calendar_key) %>%
  select(
    customer_id,
    reading_datetime,
    general_supply_kwh
  )

sm_cust_data <- write_rds(sm_cust_data, "paper/data/sm_cust_data.rds")


# find possible harmonies for all the 100 households
harmonies <- sm_cust_data %>%
  harmony(
    ugran = "month",
    filter_in = "wknd_wday",
    filter_out = c("hhour", "fortnight")
  )

# capture unique harmonies through row numbers

harmonies_tbl <- harmonies %>% mutate(row = row_number())

sim_table <- expand.grid(customer_id = sm_cust, row = harmonies_tbl$row) %>% 
  left_join(harmonies_tbl, by = "row") %>% tibble() %>% arrange(customer_id)

write_csv(sim_table, here::here('paper/sim_table/sim_table.csv'))
write_csv(harmonies, here::here('paper/sim_table/harmonies.csv'))

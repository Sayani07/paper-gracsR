# script to run algo1 for distance on wkndwday for the cleaner 356 customers
#
library(tidyverse)
library(gravitas)
library(tsibble)
library(gracsr)

elec <- read_rds(here::here("data/elec_nogap_2013_clean_356cust.rds"))%>% 
                   dplyr::select(customer_id, reading_datetime, general_supply_kwh)

response = "general_supply_kwh"
gran1 = "wknd_wday"

#elec <- elec %>% filter(customer_id %in% c(8143599, 8147121, 8159803, 10542667))

# distance_data_robust <- elec %>% scale_gran(method = "robust") %>% dist_gran(gran1)
# write_rds(distance_data_robust, here::here("data/dist_gran_wkndwday_356cust_robust.rds"))

distance_data_nqt <- elec %>% scale_gran(method = "nqt") %>% dist_gran(gran1)

write_rds(distance_data_nqt, here::here("data/dist_gran_wkndwday_356cust_nqt.rds"))


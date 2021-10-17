library(gracsr)
library(tidyverse)
library(readr)
library(tsibble)
library(gravitas)


# no scaling

data_pick <- read_rds(here::here("data/elec_nogap_2013_clean_356cust.rds")) 

data_356cust_hod <- quantile_gran(data_pick,
                                  "hour_day", 
                                  quantile_prob_val = seq(0.1, 0.9, 0.1))

data_356cust_moy <- quantile_gran(data_pick,
                                  "month_year", 
                                  quantile_prob_val = seq(0.1, 0.9, 0.1))

data_356cust_wkndwday <- quantile_gran(data_pick,
                                       "wknd_wday", 
                                       quantile_prob_val = seq(0.1, 0.9, 0.1))


write_rds(data_356cust_hod, here::here("data/quantile_data_356cust_hod_noscale.rds"))
write_rds(data_356cust_moy,here::here("data/quantile_data_356cust_moy_noscale.rds"))
write_rds(data_356cust_wkndwday, here::here("data/quantile_data_356cust_wkndwday_noscale.rds"))



# robust scaling

data_pick_robust <- read_rds(here::here("data/elec_nogap_2013_clean_356cust.rds")) %>% 
  gracsr::scale_gran( method = "robust",
                      response = "general_supply_kwh")

data_356cust_hod <- quantile_gran(data_pick_robust,
              "hour_day", 
              quantile_prob_val = seq(0.1, 0.9, 0.1))

data_356cust_moy <- quantile_gran(data_pick_robust,
                                  "month_year", 
                                  quantile_prob_val = seq(0.1, 0.9, 0.1))

data_356cust_wkndwday <- quantile_gran(data_pick_robust,
                                  "wknd_wday", 
                                  quantile_prob_val = seq(0.1, 0.9, 0.1))


write_rds(data_356cust_hod, here::here("data/quantile_data_356cust_hod_robust.rds"))
write_rds(data_356cust_moy,here::here("data/quantile_data_356cust_moy_robust.rds"))
write_rds(data_356cust_wkndwday, here::here("data/quantile_data_356cust_wkndwday_robust.rds"))

# nqt scaling

data_pick_nqt <- read_rds(here::here("data/elec_nogap_2013_clean_356cust.rds")) %>% 
  gracsr::scale_gran( method = "nqt",
                      response = "general_supply_kwh")

data_356cust_hod <- quantile_gran(data_pick_nqt,
                                  "hour_day", 
                                  quantile_prob_val = seq(0.1, 0.9, 0.1))

data_356cust_moy <- quantile_gran(data_pick_nqt,
                                  "month_year", 
                                  quantile_prob_val = seq(0.1, 0.9, 0.1))

data_356cust_wkndwday <- quantile_gran(data_pick_nqt,
                                       "wknd_wday", 
                                       quantile_prob_val = seq(0.1, 0.9, 0.1))


write_rds(data_356cust_hod, here::here("data/quantile_data_356cust_hod_nqt.rds"))
write_rds(data_356cust_moy,here::here("data/quantile_data_356cust_moy_nqt.rds"))
write_rds(data_356cust_wkndwday, here::here("data/quantile_data_356cust_wkndwday_nqt.rds"))
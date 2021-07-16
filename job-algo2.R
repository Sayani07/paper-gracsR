library(knitr)
library(tidyverse)
library(lubridate)
library(lvplot)
#library(ggridges)
library(tsibble)
library(gravitas)
library(ggpubr)
library(readr)
library(kableExtra)
library(distributional)
library(ggplot2)
library(sugrrants)
library(here)
library(ggplot2)
library(patchwork)
library(scales)
library(GGally)
library(viridis)


ncust = 100 # number of sampled customers
nperm = 2 # number of permutations for normalization
nsamp = 2 # number of samples for threshold

# take customers who do not have gaps in their data
# elec_nogap <- read_rds("data/elec_nogap.rds")
# 
# elec_nogap_2013 <- elec_nogap %>%
#   filter(year(reading_datetime)==2013)
#   
# write_rds(elec_nogap_2013, "data/elec_nogap_2013.rds")

# elec_nogap_2013 <- read_rds("data/elec_nogap_2013.rds")

# just take 100 customers from them
# sm_100 <- elec_nogap_2013 %>%
#   as_tibble() %>%
#   distinct(customer_id) %>%
#   slice_sample(n = ncust)
# 
# 
# # take data for those customers only for 2013
# elec <- elec_nogap_2013 %>%
#   filter(customer_id %in% sm_100$customer_id) %>%
#   ungroup()
# 
# write_rds(elec, "data/elec_nogap_2013_100.rds")

elec <- read_rds("data/elec_nogap_2013_100.rds")
cust_id <- length(unique(elec$customer_id))
#scen <- as.numeric(commandArgs()[[6]])
scen <- 2
custj <- cust_id[scen]
# consider harmonies which are generally significant for electricity data

harmonies <- read_rds("../paper-hakear/paper/data/harmonies.rds")

# harmonies <- harmonies %>% 
#   mutate(comb = paste(facet_variable, 
#                              x_variable, 
#                              sep = "-")) %>% 
#   filter(comb %in% c("hour_day-wknd_wday", "day_month-hour_day", "wknd_wday-hour_day", "hour_day-day_week", "day_week-hour_day")) %>% 
#   select(-comb)
# 
# write_rds(harmonies, "data/harmonies.rds")

harmonies <- read_rds("data/harmonies.rds")

# take customers who do not have gaps in their data
  
  data_id <-  elec %>% 
    filter(customer_id %in% custj) %>% 
    as_tsibble(index = reading_datetime)
  
  cust <- (data_id$customer_id) %>% unique()
  
  elec_select_harmony = hakear::select_harmonies(data_id,
                               harmony_tbl = harmonies,
                               response = general_supply_kwh,
                               nperm = nperm,
                               nsamp = nsamp
  ) %>% mutate(customer_id = cust)
  write_rds(elec_select_harmony, paste0("data/elec_harmony-",custj,"-nogap.rds"))


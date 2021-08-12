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

# take customers who do not have gaps in their data
elec_nogap <- read_rds("data/elec_nogap.rds")

set.seed(12345)

# just take 100 customers from them
sm_100 <- elec_nogap %>%
  as_tibble() %>% 
  distinct(customer_id) %>%
  slice_sample(n = 100)

# elec <- elec_ts %>%
#   as_tibble() %>% 
#   distinct(customer_id) %>%
#   slice_sample(n = 2)

# take data for those customers only for 2013
elec <- elec_nogap %>% 
  filter(customer_id %in% sm_100$customer_id) %>% 
  ungroup() %>% 
  filter(year(reading_datetime)==2013)

# consider harmonies which are generally significant for 

harmonies <- read_rds("../paper-hakear/paper/data/harmonies.rds")

harmonies_acro <- harmonies %>% 
  mutate(facet_variable = case_when(
    facet_variable == "hour_day" ~ "hod" ,
    facet_variable == "day_month" ~ "dom" ,
    facet_variable == "day_week" ~ "dow" ,
    facet_variable == "week_month" ~ "wom" ,
    facet_variable == "wknd_wday" ~ "wdwnd"
  )) %>% 
  mutate(x_variable = case_when(
    x_variable == "hour_day" ~ "hod" ,
    x_variable == "day_month" ~ "dom" ,
    x_variable == "day_week" ~ "dow" ,
    x_variable == "week_month" ~ "wom" ,
    x_variable == "wknd_wday" ~ "wdwnd"
  )) %>% mutate(comb = paste(facet_variable, x_variable, sep = "-")) %>% 
  filter(comb %in% c("hod-wdwnd", "dom-hod", "wdwnd-hod", "hod-dow", "dow-hod")) %>% 
  select(-comb)

# take customers who do not have gaps in their data

library(tictoc)
#tic()
elec_split = elec %>% group_split(customer_id)

harmonies_acro <- harmonies %>% mutate(facet_variable=NA, facet_levels=NA) %>% distinct()

elec_select_harmony = parallel::mclapply(1:100, function(x){
  
  data_id <-  elec_split %>% magrittr::extract2(x) %>% 
    as_tsibble(index = reading_datetime)

  
  k = hakear::select_harmonies(data_id,
                           harmony_tbl = harmonies_acro,
                           response = general_supply_kwh,
                           nperm = 200,
                           nsamp = 2
  ) %>% mutate(customer_id = unique(data_id$customer_id))
  #write_rds(k, paste("data/elec_harmony-100-nogap.rds"))
}, mc.cores = parallel::detectCores() - 1, mc.preschedule = FALSE, mc.set.seed = FALSE)
#toc()
elec_harmny <- elec_select_harmony %>% bind_rows() 
write_rds(elec_select_harmony, paste("data/elec_harmony-100-nogap.rds"))
# write_rds(elec_harmny, paste("data/elec_harmony-100-nogap-onegran.rds"))

# ## ---- elec_select_harmony-8
# elec_harmony_all <- elec_select_harmony %>% 
#   bind_rows(.id = "id") %>% 
#   mutate(facet_variable = case_when(
#     facet_variable == "hour_day" ~ "hod" ,
#     facet_variable == "day_month" ~ "dom" ,
#     facet_variable == "day_week" ~ "dow" ,
#     facet_variable == "week_month" ~ "wom" ,
#     facet_variable == "wknd_wday" ~ "wdwnd"
#   )) %>% 
#   mutate(x_variable = case_when(
#     x_variable == "hour_day" ~ "hod" ,
#     x_variable == "day_month" ~ "dom" ,
#     x_variable == "day_week" ~ "dow" ,
#     x_variable == "week_month" ~ "wom" ,
#     x_variable == "wknd_wday" ~ "wdwnd"
#   )) %>% 
#   mutate(id = paste("id", id, sep = " ")) %>% 
#   group_by(id) %>% 
#   mutate(rank = row_number())




#data <- read_rds("data/elec_harmony_100.rds")


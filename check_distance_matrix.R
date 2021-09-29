library(gravitas)
library(tidyverse)
library(gracsr)
library(tsibble)

sm <- smart_meter10 %>%
filter(customer_id %in% c("10006704", "10017936","10006414", "10018250"))
gran1 = "hour_day"
gran2 = NULL
response = "general_supply_kwh"
dist_gran(sm, "hour_day")
dist_gran(sm, "month_year")
dist_mat <- sm %>% scale_gran(method = "robust") %>% dist_gran("hour_day")

dist_mat %>% labels()

dist_mat %>% clust_gran()


groups  <-  dist_mat %>% hclust
cutree(groups, k = 2) %>% labels

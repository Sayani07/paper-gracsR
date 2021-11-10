# script to remove customers from 600 customers in "data/elec_nogap_2013_600.rds" to get a cleaner data set
# Remove customers for which moy is not a harmony, that is, there are some levels for which observations are missing
# Remove customers for which the quantiles are of same value (to avoid problems in JS computation)


library(tidyverse)
library(gravitas)
library(tsibble)
elec <- read_rds(here::here("data/elec_nogap_2013_600.rds"))%>% 
                   dplyr::select(customer_id, reading_datetime, general_supply_kwh)

# remove customers whose iqr is zero
elec_iqr0 <- elec %>% as_tibble() %>% group_by(customer_id) %>%
  summarise(q2 = stats::median(general_supply_kwh, na.rm = TRUE),
            iqr = stats::IQR(general_supply_kwh, na.rm = TRUE),
            quantile = stats::quantile(general_supply_kwh, 
                  na.rm = TRUE, prob = seq(0.1, 0.9, 0.1))) %>% 
  filter(iqr==0)

elec_sub <- elec %>%
  filter(!(customer_id %in%
             elec_iqr0$customer_id))

# Iteration 1

.data = elec_sub

gran1 = "month_year"
response = "general_supply_kwh"

key = "customer_id"
gran2 =  NULL
quantile_prob_val =  seq(0.1, 0.9, 0.1)


if(is.null(gran2)){
  sm_gran <- .data %>%
    create_gran(gran1) %>%
    as_tibble() %>%
    select(key,
           response,
           {{gran1}})
  
}

if(!is.null(gran2)){
  sm_gran <- .data %>%
    create_gran(gran1) %>%
    create_gran(gran2) %>%
    as_tibble() %>%
    select(key,
           response,
           {{gran1}},
           {{gran2}})
}


data <- unite(sm_gran, category, -c(1, 2), sep = "-")

# category reference
uni_cat <- unique(data$category)
category_ref <-tibble(category_id = seq(uni_cat),
                      category = uni_cat)



sm_list <- data %>%
  select(key, category, response) %>%
  pivot_wider(names_from = category,
              values_from = response, values_fn = list)


cust = sm_list$customer_id %>% unique
nsm = sm_list[-1] %>% ncol()


sm_is_null <- map(seq_len(length(cust)), function(x){
  map(seq_len(nsm), function(y){
    data = sm_list %>%
      filter(customer_id==cust[x])
    data[-1][y] %>% unlist %>% is.null() |
      length(unique(quantile(unlist(data[-1][y]), probs = quantile_prob_val)))==1
  }) %>% bind_cols()
}) %>% bind_rows()


sm_is_null_rs <- sm_is_null %>% as_tibble() %>%  rowSums()

# Remove customers for which moy is not a harmony, that is, there are some levels for which observations are missing
# Remove customers for which the quantiles are of same value (to avoid problems in JS computation)

sm_choose_cust <- sm_is_null %>% as_tibble() %>% bind_cols(customer_id = sm_list$customer_id, sum  = sm_is_null_rs) %>% filter(sum==0)

elec_sub <- elec %>%
  filter((customer_id %in%
            sm_choose_cust$customer_id))

write_rds(elec_sub, here::here("data/elec_nogap_2013_clean_356cust.rds"))


# script to assimilate results from 02_algo1_moy_356cust_robust.R, 02_algo1_hod_356cust_robust.R and 02_algo1_wkndwday_356cust_robust.R
# write code after running the results for each of the above mentioned code

library(tidyverse)
library(gravitas)
library(tsibble)
library(ggpubr)
library(readr)
library(plotly)

#----iteration-1

elec_356_raw <- read_rds(here::here("data/elec_nogap_2013_clean_356cust.rds"))

elec_600_wpd <- read_rds(here::here("data/algo2-cust600-wpd-rawdata.rds"))

elec_pick <- elec_600_wpd %>% 
  filter(customer_id %in% elec_356_raw$customer_id)

elec_pick_wide <- elec_pick %>% pivot_wider(-c(1, 2), names_from = "x_variable", values_from = wpd)

robust_scale <-  function(x){
  (x - stats::median(x))/(stats::IQR(x))
}

elec_pick_wide_scaled <- apply(elec_pick_wide[-1], 2, robust_scale)


mds <- elec_pick_wide_scaled %>% dist() %>% cmdscale() %>% as_tibble()

colnames(mds) <- c("Dim.1", "Dim.2")

ggscatter(mds, 
          x = "Dim.1",
          y = "Dim.2", 
          #label = elec_pick_wide$customer_id,
          size = 1,
          repel = TRUE)


fig <- plot_ly(mds, x = ~Dim.1, y = ~ Dim.2, text = elec_pick_wide$customer_id)

# Outliers list after loooking at the plot

# (8147121, 8568565, 8283422, 9393476, 10542667)


#----Iteration (2 after removing outliers)

elec_raw <- read_rds(here::here("data/elec_nogap_2013_clean_356cust.rds")) %>% 
  filter(!(customer_id %in% c(8147121, 8568565, 8283422, 9393476, 10542667)
))

elec_600_wpd <- read_rds(here::here("data/algo2-cust600-wpd-rawdata.rds"))

elec_pick <- elec_600_wpd %>% 
  filter(customer_id %in% elec_raw$customer_id)

elec_pick_wide <- elec_pick %>% pivot_wider(-c(1, 2), names_from = "x_variable", values_from = wpd)

robust_scale <-  function(x){
  (x - stats::median(x))/(stats::IQR(x))
}

elec_pick_wide_scaled <- apply(elec_pick_wide[-1], 2, robust_scale)


mds <- elec_pick_wide_scaled %>% dist() %>% cmdscale() %>% as_tibble()

colnames(mds) <- c("Dim.1", "Dim.2")

ggscatter(mds, 
          x = "Dim.1",
          y = "Dim.2", 
          #label = elec_pick_wide$customer_id,
          size = 1,
          repel = TRUE)


fig <- plot_ly(mds, x = ~Dim.1, y = ~ Dim.2, text = elec_pick_wide$customer_id)

## CLUSTER CHARACTERIZATION

quantile_prob_graph <- c(0.25, 0.5, 0.75)


rownames(elec_pick_wide_scaled) = elec_pick_wide$customer_id

dist_tibble <- elec_pick_wide_scaled %>% dist() %>% broom::tidy()



  data_pick_one <- dist_tibble %>%
    filter(item1 %in% 
                                               c(8952846)) %>% group_by(item1) %>% arrange(distance) %>% head(5) %>% 
  mutate(item1 = as.integer(as.character(item1)),
         item2 = as.integer(as.character(item2)))

  # 8633839
  data_pick_two <- dist_tibble %>%
    filter(item1 %in% 
             c(8477261)) %>% group_by(item1) %>% arrange(distance) %>% head(5) %>% 
    mutate(item1 = as.integer(as.character(item1)),
           item2 = as.integer(as.character(item2)))
  
  data_pick_three <- dist_tibble %>%
    filter(item1 %in% 
             c(9070930)) %>% group_by(item1) %>% arrange(distance) %>% head(5) %>% 
    mutate(item1 = as.integer(as.character(item1)),
           item2 = as.integer(as.character(item2)))
  
data_pick_cust <- bind_rows(
    unique(c(data_pick_one$item1,data_pick_one$item2)) %>% as_tibble(),    unique(c(data_pick_two$item1,data_pick_two$item2)) %>% as_tibble(), unique(c(data_pick_three$item1,data_pick_three$item2)) %>% as_tibble(), .id = "design")
  
data_pick <- read_rds(here::here("data/elec_nogap_2013_clean_356cust.rds"))%>%
  filter(customer_id %in% data_pick_cust$value) 

sm_hod <- gravitas::create_gran(data_pick, "hour_day")

sm_hod_list <- sm_hod %>% 
  as_tibble() %>% 
  select(customer_id, hour_day, general_supply_kwh) %>% 
  pivot_wider(names_from = hour_day,
              values_from = general_supply_kwh)  %>% 
  left_join(data_pick_cust, by = c("customer_id" = "value"))

ncol_sm <- seq_len(ncol(sm_hod_list[-c(1, ncol(sm_hod_list))]))
nrow_sm <- seq_len(nrow(sm_hod_list))


sm_hod_quantiles_cat <- map(nrow_sm, function(x){
  map(ncol_sm, function(y){
    cell <- sm_hod_list%>% 
      dplyr::filter(customer_id == customer_id[x]) %>% 
      select(-c(1, ncol(sm_hod_list))) %>% 
      extract(y) %>% 
      unlist()
    quantile(cell, prob = quantile_prob_graph, na.rm = TRUE)
  })  %>% bind_rows(.id = "categories_serial_id")
}) %>% bind_rows(.id = "customer_serial_id")


ref_cat <- names(sm_hod_list)[-c(1, ncol(sm_hod_list))] %>% as_tibble() %>% set_names("category") %>%  
  mutate(categories_serial_id = row_number())


ref_cust <- sm_hod_list$customer_id %>% as_tibble()%>% set_names("customer_id")  %>% 
  mutate(customer_serial_id = row_number())

sm_quantiles_ref <- sm_hod_quantiles_cat %>% 
  mutate(customer_serial_id = as.integer(customer_serial_id),
         categories_serial_id = as.integer(categories_serial_id)) %>%
  left_join(ref_cat, by = "categories_serial_id") %>% 
  left_join(ref_cust, by = "customer_serial_id") %>% 
  select(customer_serial_id, categories_serial_id, category, customer_id, everything())

data_heatmap <- sm_quantiles_ref %>% 
  left_join(data_pick %>% distinct(customer_id)) %>% 
  left_join(data_pick_cust, by = c("customer_id" = "value"))


data_heatmap %>% 
  ggplot(aes(x = categories_serial_id)) + 
  geom_ribbon(aes(ymin = `25%`, 
                  ymax = `75%`), fill = "lightblue") +
  geom_line(aes(y = `50%`), size = 0.7) +
  facet_wrap(design~customer_id, 
             scales = "free_y", 
             labeller = "label_value",
             ncol = 6) +
  theme_bw() +
  theme(strip.text = element_text(size = 10, margin = margin(b = 0, t = 0))) + xlab("hour-of-day") + ylab("demand (in Kwh)")



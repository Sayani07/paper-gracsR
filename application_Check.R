library(gracsr)
library(tidyverse)
library(readr)
library(tsibble)
library(gravitas)


data_356cust_hod <- read_rds("data/quantile_data_356cust_hod_robust.rds") %>% 
  filter(quantiles %in% "50%")

data_356cust_moy <- read_rds("data/quantile_data_356cust_moy_robust.rds") %>% 
  filter(quantiles %in% "50%")

data_356cust_wkndwday <- read_rds("data/quantile_data_356cust_wkndwday_robust.rds") %>% 
  filter(quantiles %in% "50%")


data_356cust_hod_wide <- data_356cust_hod %>%
  pivot_wider(names_from = c("gran", "category", "quantiles"),
              values_from = "quantiles_values")

data_356cust_moy_wide <- data_356cust_moy %>%
  pivot_wider(names_from = c("gran", "category", "quantiles"),
              values_from = "quantiles_values")

data_356cust_wkndwday_wide <- data_356cust_wkndwday %>%
  pivot_wider(names_from = c("gran", "category", "quantiles"),
              values_from = "quantiles_values")

data_356cust_wide <- left_join(data_356cust_hod_wide,
                               data_356cust_moy_wide, by="customer_id") %>% 
  left_join(data_356cust_wkndwday_wide,  by="customer_id"
  )


data_356cust_pc <- prcomp(data_356cust_wide[,-1],
                          center = FALSE, scale = FALSE, retx = TRUE)

plot(data_356cust_pc, type="l", npcs=50)

data_24cust_wide <- data_356cust_wide %>% filter(customer_id %in% data_pick$customer_id)

set.seed(2935)

tSNE_fit <- data_24cust_wide%>% 
  select(-customer_id) %>% 
  Rtsne(pca = FALSE, perplexity = 2)


tsne_df <- data.frame(tsneX = tSNE_fit$Y[, 1], 
                      tsneY = tSNE_fit$Y[, 2], 
                      customer_id = as.character(data_24cust_wide$customer_id))

tsne_xy <- ggplot(tsne_df, aes(x = tsneX, y = tsneY)) +
  geom_point(aes(text = customer_id), size =2) +
  #scale_color_manual(values = limn_pal_tableau10()) +
  scale_colour_viridis_d(direction = -1) +
  guides(color = FALSE) +
  labs(caption = "tSNE") +
  theme(aspect.ratio = 1) +
  theme_light()+ coord_fixed(ratio=1)

tsne_xy

library(gracsr)
library(tidyverse)
library(readr)
library(tsibble)
library(gravitas)



data_356cust_hod <- read_rds("data/quantile_data_356cust_hod_nqt.rds") %>% 
  filter(quantiles %in% "50%")

data_356cust_moy <- read_rds("data/quantile_data_356cust_moy_nqt.rds") %>% 
  filter(quantiles %in% "50%")

data_356cust_wkndwday <- read_rds("data/quantile_data_356cust_wkndwday_nqt.rds") %>% 
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

save(data_356cust_wide, file="data/data_356cust_wide_nqt.rda")

# cluster_result <- cluster_result %>% mutate(customer_id = as.integer(id))

# data_356cust_wide_group <- data_356cust_wide %>% 
#   left_join(cluster_result, by = ("customer_id")) %>% 
#   mutate(group = if_else(is.na(group), 10L, group))


# data_356cust_wide_group$group <- factor(data_356cust_wide_group$group)

# without scaling

data_356cust_pc <- prcomp(data_356cust_wide[,-1],
                          center = FALSE, scale = FALSE, retx = TRUE)

plot(data_356cust_pc, type="l", npcs=50)

# from scree-plot it looks like first 10 PCs explain most of the
# variation


library(liminal)

set.seed(2935)
data_356cust_pc10 <- as_tibble(data_356cust_pc$x[,1:10])
# %>%   mutate(group = data_356cust_wide_group$group)
limn_tour(data_356cust_pc10, PC1:PC10)

sort(abs(data_356cust_pc$rotation[,1]))

##---- t-SNE embeddings

library(Rtsne)
set.seed(2099)
tSNE_fit <- data_356cust_wide%>% 
  select(-customer_id) %>% 
  Rtsne(PCA = FALSE,
    Y_init = clamp_sd(as.matrix(dplyr::select(data_356cust_pc10, PC1, PC2)), sd = 1e-4),
         perplexity = 30)


# data_pick_one <- c(8618759, 8291696, 10357256, 8290374) %>% as_tibble 
# data_pick_two <- c(9044864, 8642053, 10534367, 9021526,11162275) %>% as_tibble
# data_pick_three <- c(8221762, 8273636, 10359424, 8232822)%>% as_tibble
# 
# 
# #data_pick_four <- c(10590714,8495194,8589936, 8454235) %>% as_tibble
# 
# 
# data_pick_one <- c(8541744, 9355808, 8603880, 8619309, 10542667) %>% as_tibble 
# data_pick_two <- c(8688242, 8643837, 8184707, 10534355, 8684420) %>% as_tibble
# data_pick_three <- c(9792072, 8589936, 8454235, 10692366, 8603828)%>% as_tibble


data_pick_one <- c(8541744, 9355808, 8603880, 8619309, 10542667) %>% as_tibble %>% set_names("customer_id")
data_pick_two <- c(8688242, 8643837, 8184707, 10534355, 8684420) %>% as_tibble%>% set_names("customer_id")
data_pick_three <- c(9792072, 8589936, 8454235, 10692366, 8603828)%>% as_tibble%>% set_names("customer_id")
data_pick_four <- c(8618759, 8291696, 10357256, 8290374) %>% as_tibble %>% set_names("customer_id")
data_pick_five <- c(9044864, 8642053, 10534367, 9021526,11162275) %>% as_tibble %>% set_names("customer_id")
data_pick_six <- c(8221762, 8273636, 10359424, 8232822)%>% as_tibble %>% set_names("customer_id")

data_pick_cust <- bind_rows(
  data_pick_one, data_pick_two, data_pick_three,data_pick_four,data_pick_five, data_pick_six,
  .id = "design") %>% 
  mutate(customer_id = as.character(customer_id))

# data_pick_cust <- bind_rows(
#   data_pick_one, data_pick_two, data_pick_three,
#   .id = "design")


tsne_df <- data.frame(tsneX = tSNE_fit$Y[, 1], 
                      tsneY = tSNE_fit$Y[, 2],
                      customer_id = as.character(data_356cust_wide$customer_id)) %>% left_join(
  data_pick_cust, by = c("customer_id"
  )) %>% mutate(design = if_else(is.na(design), "0", design))

#rownames(tsne_df) <- data_356cust_wide$customer_id

rownames(data_356cust_pc10) <- data_356cust_wide$customer_id

## ----tsne-xy--------------------------------------------------------
tsne_xy <- ggplot(tsne_df, aes(x = tsneX, y = tsneY, color = design)) +
  geom_point(aes(text = customer_id)) +
  #scale_color_manual(values = limn_pal_tableau10()) +
  scale_colour_viridis_d(direction = -1) +
  guides(color = FALSE) +
  labs(caption = "tSNE") +
  theme(aspect.ratio = 1) +
  theme_light()

## ----highlight-customer--------------------------------------------------------

library(plotly)
tsne_plotly <- tsne_xy %>% ggplotly(tooltip = "text")


## ----tour----------------------------------------------------------

a <- limn_tour_link(
  tsne_df[,1:2],
  data_356cust_pc10,
  cols = PC1:PC10
)



# +
#   stat_ellipse(data=tSNE.plot,
#                geom="polygon",
#                aes(x=tSNE1,y=tSNE2),
#                    #group=customer_id,
#                    #fill=customer_id),
#                alpha=0.5,
#                lty="dashed",
#                color="black",
#                key_glyph="blank")+
#   theme_bw()

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
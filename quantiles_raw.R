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

# data_356cust_hod <- quantile_gran(data_pick_robust,
#               "hour_day", 
#               quantile_prob_val = seq(0.1, 0.9, 0.1))
# 
# data_356cust_moy <- quantile_gran(data_pick_robust,
#                                   "month_year", 
#                                   quantile_prob_val = seq(0.1, 0.9, 0.1))
# 
# data_356cust_wkndwday <- quantile_gran(data_pick_robust,
#                                   "wknd_wday", 
#                                   quantile_prob_val = seq(0.1, 0.9, 0.1))
# 

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

save(data_356cust_wide, file="data/data_356cust_wide.rda")

# without scaling

data_356cust_pc <- prcomp(data_356cust_wide[,-1],
                          center = FALSE, scale = FALSE, retx = TRUE)

plot(data_356cust_pc, type="l", npcs=50)

library(liminal)
data_356cust_pc10 <- as_tibble(data_356cust_pc$x[,1:10])
limn_tour(data_356cust_pc10, PC1:PC5)

sort(abs(data_356cust_pc$rotation[,1]))

abs(data_356cust_pc$rotation[,1]) %>% hist
#abs(bas6[,1]) %>% hist

##---- with scaling

# data_356cust_pc <- prcomp(data_356cust_wide[,-1],
#                           center = FALSE, scale = TRUE, retx = TRUE)
# 
# plot(data_356cust_pc, type="l", npcs=50)
# 
# library(liminal)
# data_356cust_pc10 <- as_tibble(data_356cust_pc$x[,1:10])
# limn_tour(data_356cust_pc10, PC1:PC10)
# 
# sort(abs(data_356cust_pc$rotation[,1]))


##---- t-SNE embeddings

library(Rtsne)
tSNE_fit<-data_356cust_wide%>% 
  select(-customer_id) %>% 
  Rtsne()


tSNE_fit$Y %>% 
  as.data.frame() %>% 
  rename(tSNE1="V1",
         tSNE2="V2") %>% 
  mutate(customer_id=as.character(data_356cust_wide$customer_id)) -> tSNE.plot

library(ggplot2)
ggplot()+
  geom_point(data=tSNE.plot,
             aes(x=tSNE1,y=tSNE2))


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
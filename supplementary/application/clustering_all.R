library(readr)
library(dplyr)
library(here)
library(tidyr)
library(tsibble)
library(gracsr)
library(gravitas)


# Read the nqt distances

wkndwday <- read_rds(here("data/dist_gran_wkndwday_356cust_nqt.rds")) %>% broom::tidy()

moy <- read_rds(here("data/dist_gran_moy_356cust_nqt.rds")) %>% broom::tidy()

hod <- read_rds(here("data/dist_gran_hod_356cust_nqt.rds")) %>% broom::tidy()


# Make the distance metrics

distance <- wkndwday %>% 
  left_join(moy, by = c("item1", "item2")) %>% 
  left_join(hod, by = c("item1", "item2")) %>% 
  rename("wkndwday" ="distance.x",
         "moy" = "distance.y",
         "hod" = "distance") %>%
  mutate(item1 = as.integer(as.character(item1)),
         item2 = as.integer(as.character(item2))) 

filtered_distance <- distance %>%
  filter(!(item1 %in% c(8196183, 8508008, 8680538))) %>% 
  filter(!(item2 %in% c(8196183, 8508008, 8680538)))

total_distance <- filtered_distance %>% 
  mutate(total = wkndwday/2 + moy/12 + hod/24) 


total_distance_wide <- total_distance %>% pivot_wider(-c(2:5), 
                                                      names_from = item2,
                                                      values_from = total)


rownames(total_distance_wide) <- total_distance_wide$item1

mds_data <- total_distance_wide %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  tibble::rownames_to_column() %>%  
  select(-item1) %>% 
  pivot_longer(-rowname) %>% 
  pivot_wider(names_from=rowname, values_from=value) 

rownames(mds_data) <- total_distance_wide$item1

df <- mds_data[-1] %>% as.matrix()
DM <- matrix(0, ncol(mds_data), ncol(mds_data))
DM[lower.tri(DM)] = df[lower.tri(df, diag=TRUE)] # distance metric
f = as.dist(DM)



first_lot <- mds_data %>% names()

id <- c(first_lot[-1], mds_data$name[nrow(mds_data)])


# Find optimal number of clusters

library(fpc)
library(cluster)
k = array()
for(i in 5:50)
{
  group <- f %>% hclust (method = "ward.D") %>% cutree(k=i)
  p <- cluster.stats(f, clustering = group, silhouette = TRUE)
  k[i]=p$sindex
}

ggplot(k %>% as_tibble %>% mutate(k = row_number()), aes(x=k, y = value)) + geom_line() + scale_x_continuous(breaks = seq(2, 50, 2))


plot(k, type = "l")
# 6 coming as the number of clusters with maximum silwidth

group <- f %>% hclust (method = "ward.D") %>% cutree(k=17)
cluster_result <- bind_cols(customer_id = id, group = group) 

cluster_result %>% group_by(group) %>% count()

# Characterise them

data_pick <- read_rds(here::here("data/elec_nogap_2013_clean_356cust.rds"))%>%
  filter(!(customer_id %in% c(8196183, 8508008, 8680538))) %>% 
  gracsr::scale_gran( method = "robust", response = "general_supply_kwh")

data_group <- data_pick %>% 
  mutate(customer_id = as.character(customer_id)) %>% 
  left_join(cluster_result, by = c("customer_id"))

## hod
data_heatmap_hod_group <- quantile_gran(data_group,
                                        gran1="hour_day",
                                        quantile_prob_val = c(0.1, 0.25, 0.5, 0.75, 0.9),
                                        group="group") %>% 
  pivot_wider(names_from = quantiles, 
              values_from = quantiles_values) 


data_heatmap_hod_group$category <- factor(data_heatmap_hod_group$category, levels = 0:23)

data_heatmap_hod_group %>% 
  ggplot(aes(x = category)) + 
  geom_ribbon(aes(ymin = `25%`, 
                  ymax = `75%`, 
                  group=group,
                  fill = as.factor(group))) +
  geom_ribbon(aes(ymin = `10%`, 
                  ymax = `90%`,
                  group=group,
                  fill = as.factor(group)), alpha = 0.5) + 
  geom_line(aes(y = `50%`,
                group=group)) +
  facet_wrap(~group, 
             scales = "free_y", 
             labeller = "label_value",
             ncol = 17) +
  theme(strip.text = element_text(size = 10, margin = margin(b = 0, t = 0))) +
  xlab("hour-of-day") + ylab("demand (in Kwh)") + 
  theme_bw() + theme(panel.spacing =unit(0, "lines")) +
  theme(axis.text.x = element_text(angle=90, hjust=1, size = 7)) +
  scale_x_discrete(breaks = seq(1, 24, 3))+ theme(
    strip.text = element_text(size = 8, margin = margin(b = 0, t = 0)))+
  theme(plot.margin = margin(0, 0, 0, 0, "cm") ) + theme(legend.position = "bottom") +
  scale_fill_manual(values=as.vector(polychrome(17)))+
  theme_application() +theme(legend.position = "bottom")



data_heatmap_moy_group <- quantile_gran(data_group,
                                        gran1="month_year",
                                        quantile_prob_val = c(0.1, 0.25, 0.5, 0.75, 0.9),
                                        group="group") %>% 
  pivot_wider(names_from = quantiles, values_from = quantiles_values) 

data_heatmap_moy_group$category <- factor(data_heatmap_moy_group$category, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

data_heatmap_moy_group %>% 
  ggplot(aes(x = category)) + 
  geom_ribbon(aes(ymin = `25%`, 
                  ymax = `75%`, group=group, fill = as.factor(group))) +  geom_ribbon(aes(ymin = `10%`, 
                                                                                          ymax = `90%`, group=group, fill = as.factor(group)), alpha = 0.5) + 
  geom_line(aes(y = `50%`, group=group)) +
  facet_wrap(~group, 
             scales = "free_y", 
             labeller = "label_value",
             ncol = 17) +
  theme(strip.text = element_text(size = 10, margin = margin(b = 0, t = 0))) + xlab("month-of-year") + ylab("demand (in Kwh)") + theme_bw() + theme(panel.spacing =unit(0, "lines")) + theme(axis.text.x = element_text(angle=90, hjust=1, size = 7)) + theme(
    strip.text = element_text(size = 8, margin = margin(b = 0, t = 0)))+
  theme(plot.margin = margin(0, 0, 0, 0, "cm") ) +
  scale_fill_manual(values=as.vector(polychrome(17)))+
  theme_application() +theme(legend.position = "bottom")


data_heatmap_wkndwday_group <- quantile_gran(data_group,
                                             gran1="wknd_wday",
                                             quantile_prob_val = c(0.1, 0.25, 0.5, 0.75, 0.9),
                                             group="group") %>% 
  pivot_wider(names_from = quantiles, values_from = quantiles_values) 

data_heatmap_wkndwday_group$category <- factor(data_heatmap_wkndwday_group$category, levels = c("Weekday", "Weekend"))


data_wkndwday <- data_pick  %>%
  mutate(customer_id = as.character(customer_id)) %>% 
  left_join(cluster_result, by = "customer_id")%>% 
  create_gran("wknd_wday")  %>% 
  create_gran("hour_day")


ylim1 = boxplot.stats(data_wkndwday$general_supply_kwh)$stats[c(1, 5)]

data_wkndwday%>% 
  ggplot(aes(x=wknd_wday, y = general_supply_kwh)) +
  #lvplot::geom_lv(aes(fill = as.factor(design), 
  #                   color = as.factor(design)), k=5, alpha = 0.5) +
  geom_boxplot(aes(fill = as.factor(group), color = as.factor(group)),alpha = 0.5)+
  coord_cartesian(ylim = ylim1*1.05)+
  facet_wrap(~group, 
             scales = "free_y", 
             labeller = "label_value") +
  theme(strip.text = element_text(size = 10, margin = margin(b = 0, t = 0))) + xlab("wknd_wday") + ylab("demand (in Kwh)") + theme_bw() + theme(panel.spacing =unit(0, "lines")) + theme(axis.text.x = element_text(angle=90, hjust=1, size = 7)) + theme(
    strip.text = element_text(size = 8, margin = margin(b = 0, t = 0)))+
  theme(plot.margin = margin(0, 0, 0, 0, "cm") ) +
  scale_fill_manual(values=as.vector(polychrome(17)))+
  theme_application() +theme(legend.position = "bottom")


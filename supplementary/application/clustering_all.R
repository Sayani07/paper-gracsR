library(readr)
library(dplyr)
library(here)
library(tidyr)
library(tsibble)
library(gracsr)
library(gravitas)
library(ggplot2)
library(ggplot2)
library(pals)


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


# contribution


data_validation <- hod %>%  
  rename("hod" = "distance") %>% 
  left_join(moy, 
            by = c("item1", "item2"))%>% 
  rename("moy" = "distance") %>% 
  left_join((wkndwday), 
            by = c("item1", "item2"))%>% 
  rename("wkndwday" = "distance") %>% 
  left_join(cluster_result, by = c("item1" = "customer_id")) %>% 
  rename("group_item1" = "group") %>% 
  left_join(cluster_result, by = c("item2" = "customer_id")) %>%  
  rename("group_item2" = "group") %>% 
  mutate(hod = hod/24, moy = moy/12, wkndwday = wkndwday/2) %>% 
  filter(!is.na(group_item1)| !is.na(group_item2)) %>% 
  pivot_longer(3:5,names_to="gran",
               values_to = "distance")  
  

data_validation %>%
  group_by(gran) %>%
  summarise(d = sd(distance))

data_validation %>%
  group_by(gran, group_item1, group_item2) %>% 
  summarise(sum = sum(distance)) %>% 
  pivot_wider(names_from = group_item2, values_from = sum) %>% 
  mutate(distance = sum(`1`, `2`, `3`, `4`, `5`,`6`, `7`, `8`, `9`, `10`, `11`, `12`, `13`, `14`, `15`, `16`, `17`)) %>% 
  select(c(gran, group_item1, distance)) %>% 
  pivot_wider(names_from = group_item1, values_from = distance)


hod_cat <- data_pick %>% 
  dist_gran_cat(gran1 = "hour_day", response = "general_supply_kwh") %>% 
  mutate(gran = "hod")

moy_cat <- data_pick %>% 
  dist_gran_cat(gran1 = "month_year", response = "general_supply_kwh")%>%
  mutate(gran = "moy")

wkndwday_cat <- data_pick %>% 
  dist_gran_cat(gran1 = "wknd_wday", response = "general_supply_kwh")%>% 
  mutate(gran = "wnwd")


data_validation_cat <- bind_rows(hod_cat, moy_cat, wkndwday_cat) %>% 
  mutate(customer_from = as.character(customer_from),
         customer_to = as.character(customer_to))
        


gran_cat_dist <- data_validation_cat %>% 
  left_join(cluster_result, by = c("customer_from" = "customer_id")) %>% 
  rename("group_from" = "group") %>% 
  left_join(cluster_result, by = c("customer_to" = "customer_id")) %>%  
  rename("group_to" = "group") %>% 
  group_by(gran,
           category,
           group_from, 
           group_to) %>% 
  summarise(sum = sum(distance)) %>% 
  pivot_wider(names_from = group_to, values_from = sum) %>% 
  mutate(distance = sum(`1`, `2`, `3`, `4`, `5`,`6`, `7`, `8`, `9`, `10`, `11`, `12`, `13`, `14`, `15`, `16`, `17`)) %>% 
  select(-(4:8)) %>% 
  arrange(-distance) 


# Contribution of individual category for hod


gran_cat_hod <-   gran_cat_dist %>% 
  filter(gran=="hod") %>% 
  mutate(category = as.integer(category)) %>% 
  arrange(group_from, category)

gran_cat_hod$category <- factor(gran_cat_hod$category, levels = 0:23)

# for each group, percent contribution of hours

group_total <- gran_cat_hod %>% 
  ungroup() %>% 
  group_by(group_from) %>% 
  summarise(group_total = sum(distance))

# gran_cat_hod %>% 
#   ungroup %>% 
#   left_join(group_total, by = "group_from") %>% 
#   mutate(cat_contibution = distance*100/group_total) %>% 
#   group_by(group_from) %>% 
#   slice(c(1:3))

# gran_cat_hod %>% 
#   ungroup %>% 
#   left_join(group_total, by = "group_from") %>% 
#   mutate(cat_contibution = distance*100/group_total) %>% 
#   group_by(group_from) %>% 
#   arrange(-distance) %>% slice(c(1:4)) %>% 
#   ggplot(aes(group_from, category, color = as.factor(group_from))) +
#   geom_point(aes(size = distance))+
#   scale_fill_manual(values=as.vector(polychrome(17)))+
#   theme_light()

data_pcp <- gran_cat_hod %>% 
  ungroup %>% 
  left_join(group_total, by = "group_from") %>% 
  mutate(cat_contibution = distance*100/group_total) %>% 
  group_by(group_from) %>% 
  #arrange(-distance) %>% 
  select(group_from, category, cat_contibution) %>% 
  pivot_wider(names_from = "category", 
              values_from = "cat_contibution") %>% mutate(group_from = as.factor(group_from))

parcoord <- GGally::ggparcoord(data_pcp ,
                               columns = 2:ncol(data_pcp),
                               groupColumn = "group_from",
                               showPoints = TRUE, 
                               alphaLines = 1,
                               scale = "globalminmax"
) + 
  ggplot2::theme(
    plot.title = ggplot2::element_text(size=10)
  )+
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 10)) +
  theme(legend.position = "bottom") +
  xlab("") +
  ylab("wpd")+
  scale_fill_manual(values=as.vector(polychrome(17)))+
  theme_bw()
parcoord 






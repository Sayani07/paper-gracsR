##----groups-4and5
cluster_result_kopt4 <- suppressMessages(f %>% 
                                           clust_gran(kopt = 4)) %>% 
  rename("customer_id" = "id") %>% 
  mutate(group = as.factor(group))

cluster_result_id4and5 <- cluster_result_id %>% left_join(cluster_result_kopt4, by="customer_id") %>% rename("group4" = "group.y") %>% rename("group5" = "group.x")
##----data-heatmap-hod-group-new-4and5

data_group <- data_pick_robust  %>% 
  left_join(cluster_result_id4and5, by = c("customer_id"))

data_heatmap_hod_group <- quantile_gran(data_group,
                                        gran1="hour_day",
                                        quantile_prob_val = c(0.25, 0.5, 0.75),
                                        group="group4") %>% 
  pivot_wider(names_from = quantiles, values_from = quantiles_values) 


data_heatmap_hod_group$category <- factor(data_heatmap_hod_group$category, levels = 0:23)

data_heatmap_hod_group$group4 <- paste("groupA", data_heatmap_hod_group$group4, sep = "-")

hod_group <- data_heatmap_hod_group %>% 
  ggplot(aes(x = category)) + 
  geom_ribbon(aes(ymin = `25%`, 
                  ymax = `75%`,
                  group=group4,
                  fill = as.factor(group4), alpha = 0.5),
              alpha = 0.5) +
  geom_line(aes(y = `50%`,
                group=group4, 
                color = as.factor(group4)), size = 1)+
  facet_wrap(~group4, 
             scales = "free_y",  
             nrow = 5, labeller = "label_value") + 
  #labeller = labeller(xfacet = c(`1` = "Group 2", `2` = "Group 4",`3` = "Group 1",`4` = "Group 3"))
  theme(strip.text = element_text(size = 10, margin = margin(b = 0, t = 0))) + xlab("hod") + 
  ylab("demand (in Kwh)") + 
  theme_bw()  +
  scale_x_discrete(breaks = seq(1, 24, 3))+ 
  #theme(strip.text = element_text(size = 8, margin = margin(b = 0, t = 0)))+
  theme_application3() +
  scale_fill_manual(values=as.vector(tableau20(20)))+
  scale_color_manual(values=as.vector(tableau20(20)))+
  theme(legend.position = "bottom") 

##----data-heatmap-moy-group-new-4and5
data_heatmap_moy_group <- quantile_gran(data_group,
                                        gran1="month_year",
                                        quantile_prob_val = c(0.25, 0.5, 0.75),
                                        group="group4") %>% 
  pivot_wider(names_from = quantiles, values_from = quantiles_values) 

data_heatmap_moy_group$category <- factor(data_heatmap_moy_group$category, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))


data_heatmap_moy_group$group4 <- paste("groupA", data_heatmap_moy_group$group4, sep = "-")


moy_group <- data_heatmap_moy_group %>% 
  ggplot(aes(x = category)) + 
  geom_ribbon(aes(ymin = `25%`, 
                  ymax = `75%`, group=group4, fill = as.factor(group4)), alpha = 0.5) +
  geom_line(aes(y = `50%`, group=group4, color = as.factor(group4)), size = 1 ) +
  facet_wrap(~group4, 
             scales = "free_y", 
             labeller = "label_value",
             nrow = 5) +
  theme(strip.text = element_text(size = 10, margin = margin(b = 0, t = 0))) + xlab("moy") + 
  ylab("demand (in Kwh)") +
  theme_bw() + theme_application3()  +
  scale_fill_manual(values=as.vector(tableau20(20)))+
  scale_color_manual(values=as.vector(tableau20(20)))+
  theme(legend.position = "bottom") 

##----data-heatmap-wkndwday-group-4and5
wkndwday_data <- data_group %>% create_gran("wknd_wday") %>% 
  create_gran("hour_day")

ylim1 = boxplot.stats(wkndwday_data$general_supply_kwh)$stats[c(1, 5)]

wkndwday_data$group4 <- paste("groupA", wkndwday_data$group4, sep = "-")


wkndwday_group <- wkndwday_data%>% 
  ggplot(aes(x=wknd_wday, y = general_supply_kwh)) +
  geom_boxplot(aes(fill = group4, color = group4),alpha = 0.5, outlier.alpha = 0.05)+
  coord_cartesian(ylim = ylim1*1.05)+
  xlab("wnwd") + 
  ylab("demand (in Kwh)") +
  facet_wrap(~group4,
             ncol = 1, 
             scales = "free_y", 
             labeller = "label_value") + 
  theme_bw()  +
  scale_fill_manual(values=as.vector(tableau20(10)))+
  scale_color_manual(values=as.vector(tableau20(10)))+
  theme(legend.position = "none") +
  theme_application3()

##----combined-groups-js-4and5
combined_groups_js4nd5 <- (hod_group + moy_group + wkndwday_group) +
  plot_layout(guides = "collect")& theme(legend.position = 'none')


##----data-heatmap-hod-group-new-4and5

data_group <- data_pick_robust  %>% 
  left_join(cluster_result_id4and5, by = c("customer_id"))

data_heatmap_hod_group <- quantile_gran(data_group,
                                        gran1="hour_day",
                                        quantile_prob_val = c(0.25, 0.5, 0.75),
                                        group="group5") %>% 
  pivot_wider(names_from = quantiles, values_from = quantiles_values) 


data_heatmap_hod_group$category <- factor(data_heatmap_hod_group$category, levels = 0:23)

data_heatmap_hod_group$group5 <- paste("groupB", data_heatmap_hod_group$group5, sep = "-")

hod_group <- data_heatmap_hod_group %>% 
  ggplot(aes(x = category)) + 
  geom_ribbon(aes(ymin = `25%`, 
                  ymax = `75%`,
                  group=group5,
                  fill = as.factor(group5), alpha = 0.5),
              alpha = 0.5) +
  geom_line(aes(y = `50%`,
                group=group5, 
                color = as.factor(group5)), size = 1)+
  facet_wrap(~group5, 
             scales = "free_y",  
             nrow = 5, labeller = "label_value") + 
  #labeller = labeller(xfacet = c(`1` = "Group 2", `2` = "Group 4",`3` = "Group 1",`4` = "Group 3"))
  theme(strip.text = element_text(size = 10, margin = margin(b = 0, t = 0))) + xlab("hod") + 
  ylab("demand (in Kwh)") + 
  theme_bw()  +
  scale_x_discrete(breaks = seq(1, 24, 3))+ 
  #theme(strip.text = element_text(size = 8, margin = margin(b = 0, t = 0)))+
  theme_application3()  +
  scale_fill_manual(values = c("#E69F00", "#009E73","#0072B2", "#D55E00","#CC79A7"))+
  scale_color_manual(values = c("#E69F00", "#009E73","#0072B2", "#D55E00", "#CC79A7")) +
  theme(legend.position = "bottom") 

##----data-heatmap-moy-group-new-4and5
data_heatmap_moy_group <- quantile_gran(data_group,
                                        gran1="month_year",
                                        quantile_prob_val = c(0.25, 0.5, 0.75),
                                        group="group5") %>% 
  pivot_wider(names_from = quantiles, values_from = quantiles_values) 

data_heatmap_moy_group$category <- factor(data_heatmap_moy_group$category, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))


data_heatmap_moy_group$group5 <- paste("groupB", data_heatmap_moy_group$group5, sep = "-")


moy_group <- data_heatmap_moy_group %>% 
  ggplot(aes(x = category)) + 
  geom_ribbon(aes(ymin = `25%`, 
                  ymax = `75%`, group=group5, fill = as.factor(group5)), alpha = 0.5) +
  geom_line(aes(y = `50%`, group=group5, color = as.factor(group5)), size = 1 ) +
  facet_wrap(~group5, 
             scales = "free_y", 
             labeller = "label_value",
             nrow = 5) +
  theme(strip.text = element_text(size = 10, margin = margin(b = 0, t = 0))) + xlab("moy") + 
  ylab("demand (in Kwh)") +
  theme_bw() + theme_application3()  +
  scale_fill_manual(values = c("#E69F00", "#009E73","#0072B2", "#D55E00","#CC79A7"))+
  scale_color_manual(values = c("#E69F00", "#009E73","#0072B2", "#D55E00", "#CC79A7")) +
  theme(legend.position = "bottom") 

##----data-heatmap-wkndwday-group-4and5
wkndwday_data <- data_group %>% create_gran("wknd_wday") %>% 
  create_gran("hour_day")

ylim1 = boxplot.stats(wkndwday_data$general_supply_kwh)$stats[c(1, 5)]

wkndwday_data$group5 <- paste("groupB", wkndwday_data$group5, sep = "-")


wkndwday_group <- wkndwday_data%>% 
  ggplot(aes(x=wknd_wday, y = general_supply_kwh)) +
  geom_boxplot(aes(fill = group5, color = group5),alpha = 0.5, outlier.alpha = 0.05)+
  coord_cartesian(ylim = ylim1*1.05)+
  xlab("wnwd") + 
  ylab("demand (in Kwh)") +
  facet_wrap(~group5,
             ncol = 1, 
             scales = "free_y", 
             labeller = "label_value") + 
  theme_bw()   +
  scale_fill_manual(values = c("#E69F00", "#009E73","#0072B2", "#D55E00","#CC79A7"))+
  scale_color_manual(values = c("#E69F00", "#009E73","#0072B2", "#D55E00", "#CC79A7")) +
  theme(legend.position = "none") +
  theme_application3()

##----combined-groups-js-4and5
combined_groups_js5 <- (hod_group + moy_group + wkndwday_group) +
  plot_layout(guides = "collect")& theme(legend.position = 'none')


ggpubr::ggarrange(combined_groups_js4nd5, combined_groups_js5, labels = c("(a)", "(b)"))

ggsave("figs/combined4and5.png")


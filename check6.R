



cluster_result_kopt4 <- suppressMessages(distance_jsd %>%
                                           clust_gran(kopt = 6)) %>%
  rename("customer_id" = "id") %>%
  mutate(group = as.factor(group))


cluster_result_id4and5 <- cluster_result_id %>%
  left_join(cluster_result_kopt4, by = "customer_id") %>%
  rename("group4" = "group.y") %>%
  rename("group5" = "group.x")

# data-heatmap-hod-group-new-4

data_group <- data_pick_robust %>%
  left_join(cluster_result_id4and5, by = c("customer_id"))


data_heatmap_hod_group <- quantile_gran(data_group,
                                        gran1 = "hour_day",
                                        quantile_prob_val = c(0.25, 0.5, 0.75),
                                        group = "group4"
) %>%
  pivot_wider(names_from = quantiles, values_from = quantiles_values)


data_heatmap_hod_group$category <- factor(data_heatmap_hod_group$category,
                                          levels = 0:23
)

data_heatmap_hod_group$group4 <- paste("A", data_heatmap_hod_group$group4,
                                       sep = "-"
)

hod_group <- data_heatmap_hod_group %>%
  ggplot(aes(x = category)) +
  geom_ribbon(aes(
    ymin = `25%`,
    ymax = `75%`,
    group = group4,
    fill = as.factor(group4), alpha = 0.5
  ),
  alpha = 0.5
  ) +
  geom_line(aes(
    y = `50%`,
    group = group4,
    color = as.factor(group4)
  ), size = 1) +
  facet_wrap(~group4,
             scales = "free_y",
             nrow = 6, labeller = "label_value"
  ) +
  theme(strip.text = element_text(size = 10, margin = margin(b = 0, t = 0))) +
  xlab("hod") +
  ylab("demand (in Kwh)") +
  theme_bw() +
  scale_x_discrete(breaks = seq(1, 24, 3)) +
  theme_application3() +
  scale_fill_manual(values = c(
    "#A89030", "#5D7050",
    "#481800",  "#125070", "grey", "pink"
  )) +
  scale_color_manual(values = c(
    "#A89030", "#5D7050",
    "#481800",  "#125070", "grey", "pink"
  ))+
  theme(legend.position = "bottom")

# data-heatmap-moy-group-new-4
data_heatmap_moy_group <- quantile_gran(data_group,
                                        gran1 = "month_year",
                                        quantile_prob_val = c(0.25, 0.5, 0.75),
                                        group = "group4"
) %>%
  pivot_wider(names_from = quantiles, values_from = quantiles_values)

data_heatmap_moy_group$category <- factor(data_heatmap_moy_group$category,
                                          levels = c(
                                            "Jan", "Feb", "Mar", "Apr",
                                            "May", "Jun", "Jul", "Aug",
                                            "Sep", "Oct", "Nov", "Dec"
                                          )
)


data_heatmap_moy_group$group4 <- paste("A", data_heatmap_moy_group$group4,
                                       sep = "-"
)


moy_group <- data_heatmap_moy_group %>%
  ggplot(aes(x = category)) +
  geom_ribbon(aes(
    ymin = `25%`,
    ymax = `75%`, group = group4, fill = as.factor(group4)
  ), alpha = 0.5) +
  geom_line(aes(y = `50%`, group = group4, color = as.factor(group4)),
            size = 1
  ) +
  facet_wrap(~group4,
             scales = "free_y",
             labeller = "label_value",
             nrow = 6
  ) +
  theme(strip.text = element_text(size = 10, margin = margin(b = 0, t = 0))) +
  xlab("moy") +
  ylab("demand (in Kwh)") +
  theme_bw() +
  theme_application3() +
  scale_fill_manual(values = c(
    "#A89030", "#5D7050",
    "#481800",  "#125070", "grey", "pink"
  )) +
  scale_color_manual(values = c(
    "#A89030", "#5D7050",
    "#481800",  "#125070", "grey", "pink"
  ))+
  theme(legend.position = "bottom")

# data-heatmap-wkndwday-group-4
wkndwday_data <- data_group %>%
  create_gran("wknd_wday") %>%
  create_gran("hour_day")

ylim1 <- boxplot.stats(wkndwday_data$general_supply_kwh)$stats[c(1, 5)]

wkndwday_data$group4 <- paste("A", wkndwday_data$group4, sep = "-")


wkndwday_group <- wkndwday_data %>%
  ggplot(aes(x = wknd_wday, y = general_supply_kwh)) +
  geom_boxplot(aes(fill = group4, color = group4),
               alpha = 0.5,
               outlier.alpha = 0.05
  ) +
  coord_cartesian(ylim = ylim1 * 1.05) +
  xlab("wnwd") +
  ylab("demand (in Kwh)") +
  facet_wrap(~group4,
             ncol = 1,
             scales = "free_y",
             labeller = "label_value"
  ) +
  theme_bw() +
  scale_fill_manual(values = c(
    "#A89030", "#5D7050",
    "#481800",  "#125070", "grey", "pink"
  )) +
  scale_color_manual(values = c(
    "#A89030", "#5D7050",
    "#481800",  "#125070", "grey", "pink"
  ))+
  theme(legend.position = "none") +
  theme_application3()


combined_groups_js4 <- (hod_group + moy_group + wkndwday_group) +
  plot_layout(guides = "collect", ) & theme(legend.position = "none")



# combined_groups_js4 <- ggarrange(hod_group, moy_group, wkndwday_group, ncol = 3)



elec_600_wpd <- read_rds(here::here("data/algo2-cust600-wpd-rawdata.rds"))

elec_pick <- elec_600_wpd %>%
  filter(customer_id %in% data_pick_cust$customer_id)

elec_pick_wide <- elec_pick %>% pivot_wider(-c(1, 2), names_from = "x_variable", values_from = wpd)

scaled_var <- elec_pick_wide

distance_wpd <- elec_pick_wide[-1] %>% scale() %>%  dist()


all_index_wpd <- map_dfr(2:10, function(x) {
  group <- distance_wpd %>%
    hclust(method = "ward.D") %>%
    cutree(k = x)
  p <- cluster.stats(distance_wpd, clustering = group, silhouette = TRUE)
  index <- c(k = x, sindex = p$sindex)
})%>% mutate(method = "WPD")


opt_clusters_wpd <- all_index_wpd %>% ggplot(aes(x = k, y = sindex)) +
  geom_line() +
  scale_x_continuous(breaks = seq(2, 10, 1), minor_breaks = 1) +
  theme_bw() +
  ylab("sindex") +
  xlab("number of clusters") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8))



## ----wpd-clustering-----------------------------------------------------------
group <- distance_wpd %>%
  hclust(method = "ward.D") %>%
  cutree(k = 3)


cluster_result_wpd <- bind_cols(id = elec_pick_wide$customer_id, group = group)

data_pcp <- scaled_var %>%
  # bind_cols(customer_id =  elec_pick_wide$customer_id) %>%
  left_join(cluster_result_wpd, by = c("customer_id" = "id")) %>%
  select(customer_id, group, everything()) %>%
  mutate(group = as.factor(group)) %>%
  mutate(customer_id = as.character(customer_id)) %>%
  rename(
    "moy" = "month_year",
    "hod" = "hour_day",
    "wnwd" = "wknd_wday"
  )

data_table <- data_pcp %>%
  group_by(group) %>%
  summarise(
    nobs = n(),
    moy = round(median(moy), 2),
    hod = round(median(hod), 2),
    wnwd = round(median(wnwd), 2)
  ) %>%
  select(-group)

# rownames(data_table) <- c("group-1", "group-2", "group-3", "group-4")




parcoord <- GGally::ggparcoord(data_pcp %>% left_join(cluster_result_id, by = "customer_id"),
                               columns = 3:5,
                               groupColumn = "group.x",
                               showPoints = FALSE,
                               alphaLines = 0.8,
                               order = "anyClass",
                               scale = "globalminmax"
) +
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 10)
  ) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 10)) +
  theme(legend.position = "bottom") +
  xlab("") +
  ylab("wpd") +
  scale_color_brewer(palette = "Dark2") +
  labs(colour = "group") +
  theme(panel.border = element_blank())


parcoord + geom_text(aes(label = id)) & theme(legend.position = "bottom")



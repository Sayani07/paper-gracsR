change_index_data <- function(ntimes_val = NULL,
                         nx_val = NULL,
                         nfacet_val = NULL,
                         sim_function = sim_varx_normal){

  data <- sim_panel(
    nx = nx_val, nfacet =  nfacet_val,
    ntimes = ntimes_val,
    # sim_dist = sim_varx_normal(2, 3, 5, 10, 5, -1.5)
    sim_dist = sim_function(nx_val, nfacet_val, mean_val, sd_val, w1_val, w2_val)
  ) %>% unnest(data)


  index_new <- map(seq_len(ntimes_val), function(i){
    map((seq_len(nx_val*nfacet_val)), function(j)
    {
      value = i + (j-1)*ntimes_val
    })
  }) %>% unlist()

  data_new = data %>%
    ungroup() %>%
    mutate(index_old = row_number(),
           index_new = index_new)

  y = data_new[match(index_new, data_new$index_old),]

  y <- y %>%
    mutate(time = row_number()) %>%
    select(-c(index_old, index_new))

  return(y)
}
##----making 10 iterations of 4 designs

sample_seed <-  seq(10, 100, 10)

data_null <- map(1:10, function(seed){
  set.seed(sample_seed[seed])
  change_index_data(5, 2, 3, sim_null_normal) %>%
    compute_quantiles(quantile_prob_val)
}) %>%
  bind_rows(.id = "seed_id") %>%
  mutate(data_type = "data_null")


data_varf <- map(1:10, function(seed){
  set.seed(sample_seed[seed])
  change_index_data(5, 2, 3, sim_varf_normal)%>%
    compute_quantiles(quantile_prob_val)
}) %>%
  bind_rows(.id = "seed_id") %>%
  mutate(data_type = "data_varf")

data_varx <- map(1:10, function(seed){
  set.seed(sample_seed[seed])
  change_index_data(5, 2, 3, sim_varx_normal)%>%
    compute_quantiles(quantile_prob_val)
}) %>%
  bind_rows(.id = "seed_id") %>%
  mutate(data_type = "data_varx")

data_varall <- map(1:10, function(seed){
  set.seed(sample_seed[seed])
  change_index_data(5, 2, 3, sim_varall_normal)%>%
    compute_quantiles(quantile_prob_val)
}) %>%
  bind_rows(.id = "seed_id") %>%
  mutate(data_type = "data_varall")

data_q <- bind_rows(data_null,
                      data_varf,
                      data_varx,
                      data_varall) %>%
  mutate(unique_data = paste(data_type, seed_id, sep = "-"))

# clustering process

## Compute quantiles of conditional distributions

data_q_wide <- data_q %>%
  select(id_facet, id_x, unique_data, sim_data_quantile) %>%
  pivot_wider(names_from = unique_data,
              values_from = sim_data_quantile) %>%
  select(-c(1, 2))

ndata <- data_q %>% distinct(unique_data) %>% mutate(index = row_number())
ldata <- nrow(ndata)
lcomb <-  nx_val*nfacet_val

dist_data <- map(1:ldata, function(x){ # first data
  map(1:ldata, function(y){ # 2nd data
    map(1:lcomb, function(z){ # number of combinations nx*nfacet
      JS(
        prob = quantile_prob_val,
        unlist(data_q_wide[z,x]),
        unlist(data_q_wide[z,y])
      ) %>% as_tibble()
    })%>% bind_rows(.id = "combinations")
  })%>% bind_rows(.id = "data_type1")
}) %>% bind_rows(.id = "data_type2")



dist_mat <- dist_data %>%
  group_by(data_type1, data_type2) %>%
  summarise(dist = sum(value)) %>%
  pivot_wider(names_from = data_type2,
              values_from = dist) %>%
  mutate(data_type1 = as.numeric(data_type1)) %>%
  left_join(ndata, by = c("data_type1" = "index"))


dist_mat_format <- dist_mat %>%
  ungroup() %>%
  select(-data_type1, -unique_data)


rownames(dist_mat_format) <- dist_mat$unique_data

d = stats::as.dist(dist_mat_format)

hc = stats::hclust(d,method="complete")
plot(hc)

## Multi-dimensional scaling with hierarchical clusters

mds <- d %>%
  cmdscale() %>%
  as_tibble()

colnames(mds) <- c("Dim.1", "Dim.2")
rownames(mds) <- dist_mat$unique_data

groups<-cutree(hc, k=4)

all_data_cluster <- cbind(dist_mat_format, groups) %>%
  cbind(mds)%>%
  mutate(groups = as.factor(groups)) %>% as_tibble()


mds_plot_10 <- ggplot(all_data_cluster,
       aes(x = Dim.1,
           y = Dim.2,
       color = groups,
       label = rownames(mds))) +
  geom_point(size = 0.5) +
  geom_text(check_overlap = TRUE)  +
  theme_classic()+
  theme(legend.position = "bottom") +
  scale_color_brewer(palette = "Dark2")




#
# +
#   geom_text(data = ndata, aes(label = unique_data)) +
#   theme_bw()
#
# mds_plot_10 <- ggpubr::ggscatter(data = all_data_cluster,
#                               x = "Dim.1",
#                               y = "Dim.2")
#
#
#
#                               label = rownames(mds),
#                               color = "groups",
#                               repel = TRUE,
#                               size = 2, shape = 1) +
#   scale_color_brewer(palette = "Dark2")

mds_plot_10


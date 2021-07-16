library(ggpubr)
library(gravitas)
library(tibble)
clust_var <- "customer_id"
ncust = 10
niter = 2
response = "general_supply_kwh"
gran1 = "wknd_wday"
gran2 = "hour_day"

dis_cus <- smart_meter10 %>%
  dplyr::distinct(.data[[clust_var]]) %>%
  dplyr::sample_n(ncust)

cust_data <- (1:nrow(dis_cus)) %>%
  purrr::map_df(function(i){
    make_cust_iter(smart_meter10, gran1, gran2, response, custid = dis_cus[[clust_var]][i], niter)
  })

data_whole <-  cust_data %>%
  dplyr::group_by(cust_iter_id) %>%
  dplyr::mutate(index = dplyr::row_number()) %>%
  dplyr::ungroup() %>%
  tsibble::as_tsibble(index = index, key = cust_iter_id)

.data = data_whole
distance_mat <- sim_dist(.data, gran1, gran2, response = "sim_data", key = "cust_iter_id",dist_ordered = TRUE,method = "max")
mds <- distance_mat %>%
dist() %>%
cmdscale() %>%
as_tibble()
colnames(mds) <- c("Dim.1", "Dim.2")

# Plot MDS

ggscatter(mds, x = "Dim.1", y = "Dim.2",
label = colnames(distance_mat),
size = 1,
repel = TRUE)

# clustering

.data <- smart_meter10
gran1 = "wknd_wday"
gran2 = "hour_day"
 key = "customer_id"
 response = "general_supply_kwh"
 distance_mat_all <- sim_dist(.data, gran1, gran2, response, key,dist_ordered = TRUE,method = "max")
 d <- stats::as.dist(distance_mat_all)
 hc <- stats::hclust(d)
 ggdendro::ggdendrogram(hc)

collapse_x <- function(x)
{
  y = unlist(x)
  z = paste(y, collapse = ",")
}

x = list(1, 5, 2, c(1,5))
cluster_summary <- cluster_result_id %>% select(-customer_id, -divide_cust) %>%  group_by(group) %>% nest(data = c(id))

apply(cluster_summary, 1, collapse_x)
z =  rbind(group = cluster_summary$group, rbind(members = map(cluster_summary$data, collapse_x))) %>% t()

z =  rbind(group = cluster_summary$group, rbind(members = map(cluster_summary$data, collapse_x))) %>% t()


#' Dendogram of simulated objects
#'
#' @param .data a tsibble.
#' @param gran1,gran2 the granularity against which similarity is measured. For the granularity hour of the week, value is "hour_week".
#' @param response response variable.
#' @param clust_var column in which subject lies
#' @param ncust number of subjects taken
#' @param niter numbre of simulations per customer
#' @return dendogram describing the tree produced by the clustering process
#'
#' @examples
#'library(ggplot2)
#'library(dplyr)
#'library(ggdendro)
#'library(gravitas)
#'.data = smart_meter10
#'gran1 = "wknd_wday"
#'gran2 = "hour_day"
#'response = "general_supply_kwh"
#'clust_var = "customer_id"
#' @examples
#' conditional_dendro(.data, gran1,
#'                       gran2, response,
#'                       clust_var, niter = 5,
#'                       ncust = 2)
#' @export
conditional_dendro <- function(.data, gran1,
                                   gran2, response,
                                   clust_var, niter = 5,
                                   ncust = 2)
{
dis_cus <- .data %>%
  dplyr::distinct(.data[[clust_var]]) %>%
  dplyr::sample_n(ncust)

cust_data <- (1:nrow(dis_cus)) %>%
  purrr::map_df(function(i){
    make_cust_iter(.data, gran1, gran2, response, custid = dis_cus[[clust_var]][i], niter)
    })

data_whole <-  cust_data %>%
  dplyr::group_by(cust_iter_id) %>%
  dplyr::mutate(index = dplyr::row_number()) %>%
  dplyr::ungroup() %>%
  tsibble::as_tsibble(index = index, key = cust_iter_id)

distance_sum10cust <- sim_dist(data_whole,
                              gran1,
                              gran2,
                              response = "sim_data",
                              key = "cust_iter_id",
                              dist_ordered = TRUE,
                              method = "sum")

d <- stats::as.dist(distance_sum10cust)
hc <- stats::hclust(d)
simulate_5each_sum <- ggdendro::ggdendrogram(hc)
simulate_5each_sum + ggplot2::ggtitle(paste(nrow(dis_cus),"customer simulated", niter, "times"))
}

library(dplyr)
data_heatmap <- data_whole %>%
  dplyr::mutate(variable = paste(wknd_wday, hour_day, sep = "-")) %>% dplyr::group_by(cust_iter_id, variable) %>%
  dplyr::summarise(value = median(sim_data))

heatmap.plot <- ggplot(data = data_heatmap, aes(x = variable, y = cust_iter_id)) +
  geom_tile(aes(fill = value)) +
  scale_fill_gradient2() +
  theme(axis.text.y = element_text(size = 6))

heatmap.plot
library(dplyr)
dendro.plot <- conditional_dendro(.data, gran1,
                                  gran2, response,
                                  clust_var, niter = 5,
                                  ncust = 10)

library(grid)
grid.newpage()
print(heatmap.plot, vp = viewport(x = 0.4, y = 0.5, width = 0.8, height = 1.0))
print(dendro.plot, vp = viewport(x = 0.90, y = 0.445, width = 0.2, height = 1.0))



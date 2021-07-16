#' Plot dendogram of simulated data for bivariate granularities
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
#'library(ggdendro)
#'library(gravitas)
#'library(tidyverse)
#'.data = smart_meter10
#'gran1 = "day_week"
#'gran2 = "hour_day"
#'response = "general_supply_kwh"
#'clust_var = "customer_id"
#' @export
make_cust_iter <- function(.data = NULL, gran1 = NULL,
                           gran2 = NULL, response = NULL,
                           niter = 20, custid = NULL,
                           clust_var = "customer_id")
{
  cust_data <- .data %>%
    dplyr::filter(.data[[clust_var]] %in% custid)

  step1_data <- cust_data %>%
    create_gran_pair(gran1, gran2) %>%
    tibble::as_tibble() %>%
    dplyr::select(-tsibble::index(.data)) %>%
    dplyr::mutate(
      response = .data[[response]]
    ) %>%
    dplyr::select(-!!response) %>%
    tidyr::pivot_wider(names_from = tidyselect::all_of(gran1),
                       values_from = response,
                       values_fn = list(response = list))

  # took me 1 hour to write this and make it work
  data_iter_cust1 <- (1:nrow(step1_data)) %>%
    purrr::map(function(rowi){
      (3:ncol(step1_data)) %>%
        purrr::map(function(coli){

          cell_data <- step1_data %>%
            magrittr::extract2(rowi, coli) %>%
            unlist()

          (1:niter) %>%
            purrr::map(function(iter){
              cell_data %>% sample(size = length(cell_data),
                                   replace = TRUE)}) %>%
            tibble::enframe(value = "sim_data", name = "sim_id")
        }) %>%
        tibble::enframe(name = "gran1") }) %>%
    tibble::enframe(name = "gran2") %>%
    #till here we get each level of gran2 correposnding to a list %>%
    tidyr::unnest(value) %>%
    # unfolding to get levels of gran1
    tidyr::unnest(value) %>%
    # unfolding to get levels of iterations and corresponding value
    tidyr::unnest(sim_data)

  return_val <- data_iter_cust1 %>%
    dplyr::mutate(cust_iter_id = paste(custid, sim_id, sep = "-")) %>%
    dplyr::mutate(
      !!gran1 := gran1, !!gran2 := gran2) %>%
    dplyr::select(cust_iter_id, everything(), - sim_id, -gran1, -gran2)

  return(return_val)
}

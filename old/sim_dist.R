#' Similarity distances based on probability distributions
#'
#' @param .data a tsibble.
#' @param gran1,gran2 the granularity against which similarity is measured. For the granularity hour of the week, value is "hour_week".
#' @param response response variable.
#' @param key column in which subject lies
#' @param method "sum" or "max" of distances
#' @param ... Other arguments passed on to individual methods.
#' @return matrix of similarity distances based on probability distributions
#'
#' @examples
#' library(tsibble)
#' library(dplyr)
#' library(gravitas)
#' library(purrr)
#' .data <- smart_meter10
#' gran1 <- "hour_day"
#' gran2 <- "day_week"
#' key = "customer_id"
#' response = "general_supply_kwh"
#' distance_mat <- sim_dist(.data, gran1, gran2, response, key,dist_ordered = TRUE,method = "max")
#' @export sim_dist

# distance matrix of all subjects for a harmony pair based on probability distribution

sim_dist <- function(.data = NULL,
                         gran1 = NULL,
                         gran2 = NULL,
                         response = NULL,
                         key = NULL,
                         method = "max",...
)
{
  distinct_key <- .data %>%
    tibble::as_tibble() %>%
    dplyr::select(-tsibble::index(.data)) %>%
    dplyr::select(key) %>%
    dplyr::distinct()

  distinct_key2 <- distinct_key %>%
    dplyr::mutate(key2 = distinct_key[[key]]) %>%
    expand.grid() %>%
    dplyr::rename(key1 = !!key) %>%
    dplyr::mutate(value = 1) %>%
    tidyr::pivot_wider(names_from = key2,
                       values_from = value) %>%
    dplyr::mutate(key1 = as.character(key1))

   #  filter(key1!=key2) %>%
   #  mutate(conc_key = paste(key1,"-",key2)) %>%
   #  mutate(conc_reverse = paste(key2,"-",key1)) %>%
   # filter(conc_key = conc_reverse)

  colNames <- colnames(distinct_key2)
  colNames <-colNames[2:length(colNames)]

distance = matrix(0, nrow(distinct_key2), nrow(distinct_key2))
lenrow = nrow(distinct_key2)


  for(j in 1:(lenrow-1))
  {
for(i in (j+1):(lenrow))
{
  if(distinct_key2$key1[i]==colNames[j])
  {
    distance[i, j] = NA
  }
  else
  {
  distance[i,j] =  sim_dist_pair(.data, gran1, gran2,
                            response, key,
                            subject1 = distinct_key2$key1[i],
                            subject2 = colNames[j])
  }
}
  }
distance_tbl <- distance %>% tibble::as_tibble(.name_repair = "universal")
names(distance_tbl) <- colNames
distance_tbl
}

# distance matrix of two subjects for a harmony pair

sim_dist_pair<- function(.data = NULL,
                     gran1 = NULL,
                     gran2 = NULL,
                     response = NULL,
                     key = NULL,
                     subject1 = NULL,
                     subject2 = NULL,
                     dist_ordered = TRUE,
                     method = "max"
                     )
{
 harmony_mat_sub1 <- .data %>%
    dplyr::filter(.data[[key]] %in% subject1) %>%
  harmony_mat(gran1 =   gran1, gran2 = gran2, response = response)

harmony_mat_sub2 <- .data %>%
   dplyr::filter(.data[[key]] %in% subject2) %>%
   harmony_mat(gran1 =  gran1, gran2 = gran2, response = response)


nc <- length(harmony_mat_sub1)
nr <- length(harmony_mat_sub1[[1]])

dist <- matrix(NA,
               nc,
               nr)

for(i in 1:nc)
{
  for (j in 1:nr)
  {
    m1 <- harmony_mat_sub1[[i]][[j]]
    m2 <- harmony_mat_sub2[[i]][[j]]
    dist[i, j] <- compute_JSD(m1, m2)
    dist[dist == 0] <- NA
    }
}
if(gran1==gran2)
  return(NA)
if(method == "max")
  return(max(dist, na.rm = TRUE))
if(method == "sum")
  return(sum(dist, na.rm = TRUE))
}

create_gran_pair <-  function(.data, gran1, gran2, hierarchy_tbl = NULL)
{
  events <- match(c(gran1, gran2), names(.data))

  if (!any(is.na(events))) {
    return(.data)
  }

  else{
  .data %>%
    gravitas::create_gran(gran1, hierarchy_tbl) %>%
    gravitas::create_gran(gran2, hierarchy_tbl)
}
}

# harmony data matrix for one subject
harmony_mat <- function(.data = NULL,
                        gran1 = NULL,
                        gran2 = NULL,
                        response = NULL)
{
 create_wide <- create_gran_pair(.data, gran1, gran2)

 create_list <- create_wide %>%
   tibble::as_tibble() %>%
   dplyr::select(-tsibble::index(create_wide)) %>%
   dplyr::mutate(
     response = .data[[response]]
   ) %>%
   dplyr::select(-!!response) %>%
   tidyr::pivot_wider(names_from = gran1,
                      values_from = response,
                      values_fn = list(response = list))

 colnames(create_list) <- paste0("L",colnames(create_list))
 colNms <- colnames(create_list)[3:ncol(create_list)]

create_quant_list <- NULL
 for (i in 1:length(colNms)) {
   create_quant_list[[i]] <- lapply(create_list[[colNms[i]]], quantile_extractx)
 }

create_quant_list

#
#    (i in 1:ncol(create_list)) %>%
#    purrr::map(function(i)){
#      create_list %>%
#      magrittr::extract2(i) %>%
#      (i in 1:nrow(create(list))) %>%
#        purrr::map(function(rowi)){
#      quantile_extractx(rowi)
#        }
#    }
}

quantile_extractx <- function(x =  NULL, prob = seq(0.01, 0.99, by = 0.01))
{
  stats::quantile(x, prob, type=8, na.rm = TRUE)
}

#  Rob's code for computing JSD using quantiles
#
# Compute Jensen-Shannon distance
# based on quantiles q and p at probabilities prob
JS <- function(prob,q,p)
{
  # Compute approximate densities
  x <- seq(min(q,p),max(q,p), l=201)
  qpmf <- pmf(x,prob,q)
  ppmf <- pmf(x,prob,p)
  m <- 0.5 * (ppmf + qpmf)
  JS <- suppressWarnings(0.5*(sum(stats::na.omit(ppmf*log(ppmf/m))) +
                                sum(stats::na.omit(qpmf*log(qpmf/m)))))
  return(JS)
}

# Compute approximate discretized density (like a probability mass function)
# at each x (equally spaced) given quantiles q with probabilities p
pmf <- function(x, p, q)
{
  qcdf <- stats::approx(q,p,xout=x,yleft=0,yright=1, ties = mean)$y
  qpmf <- c(0,diff(qcdf)/ (x[2]-x[1]))
  return(qpmf / sum(qpmf))
}

compute_JSD <- function(x, y, prob = seq(0.01, 0.99, 0.01))
{
  JSD <- JS(prob, x, y)
  return(JSD)
}


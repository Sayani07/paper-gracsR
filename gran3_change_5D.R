# Rscripts for simulation chapter 3, patterns across 3 grans changing atleast across one design amongst 5 designs

library(conflicted)
library(tidyverse)
library(gracsr)
library(tsibble)
library(gravitas)
library(dplyr)
library(here)


# fix parameters
 
# niter
#seed_len = seq(12345, 12350, 1)
# number of iterations corresponding to
niter <- c(5, 50) #number of series you are clustering
nT <-  c(300, 1000, 5000) # length of the time series
mean_diff <- c(1, 2, 5) # difference between consecutive categories

# number of iterations corresponding to
# niter <- c(50) #number of series you are clustering
# nT <-  c(300) # length of the time series
# mean_diff <- c(1) # difference between consecutive categories

time_series <- c("ar1", "arma22")

simtable <- expand.grid(mean_diff = mean_diff,
                        niter = niter,
                        time_series = time_series, 
                        nT = nT)

#scen<-as.numeric(commandArgs()[[6]]) # If running batch job uncomment this
scen <- 16

simj<-simtable[scen,] #Extract row of table
mean_diffj <- simj$mean_diff
niterj <- simj$niter
nTj <- simj$nT
time_seriesj <- simj$time_series
#seed_lenj <- simj$seed_len



generate_design <- function(n = 300, #length of time series
                            mu11 = 0,
                            mu12 = 0,
                            mu21 = 0,
                            mu22 = 0 ,
                            mu23 = 0,
                            mu31 = 0,
                            mu32 = 0,
                            mu33 = 0,
                            mu34 = 0,
                            mu35 = 0)
{
  
  n <- n+500 # 500 burning observations
  t <- seq(0, n-1, 1)
  
  g1 <- t %%2
  g2 <- t %%3
  g3 <- t %%5
  
  str_gran <- bind_cols(index = t, g1 = g1, g2 = g2, g3 = g3)
  
  # calculation of g1
  g1_table <- bind_cols(g1 = unique(g1), dist_mean = c(mu11, mu12))
  
  g1_tally <- str_gran %>% group_by(g1) %>%
    count() %>%
    left_join(g1_table, by = "g1")
  
  g1_dist <- g1_tally %>%
    mutate(g1_d = list(rep(g1, each = n)),
           g1_dist = list(rnorm(n, dist_mean, 1))) %>% 
    ungroup() %>% 
    select(g1_d, g1_dist) %>% 
    unnest(cols = c(g1_d, g1_dist))
  
  g1_data <- str_gran %>% arrange(g1) %>% bind_cols(g1_dist = g1_dist$g1_dist) %>% arrange(index)
  
  # calculation of g2
  
  g2_table<- bind_cols(g2 = unique(g2),
                       dist_mean = c(mu21, mu22, mu23))
  
  g2_tally <- str_gran %>%
    group_by(g2) %>%
    count() %>% 
    left_join(g2_table, by = "g2")
  
  g2_dist <- g2_tally %>%
    mutate(g2_d = list(rep(g2, each = n)),
           g2_dist = list(rnorm(n, dist_mean, 1))) %>% 
    ungroup() %>% 
    select(g2_d, g2_dist) %>% 
    unnest(cols = c(g2_d, g2_dist))
  
  g2_data <- str_gran %>% arrange(g2) %>% bind_cols(g2_dist = g2_dist$g2_dist) %>% arrange(index)
  
  # calculation of g3
  
  g3_table<- bind_cols(g3 = unique(g3), dist_mean = c(mu31, mu32, mu33, mu34, mu35))
  
  g3_tally <- str_gran %>% group_by(g3) %>% count() %>% left_join(g3_table, by = "g3")
  
  g3_dist <- g3_tally %>%
    mutate(g3_d = list(rep(g3, each = n)),
           g3_dist = list(rnorm(n, dist_mean, 1))) %>% 
    ungroup() %>% 
    select(g3_d, g3_dist) %>% 
    unnest(cols = c(g3_d, g3_dist))
  
  g3_data <- str_gran %>% arrange(g3) %>% bind_cols(g3_dist = g3_dist$g3_dist) %>% arrange(index)
  
  
  innov_data <- g1_data %>% 
    left_join(g2_data %>% select(index, g2_dist), by = "index") %>% 
    left_join(g3_data %>% select(index, g3_dist), by = "index")
  
  # ar 0.1 ma 0.3
  # list(ar = c(0.8897, -0.4858), ma = c(-0.2279, 0.2488))
  
  if(time_seriesj=="arma22"){
    ts_data <- arima.sim(n=n, list(ar = c(0.8897, -0.4858), ma = c(-0.2279, 0.2488)),
                         innov =  innov_data$g1_dist + 
                           innov_data$g2_dist +
                           innov_data$g3_dist)
  }
  else
  {
    ts_data <- arima.sim(n=n, list(ar = c(0.3), ma = c(0)),
                         innov =  innov_data$g1_dist + 
                           innov_data$g2_dist +
                           innov_data$g3_dist)
  }
  
  
  nd_time <- innov_data %>% bind_cols(ts = as.numeric(ts_data))
  nd_time[-c(1:500),] %>% mutate(index = index - 500)
  
  # 500 burning observations considered
}




set.seed(123)
  
##----bind-designs for many iterations
bind_data_iter <- map(seq_len(niterj), 
                      function(x){
  #set.seed(seed_len[x])  
  design1 <- generate_design(n=nTj) # null design
  design2 <- generate_design(n=nTj,mu12=mean_diffj)
  design3 <- generate_design(n=nTj,mu22=mean_diffj, mu23 =2*mean_diffj)
  design4 <- generate_design(n=nTj,mu32 = mean_diffj,mu33 = 2*mean_diffj,mu34 = mean_diffj)
  design5 <- generate_design(n=nTj,mu12=mean_diffj, mu22=mean_diffj, mu23 =2*mean_diffj, mu32 = mean_diffj,mu33 = 2*mean_diffj, mu34 = mean_diffj)
  
  bind_design <- bind_rows(design1, design2, design3, design4, design5, .id = "design")
}) %>% bind_rows(.id = "seed_id") %>% 
  mutate(customer_id = paste(design,seed_id, sep ="-"))


##---tsibble-data
bind_data_iter_tsibble <- bind_data_iter %>% 
  tsibble::as_tsibble(index = index, key = customer_id)

##----wpd-method

harmony_tbl <- tibble(facet_variable = NA, x_variable = c("g1", "g2", "g3"),
                      x_levels = c(2, 3, 5), facet_levels = NA)

dist_mat <- bind_data_iter_tsibble %>%
  #scale_gran(method = "robust", response = "sim_data") %>%
  dist_wpd(harmony_tbl, response = "ts", nperm=100)

groups = dist_mat%>% clust_gran(kopt = 5)

pred_group = paste(groups$group,sep = "") %>% as.factor()
actual_group = as.factor(bind_data_iter_tsibble %>%as_tibble %>% select(customer_id, design) %>% distinct() %>% pull(design))

xtab <- caret::confusionMatrix(pred_group, actual_group)

pred_ref_table <- xtab$table %>% as_tibble() %>% mutate(simj)

write_rds(pred_ref_table, here(paste0("wpd/3gran_change_5D/pred_ref_table_", scen, ".rds")))

confmatrix <- xtab %>% broom::tidy(by_class = FALSE) %>% mutate(simj)

write_rds(confmatrix, here(paste0("wpd/3gran_change_5D/confmatrix_", scen, ".rds")))
# 
# xtab %>% broom::tidy(by_class = FALSE)
# 
# pred_ref_table <- xtab$table %>% as_tibble()


##----NQT scaling

dist_mat_g1 <- bind_data_iter_tsibble %>%
  scale_gran(method = "nqt", response = "ts") %>%
  dist_gran(gran1 = "g1", response= "ts")

dist_mat_g2 <- bind_data_iter_tsibble %>%
  scale_gran(method = "nqt", response = "ts") %>%
  dist_gran(gran1 = "g2", response= "ts")


dist_mat_g3 <- bind_data_iter_tsibble %>%
  scale_gran(method = "nqt", response = "ts") %>%
  dist_gran(gran1 = "g3", response= "ts")

dist_mat <- dist_mat_g1 + dist_mat_g2 + dist_mat_g3

groups = dist_mat %>% 
  clust_gran(kopt = 5)


pred_group = paste(groups$group,sep = "") %>% as.factor()

actual_group = as.factor(bind_data_iter_tsibble %>%as_tibble %>% select(customer_id, design) %>% distinct() %>% pull(design))

xtab <- caret::confusionMatrix(pred_group, actual_group)

# xtab %>% broom::tidy(by_class = FALSE)

pred_ref_table <- xtab$table %>% as_tibble() %>% mutate(simj)

write_rds(pred_ref_table, here(paste0("js-nqt/3gran_change_5D/pred_ref_table_", scen, ".rds")))


confmatrix <- xtab %>% broom::tidy(by_class = FALSE) %>% mutate(simj)

write_rds(confmatrix, here(paste0("js-nqt/3gran_change_5D/confmatrix_", scen, ".rds")))




##----Robust scaling

dist_mat_g1 <- bind_data_iter_tsibble %>%
  scale_gran(method = "robust", response = "ts") %>%
  dist_gran(gran1 = "g1", response= "ts")

dist_mat_g2 <- bind_data_iter_tsibble %>%
  scale_gran(method = "robust", response = "ts") %>%
  dist_gran(gran1 = "g2", response= "ts")


dist_mat_g3 <- bind_data_iter_tsibble %>%
  scale_gran(method = "robust", response = "ts") %>%
  dist_gran(gran1 = "g3", response= "ts")

dist_mat <- dist_mat_g1 + dist_mat_g2 + dist_mat_g3

groups = dist_mat %>% 
  clust_gran(kopt = 5)


pred_group = paste(groups$group,sep = "") %>% as.factor()

actual_group = as.factor(bind_data_iter_tsibble %>%as_tibble %>% select(customer_id, design) %>% distinct() %>% pull(design))

xtab <- caret::confusionMatrix(pred_group, actual_group)

# xtab %>% broom::tidy(by_class = FALSE)

pred_ref_table <- xtab$table %>% as_tibble() %>% mutate(simj)

write_rds(pred_ref_table, here(paste0("js-robust/3gran_change_5D/pred_ref_table_", scen, ".rds")))

confmatrix <- xtab %>% broom::tidy(by_class = FALSE) %>% mutate(simj)

write_rds(confmatrix, here(paste0("js-robust/3gran_change_5D/confmatrix_", scen, ".rds")))





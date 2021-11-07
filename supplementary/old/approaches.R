# harmony table considered for all

#This script calculates raw mmpd for each simulation scenario
##Read Simulation Table

.libPaths(c("~/R/libs", .libPaths()))
library(distributional)
library(readr)
library(drake)
library(tidyverse)
library(hakear)
library(gravitas)
library(parallel)
library(here)
library(lubridate)
library(tsibble)
library(patchwork)

set.seed(9999)

# parameter set
nsamp = 100 # number of samples based on which thresholds are decided
ncust = 8 # number of customers for trial
nrep = 3 # number of error added usage for each customer
kstep1 = 3 # number of clusters in step1/ only step
kstep2 = 2 # number of clusters in step2 (this will actually depend on the number of observations in the cluster after step1)

# change path while running it on HPC
simtable<-read_csv(here('../gracsR/sim_table/sim_table.csv'))


# <needed>
harmonies<-read_csv(here('../gracsR/sim_table/harmonies.csv'))

elec <- read_rds(here("data/elec_all-8.rds")) %>% 
  dplyr::filter(date(reading_datetime) >= ymd("20190701"), date(reading_datetime) < ymd("20191231"), meter_id==1) %>% 
  select(-meter_id) %>% 
  rename("customer_id" = "household_id",
         "general_supply_kwh" = "kwh") %>% 
  as_tsibble(key = customer_id, index = reading_datetime)


sm_cust_data <- elec

sm_cust_data_list <- sm_cust_data %>% group_split(customer_id)


### Extract flags from simulation scenario
# Run wpd_threshold on all of them, each household can form a separate seed on cluster

all_house_nrep_orig_list_5 <-  sm_cust_data_list

all_house_harmony_wpd <- mclapply(all_house_nrep_orig_list_5,
                                  function(x)
                                    wpd = wpd_threshold(x,
                                                  harmony_tbl = harmonies,
                                                  response = general_supply_kwh, nsamp = nsamp)) %>%
  bind_rows(.id = "customer_id")

# Find harmonies that are not significant for all, whichever columns are integer in the following table drop them


significant_tag <- function(x){
  !all(is.na(str_extract(x, "\\*")))
}


m <- all_house_harmony_wpd %>%
  mutate(harmony_name = paste(facet_variable, x_variable, sep = "-")) %>%
  pivot_wider(id_cols = c(-2, -3, -4, -5, -6),
              names_from = harmony_name,
              values_from = select_harmony)



sig_harmonies <- m %>%
  select_if(significant_tag) %>%
  apply(2, parse_number) %>%
  as_tibble() %>%
  mutate(customer_id = m$customer_id) %>%
  select(customer_id, everything())

# 4. Then you have observations and variables(harmonies) for which wed numbers are there
# 5. Cluster analysis on this a) with these variables and deciles b) 2-step clustering with deciles as input only in step 2

# find percentiles of usage for each customer

find_quantiles <- function(x, probs = seq(0, 1, 0.25))
{
  quantile(x,  probs = probs, na.rm = TRUE)
}

all_house_percentiles <- lapply(all_house_nrep_orig_list_5, function(x, probs = seq(0, 1, 0.1)){
  find_quantiles(x$general_supply_kwh)
}) %>% bind_rows() %>% mutate(customer_id = m$customer_id) %>%
  select(customer_id, everything())

# percentile and harmony data together

raw_data_cluster <- sig_harmonies %>%
  full_join(all_house_percentiles, by = "customer_id")


saveRDS(raw_data_cluster, "data/raw_data_cluster.rds")

# Approach 1
# perform Multi-dimensional scaling and hierarchical clustering

mds<- raw_data_cluster %>%
  dist() %>%
  cmdscale() %>%
  as_tibble()

colnames(mds) <- c("mds_1", "mds_2")

mds <- mds %>%
  mutate(customer_id = as.character(unique(raw_data_cluster$customer_id))) %>%
  select((customer_id)
         , mds_1, mds_2)

ggpubr::ggscatter(mds,
                  x = "mds_1",
                  y = "mds_2",
                  label = rownames(mds),
                  size = 1,
                  repel = TRUE)

# hiearchical clustering as well

d = raw_data_cluster %>%
  dist()

hc = stats::hclust(d,method="complete")
#plot(hc)
groups<-cutree(hc, k=kstep1)

all_data_cluster <- cbind(raw_data_cluster, groups) %>%
  left_join(mds, by = "customer_id") %>%
  mutate(groups = as.factor(groups))



q2 <- ggpubr::ggscatter(all_data_cluster,
                        x = "mds_1",
                        y = "mds_2",
                        label = rownames(mds),
                        color = "groups",
                        size = 1,
                        repel = TRUE)


q2


q2_prime <- ggplot(all_data_cluster, aes(x = mds_1, y = mds_2)) + geom_point(aes(color = groups)) + geom_label_repel(aes(label = rownames(all_data_cluster)), box.padding   = 0.35, point.padding = 0.5,segment.color = 'grey50') + theme_bw()

# Approach 2
# perform M2-step hierarchical clustering, first just on harmonies and then on percentiles

mds <- sig_harmonies %>%
  dist() %>%
  cmdscale() %>%
  as_tibble()

colnames(mds) <- c("mds_1", "mds_2")

mds <- mds %>%
  mutate(customer_id = as.character(unique(raw_data_cluster$customer_id))) %>%
  select((customer_id)
         , mds_1, mds_2)

ggpubr::ggscatter(mds,
                  x = "mds_1",
                  y = "mds_2",
                  label = rownames(mds),
                  size = 1,
                  repel = TRUE)

d = raw_data_cluster %>%
  dist()

hc = stats::hclust(d,method="complete")
#plot(hc)
groups<-cutree(hc, k=kstep1)

all_data_cluster <- cbind(sig_harmonies, groups) %>%
  left_join(mds, by = "customer_id") %>%
  mutate(groups = as.factor(groups))

all_data_cluster_list <- all_data_cluster %>%
  full_join(all_house_percentiles, by = "customer_id") %>%
  group_split(groups)


all_data_cluster_step2 <- lapply(all_data_cluster_list,
                                 function(x){
                                   
                                   if(nrow(x)>2){
                                   mds <- x %>%
                                     dist() %>%
                                     cmdscale() %>%
                                     as_tibble()
                                   
                                   colnames(mds) <- c("mds_1", "mds_2")
                                   
                                   mds <- mds %>%
                                     mutate(customer_id = as.character(unique(x$customer_id))) %>%
                                     select((customer_id)
                                            , mds_1, mds_2)
                        
                                   
                                   d = x %>%
                                     dist()
                                   
                                   hc = stats::hclust(d,method="complete")
                                   #plot(hc)
                                   groups2<-cutree(hc, k=kstep2)
                                   }
                                   else {
                                     groups2 <- 1
                                   }
                                   
                                   all_data_cluster <- cbind(x, groups2) %>%
                                     left_join(mds, by = "customer_id")
                                 }
) %>% bind_rows() %>%
  mutate(group_name = as.factor(paste0("main = ", groups, ", sub= ", groups2)))





q3 <- ggpubr::ggscatter(all_data_cluster_step2,
                        x = "mds_1.x",
                        y = "mds_2.x",
                        label = rownames(all_data_cluster),
                        color = "groups",
                        size = "groups2",
                        repel = TRUE)

library(ggrepel)

q4 <- ggplot(all_data_cluster_step2, aes(x = mds_1.x, y = mds_1.y)) + geom_point(aes(color = groups, size = as.factor(groups2))) + geom_label_repel(aes(label = rownames(all_data_cluster)), box.padding   = 0.35, point.padding = 0.5,segment.color = 'grey50') + theme_bw()


q3


q2_prime + theme(legend.position = "bottom") + q4 + theme(legend.position = "bottom")

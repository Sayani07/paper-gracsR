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

set.seed(9999)

# parameter set
nsamp = 100 # number of samples based on which thresholds are decided
ncust = 6 # number of customers for trial
nrep = 3 # number of error added usage for each customer
kstep1 = 3 # number of clusters in step1/ only step
kstep2 = 2 # number of clusters in step2 (this will actually depend on the number of observations in the cluster after step1)


# change path while running it on HPC
simtable<-read_csv(here('sim_table/sim_table.csv'))

# <needed>
harmonies<-read_csv(here('sim_table/harmonies.csv'))

sm_cust_data <- read_rds(here("data/sm_cust_data.rds"))

sm_cust_data_list <- sm_cust_data %>% group_split(customer_id)

# Add some error distribution to each of them before trying out the clustering process

# function to generate some error to the usage for each customer
# number of times to repeat
# names of customer id when repeated

add_error_unif <- function(x){
 y = runif(nrow(x), 0.01, 0.05)
 x$general_supply_kwh + y
}

# generate nrep times distribution for one household

one_house_nrep <- function(x, nrep = 3)
{
  onerep = lapply(1:nrep, function(y){
    bind_cols(customer_id  =  paste(x$customer_id, y, sep = "-"),
              reading_datetime =  x$reading_datetime,
              general_supply_kwh = rep(add_error_unif(x), 1))
  })
    onerep
}


# generate nrep times distribution for many households

many_house_nrep <- lapply(sm_cust_data_list, function(x, nrep = 3){
  one_house_nrep(x, nrep = nrep)
}
  ) %>% bind_rows


all_house_nrep_orig <- bind_rows(sm_cust_data, many_house_nrep)


saveRDS(all_house_nrep_orig, "data/all_house_nrep_orig.rds") # seed and dist not included yet



all_house_nrep_orig_list <- all_house_nrep_orig %>%
  group_split(customer_id)

all_house_nrep_orig_list_5 <- all_house_nrep_orig_list[1:(ncust*(nrep+1))]

### Extract flags from simulation scenario
# Run wpd_threshold on all of them, each household can form a separate seed on cluster

all_house_harmony_wpd <- mclapply(all_house_nrep_orig_list_5,
         function(x)
wpd_threshold(x,
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

colnames(mds) <- c("Dim.1", "Dim.2")

mds <- mds %>%
  mutate(customer_id = as.character(unique(raw_data_cluster$customer_id))) %>%
  select((customer_id)
         , Dim.1, Dim.2)

ggpubr::ggscatter(mds,
                  x = "Dim.1",
                  y = "Dim.2",
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
                        x = "Dim.1",
                        y = "Dim.2",
                        label = rownames(mds),
                        color = "groups",
                        size = 1,
                        repel = TRUE)


q2

# Approach 2
# perform M2-step hierarchical clustering, first just on harmonies and then on percentiles

mds <- sig_harmonies %>%
  dist() %>%
  cmdscale() %>%
  as_tibble()

colnames(mds) <- c("Dim.1", "Dim.2")

mds <- mds %>%
  mutate(customer_id = as.character(unique(raw_data_cluster$customer_id))) %>%
  select((customer_id)
         , Dim.1, Dim.2)

ggpubr::ggscatter(mds,
                  x = "Dim.1",
                  y = "Dim.2",
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

                                   mds <- x %>%
                                     dist() %>%
                                     cmdscale() %>%
                                     as_tibble()

                                   colnames(mds) <- c("Dim.1", "Dim.2")

                                   mds <- mds %>%
                                     mutate(customer_id = as.character(unique(x$customer_id))) %>%
                                     select((customer_id)
                                            , Dim.1, Dim.2)

                                   d = x %>%
                                     dist()

                                   hc = stats::hclust(d,method="complete")
                                   #plot(hc)
                                   groups2<-cutree(hc, k=kstep2)

                                   all_data_cluster <- cbind(x, groups2) %>%
                                     left_join(mds, by = "customer_id")
                                 }
       ) %>% bind_rows() %>%
  mutate(group_name = as.factor(paste0("main = ", groups, ", sub= ", groups2)))





q2 <- ggpubr::ggscatter(all_data_cluster_step2,
                        x = "Dim.1.x",
                        y = "Dim.2.x",
                        label = rownames(raw_data_cluster),
                        color = "groups",
                        size = "groups2",
                        repel = TRUE)


q2

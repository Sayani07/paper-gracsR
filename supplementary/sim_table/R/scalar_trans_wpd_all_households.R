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


set.seed(9999)

# change path while running it on HPC
simtable<-read_csv('paper/sim_table/sim_table.csv')
harmonies<-read_csv('paper/sim_table/harmonies.csv')
sm_cust_data <- read_rds("paper/data/sm_cust_data.rds")

### Extract flags from simulation scenario

# scen<- 170 #If running within R uncomment this.  This will only run first scenario
#scen<-as.numeric(commandArgs()[[6]]) # If running batch job uncomment this

simj<-simtable[scen,] #Extract row of table
customer_idj <- simj$customer_id
facetj <- simj$facet_variable
xj <- simj$x_variable
harmony_tbl_rowj <- simj[-c(1:2)]
dataj <- sm_cust_data %>% filter(customer_id %in% customer_idj)

# all_data <- create_harmony_tbl_data(dataj,
#                     harmonies,
#                     general_supply_kwh)  
#   

all_data <- create_harmony_data(dataj,
                                    harmony_tbl_rowj,
                                    general_supply_kwh)  

  
# wpd_norm <- mclapply(all_data, function(x){
  

wpd_norm <- compute_pairwise_norm_scalar(all_data, 
                        gran_x = "id_x",
                        gran_facet = "id_facet",
                        response = sim_data,
                        quantile_prob =
                          seq(0.01, 0.99, 0.01), 
                        nperm = 200)

  
wpd_harmonies <- bind_cols(customer_id = customer_idj, 
                           harmony_tbl_rowj,
          wpd_norm = wpd_norm %>% unlist() %>% round(digits = 3))


saveRDS(wpd_harmonies,
        paste0('paper/sim_table/results/',
               customer_idj,"-",
               facetj, "-", 
               xj, '.rds')) # seed and dist not included yet

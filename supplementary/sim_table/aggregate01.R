# contains function to aggregate data each for from N(0,1), N(0,5), N(5,1), N(5,5), Gamma(0.5, 1), Gamma(2, 1)
#  aggregated data from the file simulations/results_norm_scalar/norm/folder_name and tidy them up
library(dplyr)
library(here)
library(readr)
library(rlang)
library(purrr)
library(rlang)
  
  all_files = list.files(path = paste0("paper/sim_table/results_norm_scalar/"), 
                         pattern = ".rds")
  
  names_levels <- map_dfr(all_files, 
                          function(x){
                            z = str_split(str_remove(x, 
                                                     ".rds"), 
                                          "-") %>% unlist()
  
                            bind_cols(customer_id = as.numeric(z[1]),
                                      facet_variable = z[2],
                                      x_variable = z[3])
                          })
  
  # len_file = read_rds(("simulations/results_norm_scalar/norm/tuning_param/2_2_tuning_param.rds"))
  # 
  # names_rep <- names_levels %>% slice(rep(1:n(), each = nrow(len_file)))
  
  
  all_files_path <- paste0("paper/sim_table/results_norm_scalar/",
                           all_files)  
  
  
  all_data <- lapply(1:length(all_files_path), function(x){
    
    data = all_files_path %>% magrittr::extract2(x) %>% 
      readRDS()
    # 
    # names = names_levels %>% magrittr::extract(x,)
    # names_rep =   names %>% slice(rep(1:n(), each = nrow(data)))
    # bind_cols(names_rep, data)
  }) %>% 
    bind_rows() %>%
    arrange(customer_id)
  
  write_rds(all_data, "paper/sim_table/all_data_scalar.rds")

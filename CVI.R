library(readr)
library(tidyverse)
library(fpc)
library(patchwork)

append_files_plot <- function(folder_name, path){
  all_files = list.files(path = paste0(folder_name,path), 
                         pattern = "data_validation_")
  
  names_levels <- map_dfr(all_files, 
                          function(x){
                            z = str_split(str_remove(x, ".rds"), "_") %>% 
                              unlist()
                            bind_cols(index = z[3])
                          })
  
  all_files_path <- paste0(folder_name, path,
                           all_files)  
  
  
  all_data <- lapply(1:length(all_files_path), function(x){
    
    data = all_files_path %>% magrittr::extract2(x) %>% 
      readRDS()
    
    names = names_levels %>% magrittr::extract(x,)
    names_rep =   names %>% slice(rep(1:n(), each = nrow(data)))
    bind_cols(names_rep, data)
  }) %>% bind_rows() 
# 
# all_data <- append_files("js-nqt")

# compute all inter cluster distance "from" group
all_mat_mat_from <- all_data%>%
  group_by(index, gran, group_item1, group_item2) %>% 
  filter(group_item1!= group_item2) %>% 
  summarise(sum = sum(distance)) %>% 
  pivot_wider(names_from = group_item2, values_from = sum) %>% ungroup %>% 
  replace(is.na(.), 0) %>% 
  mutate(distance = rowSums(across(where(is.numeric)))) %>% 
  dplyr::select(c(index, gran, group_item1, distance)) %>% 
  rename("group"="group_item1")


# compute all inter cluster distance "to" group

all_mat_mat_to <- all_data%>%
  group_by(index, gran, group_item1, group_item2) %>% 
  filter(group_item1!= group_item2) %>% 
  summarise(sum = sum(distance)) %>% 
  pivot_wider(names_from = group_item1, values_from = sum) %>% ungroup %>% 
  replace(is.na(.), 0) %>% 
  mutate(distance = rowSums(across(where(is.numeric)))) %>% 
  dplyr::select(c(index, gran, group_item2, distance))%>% 
  rename("group"="group_item2")

# compute all inter cluster distance to and from

data_pcp <- bind_rows(all_mat_mat_from, all_mat_mat_to) %>% 
  group_by(index, gran, group) %>% 
  summarise(inter_distance = sum(distance)) %>% 
  pivot_wider(names_from = gran, values_from = inter_distance) %>% ungroup %>% 
  mutate(group = as.factor(group))

parcoord <- GGally::ggparcoord(data_pcp ,
                               columns = 3:ncol(data_pcp),
                               groupColumn = "group",
                               showPoints = FALSE, 
                               alphaLines = 0.3,
                               scale = "globalminmax"
)
parcoord
}
design1 <- append_files_plot(folder_name = "js-nqt", path = "/3gran_change_5D/")
design2 <- append_files_plot(folder_name = "js-nqt", path = "/2gran_change_4D/")
design3 <- append_files_plot(folder_name = "js-nqt", path = "/1gran_change_5D/")

(design1 + design2 + design3)&theme(legend.position = "bottom") 


design1 <- append_files_plot(folder_name = "js-robust", path = "/3gran_change_5D/")
design2 <- append_files_plot(folder_name = "js-robust", path = "/2gran_change_4D/")
design3 <- append_files_plot(folder_name = "js-robust", path = "/1gran_change_5D/")

(design1 + design2 + design3)&theme(legend.position = "bottom") 


design1 <- append_files_plot(folder_name = "js-nqt", path = "/3gran_change_5D/")
design2 <- append_files_plot(folder_name = "js-nqt", path = "/2gran_change_4D/")
design3 <- append_files_plot(folder_name = "js-nqt", path = "/1gran_change_5D/")

(design1 + design2 + design3)&theme(legend.position = "bottom")




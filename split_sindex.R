sindex_data <- function(data){
  
  total_distance <- data %>%
  group_by(item1, item2) %>% 
  summarise(total = sum(distance), .groups = "drop") %>% ungroup()

total_distance_wide <- total_distance%>% 
  pivot_wider(names_from = item2, values_from = total)

rownames(total_distance_wide) <- total_distance_wide$item1

distance_lowertriangle <- total_distance_wide %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  tibble::rownames_to_column() %>%  
  dplyr::select(-item1) %>% 
  pivot_longer(-rowname) %>% 
  pivot_wider(names_from=rowname, values_from=value) 

rownames(distance_lowertriangle) <- total_distance_wide$item1

df <- distance_lowertriangle[-1] %>% as.matrix()
DM <- matrix(0, ncol(distance_lowertriangle), ncol(distance_lowertriangle)) 
DM[lower.tri(DM)] = df[lower.tri(df, diag=TRUE)] # distance metric
#print(i)
distance_matrix = as.dist(DM)

groups <- bind_rows(data %>%
                      distinct(item1, group_item1) %>% 
                      rename("item" = "item1",
                             "group" = "group_item1"),
                    data %>% 
                      distinct(item2, group_item2) %>%
                      rename("item" = "item2",
                             "group" = "group_item2")) %>% distinct(item, group) 


k = array()
for(j in 2:10)
{
  group <- distance_matrix %>% 
    hclust (method = "ward.D") %>% 
    cutree(k=j)
  #print(j)
  p <- cluster.stats(distance_matrix,
                     clustering = group, 
                     silhouette = TRUE)
  k[j]=p$sindex
}
k %>% as_tibble() %>% set_names(c("sindex"))%>%
  mutate(k = row_number())
}




data1 <- sindex_data(read_rds("validation/js-robust/3gran_change_5D/data_validation_25.rds")) %>% mutate(diff = 1 , design= "S1")
data2 <-sindex_data(read_rds("validation/js-robust/3gran_change_5D/data_validation_26.rds")) %>% mutate(diff = 2 , design= "S1")
data3 <-sindex_data(read_rds("validation/js-robust/3gran_change_5D/data_validation_27.rds")) %>% mutate(diff = 5 , design= "S1")

data4 <- sindex_data(read_rds("validation/js-robust/2gran_change_4D/data_validation_7.rds")) %>% mutate(diff = 1 , design= "S2")
data5 <- sindex_data(read_rds("validation/js-robust/2gran_change_4D/data_validation_17.rds")) %>% mutate(diff = 2 , design= "S2")
data6 <- sindex_data(read_rds("validation/js-robust/2gran_change_4D/data_validation_9.rds")) %>% mutate(diff = 5 , design= "S2")


data7 <- sindex_data(read_rds("validation/js-robust/1gran_change_5D/data_validation_16.rds"))%>% mutate(diff = 1 , design= "S3")
data8 <-sindex_data(read_rds("validation/js-robust/1gran_change_5D/data_validation_17.rds"))%>% mutate(diff = 2 , design= "S3")
data9 <-sindex_data(read_rds("validation/js-robust/1gran_change_5D/data_validation_18.rds"))%>% mutate(diff = 5 , design= "S3")

all_data <- bind_rows(data1, data2, data3, data4, data5, data6, data7, data8, data9) %>% rename("Scenario" ="design")


sindex_plot <- ggplot(all_data,
                      aes(x = k,
                          y = sindex)) +
  geom_line(size = 1, aes(group = Scenario))+
  facet_grid(diff~Scenario, labeller = "label_value", scales = "free_y") +
  theme_bw()+
  theme(legend.position = "bottom") +
  scale_color_brewer(palette = "Dark2")+
  xlab("number of clusters") + ylab("sindex")+
  scale_x_continuous(breaks = seq(2, 10, 1), minor_breaks = 1)

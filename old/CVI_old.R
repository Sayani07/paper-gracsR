# compute dunn and avg silhoutte

library(tidyverse)
library(readr)
g = read_rds("js-nqt/3gran_change_5D/data_validation_25.rds")

total_distance <- g %>%
  group_by(item1, item2) %>% 
  summarise(total = sum(distance)) %>% ungroup()
  
total_distance_wide <- total_distance%>% 
  pivot_wider(names_from = item2, values_from = total)


rownames(total_distance_wide) <- total_distance_wide$item1

mds_data <- total_distance_wide %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  tibble::rownames_to_column() %>%  
  dplyr::select(-item1) %>% 
  pivot_longer(-rowname) %>% 
  pivot_wider(names_from=rowname, values_from=value) 

rownames(mds_data) <- total_distance_wide$item1

df <- mds_data[-1] %>% as.matrix()
DM <- matrix(0, ncol(mds_data), ncol(mds_data))
DM[lower.tri(DM)] = df[lower.tri(df, diag=TRUE)] # distance metric
f = as.dist(DM)

mds <- f %>%
  cmdscale() %>%
  as_tibble()

colnames(mds) <- c("Dim.1", "Dim.2")

groups<-bind_rows(g %>%
  distinct(item1, group_item1) %>% 
  rename("item" = "item1",
         "group" = "group_item1"),
g %>% 
  distinct(item2, group_item2) %>%
  rename("item" = "item2",
         "group" = "group_item2")) %>% distinct(item, group) 
  


all_data_cluster <- cbind(groups, mds) %>%
  mutate(group = as.factor(group)) %>% as_tibble()


mds_plot_10 <- ggplot(all_data_cluster,
                      aes(x = Dim.1,
                          y = Dim.2,
                          color = group,
                          label = rownames(mds))) +
  geom_point(size = 2, alpha = 0.8) +
  geom_text(check_overlap = TRUE)  +
  theme_classic()+
  theme(legend.position = "bottom") +
  scale_color_brewer(palette = "Dark2")

mds_plot_10

# not symmetric because distance from one group to another might not be symmetric
m <- g %>%
  group_by(gran, group_item1, group_item2) %>% 
  filter(group_item1!= group_item2) %>% 
  summarise(sum = sum(distance)) %>% 
  pivot_wider(names_from = group_item2, values_from = sum) %>% 
  replace(is.na(.), 0)%>%
  mutate(distance = rowSums(across(where(is.numeric)))) %>% 
  pivot_wider(c(gran, 
                group_item1, distance), 
              names_from = group_item1, 
              values_from = distance) %>% 
  ungroup()




t(t(m[,-1])*100/rowSums(t(m[,-1]))) %>% as_tibble() %>% 
  mutate(gran = m$gran) 


m <- all_data %>%
  group_by(index, gran, group_item1, group_item2) %>% 
  filter(group_item1!= group_item2) %>% 
  summarise(sum = sum(distance))%>% 
  pivot_wider(names_from = group_item2, values_from = sum)%>% 
  replace(is.na(.), 0) %>%
  tibble::rownames_to_column() %>%  
  dplyr::select(-group_item1) %>% 
  pivot_longer(-c(rowname, -index))


%>% 
  pivot_wider(names_from=rowname, values_from=value) 

m %>% ungroup %>% dplyr::select(-c(index, gran, group_item1)) %>% as.matrix()



%>%
  mutate(distance = rowSums(across(where(is.numeric))))

m[]




%>% 
  tibble::rownames_to_column()




mds_data <- total_distance_wide %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  tibble::rownames_to_column() %>%  
  dplyr::select(-item1) %>% 
  pivot_longer(-rowname) %>% 
  pivot_wider(names_from=rowname, values_from=value) 


%>% 
  pivot_wider(c(index, gran, 
                group_item1, distance), 
              names_from = group_item1, 
              values_from = distance) %>% 
  ungroup()



all_data %>%
  group_by(index, gran, group_item1, group_item2) %>% 
  filter(group_item1!= group_item2) %>% 
  summarise(sum = sum(distance)) %>% 
  group_by(index, gran, group_item2) %>% 
  summarize(inter_distance = sum(sum))

m_index_gran <- m %>% group_by(index, gran) %>%
  summarise(dist = sum( `1`),
            dist = sum( `1`),
            dist = sum( `1`),
            dist = sum( `1`)
  )
mutate(total_distance = rowSums(across(where(is.numeric))))

t(t(m[,-1])*100/rowSums(t(m[,-1]))) %>% as_tibble() %>% 
  mutate(gran = m$gran) 



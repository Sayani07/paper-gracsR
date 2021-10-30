library(tidyverse)
library(readr)

mds_data <- function(data){
total_distance <- data %>%
  group_by(item1, item2) %>% 
  summarise(total = sum(distance), .groups = "drop") %>% ungroup()

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

groups<-bind_rows(data %>%
                    distinct(item1, group_item1) %>% 
                    rename("item" = "item1",
                           "group" = "group_item1"),
                  data %>% 
                    distinct(item2, group_item2) %>%
                    rename("item" = "item2",
                           "group" = "group_item2")) %>% distinct(item, group) 



all_data_cluster <- cbind(groups, mds) %>%
  mutate(group = as.factor(group)) %>% as_tibble()
}


# original simulation table
niter <- c(5, 50, 100) #number of series you are clustering
nT <-  c(300, 1000, 5000) # length of the time series
mean_diff <- c(1, 2, 5) # difference between consecutive categories

simtable <- expand.grid(mean_diff = mean_diff,
                        niter = niter,
                        #time_series = time_series, 
                        nT = nT)

data1 <- mds_data(read_rds("js-nqt/3gran_change_5D/data_validation_25.rds")) %>% mutate(diff = 1 , design= "a")
data2 <-mds_data(read_rds("js-nqt/3gran_change_5D/data_validation_26.rds")) %>% mutate(diff = 2 , design= "a")
data3 <-mds_data(read_rds("js-nqt/3gran_change_5D/data_validation_27.rds")) %>% mutate(diff = 5 , design= "a")

data4 <- mds_data(read_rds("js-nqt/2gran_change_4D/data_validation_7.rds")) %>% mutate(diff = 1 , design= "b")
data5 <- mds_data(read_rds("js-nqt/2gran_change_4D/data_validation_17.rds")) %>% mutate(diff = 2 , design= "b")
data6 <- mds_data(read_rds("js-nqt/2gran_change_4D/data_validation_9.rds")) %>% mutate(diff = 5 , design= "b")


data7 <- mds_data(read_rds("js-nqt/1gran_change_5D/data_validation_16.rds"))%>% mutate(diff = 1 , design= "c")
data8 <-mds_data(read_rds("js-nqt/1gran_change_5D/data_validation_17.rds"))%>% mutate(diff = 2 , design= "c")
data9 <-mds_data(read_rds("js-nqt/1gran_change_5D/data_validation_18.rds"))%>% mutate(diff = 5 , design= "c")

all_data <- bind_rows(data1, data2, data3, data4, data5, data6, data7, data8, data9) %>% rename("Scenario" ="design")


mds_plot_10 <- ggplot(all_data,
                      aes(x = Dim.1,
                          y = Dim.2,
                          color = group)) +
  geom_point(size = 2, alpha = 0.5, shape = 3) +
  facet_grid(diff~Scenario, labeller = "label_both") +
  #geom_text(check_overlap = TRUE)  +
  theme_bw()+
  theme(legend.position = "bottom") +
  scale_color_brewer(palette = "Dark2")+
  ylab("mds1") + xlab("mds2")

mds_plot_10


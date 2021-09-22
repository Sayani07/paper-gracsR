# script to assimilate results from 02_algo1_moy_356cust_robust.R, 02_algo1_hod_356cust_robust.R and 02_algo1_wkndwday_356cust_robust.R
# write code after running the results for each of the above mentioned code

library(tidyverse)
library(gravitas)
library(tsibble)
library(ggpubr)

wkndwday <- read_rds("data/dist_gran_wkndwday_356cust_robust.rds") %>% broom::tidy()
moy <- read_rds("data/dist_gran_moy_356cust.rds") %>% broom::tidy()
hod <- read_rds("data/dist_gran_hod_356cust_robust.rds") %>% broom::tidy()

total_distance <- wkndwday %>% 
  left_join(moy, by = c("item1", "item2")) %>% 
  left_join(hod, by = c("item1", "item2")) %>% 
  rename("wkndwday" ="distance.x",
         "moy" = "distance.y",
         "hod" = "distance") %>% 
  mutate(total = wkndwday+moy+hod) 


dist_mat <- total_distance %>% 
  pivot_wider(-c(3, 4, 5), names_from = item2, values_from=total) 

dist_mat[-1] %>% mutate_all(~replace(., is.na(.), 0))  %>% gracsr::clust_gran()


hc = stats::hclust(dist_mat[-1] %>% mutate_all(~replace(., is.na(.), 0)))

mds <- dist_mat[-1] %>% mutate_all(~replace(., is.na(.), 0)) %>% cmdscale() %>%
  as_tibble() %>% 
  bind_cols(item1 = dist_mat$item1) %>% 
  mutate(serial_id = row_number())

colnames(mds) <- c("Dim.1", "Dim.2", "item1", "serial_id")

mds_scatter <- ggscatter(mds, x = "Dim.1", y = "Dim.2", 
          label = "serial_id",
          size = 0.5,
          repel = TRUE) 


# Take 37, 39, 60, 54, 56
# Take 305, 304314, 273
#
#
q2_prime <- ggplot(mds, aes(x = Dim.1, y = Dim.2)) + geom_point(color = "red", size = 0.5) + ggrepel::geom_text_repel(min.segment.length = 0, seed = 42, box.padding = 0.5, aes(label = serial_id), max.overlaps = Inf)+ theme_bw() 
#+ geom_segment(color = "grey", alpha = 0.5)

ggplot(mds, aes(x = Dim.1, y = Dim.2, label = serial_id)) + geom_point(color = "red", size = 0.5)
library(plotly)
ggplotly(mds_scatter)

fig <- plot_ly(mds, x = ~Dim.1, y = ~ Dim.2, text = mds$item1)



# <- ggscatmat(total_distance, columns=3:5) +
# scale_colour_brewer(palette="Set2")


library(here)
library(readr)
library(tidyverse)
library(plotly)
library(lubridate)
library(tsibble)
library(gravitas)
# run for  norm MMPD files aggregation
#all_data <- read_rds("simulations/result_report/all_data_ norm.rds")
# run for  norm max pairwise distances files aggregation
set.seed(7777)
all_data <- read_rds("paper/sim_table/all_data.rds")
sm_cust_data <- read_rds("paper/data/sm_cust_data.rds")
set.seed(7777)
sm_unique_cust <- sm_cust_data %>% distinct(customer_id)
sm_30 <- sample(sm_unique_cust$customer_id, 30)
sm_cust_samp <- sm_cust_data %>% 
  #filter(customer_id %in% sm_30) %>% # for demo show 30 customers
  filter(year(reading_datetime) == 2012) %>% #for demo just show one column
  as_tsibble(index = reading_datetime,
             key = customer_id) %>% 
  create_gran("day_week") %>% 
  create_gran("hour_day")

#all_data <- write_rds(all_data, "paper/sim_table/all_data.rds")

facet_variable = "day_week"
x_variable = "hour_day"

one_harmony_data <- all_data %>%
  #filter(customer_id %in% sm_30) %>% # for demo show 30 customers
  filter(facet_variable == "day_week",
                    x_variable == "hour_day") %>% 
  mutate(customer_id = as.character(customer_id))

mds<- one_harmony_data %>%
  select(customer_id, facet_variable, x_variable, wpd_norm) %>% 
  dist() %>%          
  cmdscale() %>%
  as_tibble()

colnames(mds) <- c("Dim.1", "Dim.2")
rownames(mds) <- unique(one_harmony_data$customer_id)
mds <- mds %>%
  mutate(customer_id = as.character(unique(one_harmony_data$customer_id))) %>%
  select((customer_id)
         , Dim.1, Dim.2)

ggpubr::ggscatter(mds,
                  x = "Dim.1",
                  y = "Dim.2", 
                  label = rownames(mds),
                  size = 1,
                  repel = TRUE) 

# clustering as well
  
  d = one_harmony_data %>%
    select(customer_id, facet_variable, x_variable, wpd_norm) %>% 
    dist()
  
  hc = stats::hclust(d,method="complete")
  plot(hc)
  groups<-cutree(hc, k=4)
  
  all_data_cluster <- cbind(one_harmony_data, groups) %>% 
    left_join(mds, by = "customer_id") %>% 
    mutate(groups = as.factor(groups))



ggpubr::ggscatter(all_data_cluster,
                  x = "Dim.1",
                  y = "Dim.2", 
                  label = rownames(mds),
                  color = "groups",
                  size = 1,
                  repel = TRUE) 

# data you want to show
# 1.linear scale (from base data)
# 2.cyclic scale (from all data)
# all linked with customer-id
#  MDS (co-ordinates from here)
#  and maybe cluster data which can come later


raw_data <- sm_cust_samp %>% 
  left_join(mds, by = "customer_id") 

# using thr demo-crosstalk example

d <- highlight_key(raw_data, ~customer_id)

line_plot <- plot_ly(d, colors = "Set1", x = ~reading_datetime, y = ~general_supply_kwh) %>%
  add_lines() %>% 
  layout(
    xaxis = list(title = "Reading Datetime"),
    yaxis = list(title = "Energy Demand (in Kwh)")
  )

points_plot <- plot_ly(d, colors = "Set1", x = ~Dim.1 , y = ~Dim.2) %>% 
  add_trace() %>%
  layout(
    xaxis = list(title = "Dim.1"),
    yaxis = list(title = "Dim.2")
  )

# make a ggplot object to plot_ly

gravitas_plot <- raw_data %>% 
  highlight_key(~customer_id) %>% 
  {
  ggplot(., aes(x = hour_day, 
                y = general_supply_kwh))+
  geom_boxplot() +
  facet_wrap(~day_week)
  } %>% 
  ggplotly(tooltip = "customer_id")


subplot(points_plot, 
        line_plot,
        titleX = TRUE,
        titleY = TRUE) %>%
  subplot(points_plot, nrows = 2, titleX = TRUE, titleY = TRUE) %>%
  layout(
    title = "Dynamic 2-way ANOVA (click & drag on scatterplot)",
    barmode = "overlay",
    showlegend = FALSE
  ) %>%
  highlight("plotly_selected", opacityDim = 0.6)















  
  
  
  
  
ggpubr::ggscatter(mds_shared_data,
                  x = "Dim.1",
                  y = "Dim.2", 
                  label = rownames(mds),
                  size = 1,
                  repel = TRUE) 

time_series <- base %>%
  group_by(city) %>%
  add_lines(x = ~date, y = ~median)

gg <- ggpubr::ggscatter(mds,
                        x = "Dim.1",
                        y = "Dim.2", 
                        label = rownames(mds),
                        size = 1,
                        repel = TRUE) %>%
  highlight_key(~customer_id) %>% 
  highlight()
  
  add_lines(x = ~ Dim.1, y = ~customer_id)

p1 <- highlight(ggplotly(gg), "plotly_hover")
  

# make the data a sharedData object
# then make a ggplot (gg) using that data ~ same notations
# then use highlight(ggplotly(gg)) for highlighting the plot
  
  
  
# Plot MDS

library(plotly)
df <- ggpubr::ggscatter(mds, x = "Dim.1", y = "Dim.2", 
          label = rownames(mds),
          size = 1,
          repel = TRUE) %>%
  highlight_key() %>% 
  plot_ly()



# declare 'city` as the visual query column
tx <- highlight_key(txhousing, ~city)

# initiate a plotly object

base <- plot_ly(tx, color = I("black")) %>% 
  group_by(city)






# leaflet try

# library(leaflet)
# library(sp)
# mds_new <- mds %>% scale() 
# Sr1 = Polygon(mds)
# 
# Srs1 = Polygons(list(mds), "s1")
# 
# SpP = SpatialPolygons(list(Srs1), 1L)
# 
# leaflet(mds) %>% addPolygons(data = PolygonData)
# leaflet(options = leafletOptions(crs = leafletCRS(crsClass = "L.CRS.Simple"))) %>% addPolygons(data = Srs1)
# mapview(SpP, native.crs = TRUE)




# NEW TRY
# FIRST RAW DATA
sm_cust_data <- read_rds("paper/data/sm_cust_data.rds")

sm <- highlight_key(sm_cust_data, ~customer_id)
gg <- ggplot(sm) + geom_line(aes(reading_datetime, 
                           general_supply_kwh,
                           group = customer_id))
library(crosstalk)
filter <- bscols(
  filter_select("id",
                "select a customer_id", 
                sm,
                ~customer_id),
  ggplotly(gg, dynamicTicks = TRUE),
  widths = c(12, 12)
)



base <- plot_ly(sm, color = I("black")) %>% 
  group_by(customer_id)

# create a time series of all customers
base %>%
  filter_select("id",
                "select a customer_id", 
                sm,
                customer_id) %>%
  add_lines(x = ~reading_datetime, y = ~general_supply_kwh)



# data dictionary


data_dict <- tibble(
  variable = c("day_month",
  "day_week",
  "hour_day",
  "week_month",
  "wknd_wday"),
  abb  = c("dom",
                    "dow",
                    "hod",
                    "wom",
                    "wnd/wd")
)
 

# visualize

rank_data <- all_data %>% 
  left_join(data_dict, by = c("facet_variable" = "variable")) %>%
  left_join(data_dict, by = c("x_variable" = "variable"))  %>% 
  rename("facet" = "abb.x",
         "x" = "abb.y") %>% 
  mutate(pair = paste(facet, x,  sep = "-")) %>% 
  group_by(customer_id) %>% 
  arrange(-wpd_norm) %>% 
  mutate(rank  = row_number()) %>% 
  ungroup() %>% 
  select(customer_id, pair, rank) 
  

rank_data %>% 
  filter(pair == "dom-dow") %>% 
  ggplot() +
  geom_line(aes(x = as.character(customer_id),
                y = as.factor(rank),
                group = pair,
                color = pair))  + coord_flip()



ggplot(data=rank_data, aes(x = pair, 
                            y = rank)) + geom_boxplot() 

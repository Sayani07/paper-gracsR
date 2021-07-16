## ---- load-lib
library(tidyverse)
library(lubridate)
library(tsibble)
library(fable)
library(patchwork)
library(here)
theme_set(theme_bw())
library(forcats)
library(readr)

##----load-data
load("data/ALL_DATA.Rdata")


##----format-data

smart_meter_data <- EUDMData %>% 
  dplyr::rename_all(tolower) %>% 
  dplyr::mutate(reading_datetime = lubridate::ymd_hms(reading_datetime)) %>% 
  dplyr::arrange(customer_id, reading_datetime) %>% 
  dplyr::group_by(customer_id) %>% 
  dplyr::mutate(reading_datetime = case_when(
    duplicated(reading_datetime) ~ reading_datetime + lubridate::hours(1), 
    TRUE ~ reading_datetime
  ))
  
elec_ts <- smart_meter_data %>% 
  build_tsibble(
    key = customer_id, index = reading_datetime,
    validate = FALSE, ordered = TRUE
  )

readr::write_rds(elec_ts, "data/elec_ts.rds")

## ---- elec-gaps
elec_ts <- read_rds("data/elec_ts.rds")
gap_df <- has_gaps(elec_ts)

nogap <- gap_df %>% filter(.gaps==FALSE)


elec_nogap <- elec_ts %>% 
  filter(customer_id %in% nogap$customer_id)

readr::write_rds(elec_nogap, "data/elec_nogap.rds")

# sum(gap_df$.gaps) / NROW(gap_df)

## ---- count-gaps
count_na_df <- elec_ts %>% 
  count_gaps()


lumped_na_df <- count_na_df %>% 
  mutate(
    customer_id = as.factor(customer_id) %>% 
      fct_lump(264) %>%
      fct_reorder(.n, sum)
  ) 

p_264 <- lumped_na_df %>%
  filter(customer_id != "Other") %>%
  ggplot(aes(x = customer_id)) +
  geom_linerange(aes(ymin = .from, ymax = .to)) +
  geom_point(aes(y = .from), size = 0.6, shape = 4) +
  geom_point(aes(y = .to), size = 0.6, shape = 4) +
  coord_flip() +
  xlab("Top customers with more than
       10% observations missing") +
  ylab("") +  theme(axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none",
    plot.margin = unit(c(0, 0, -1, -1), "line")) + scale_y_datetime(" ",
                   date_labels = "%b %d",
                      breaks = "1 month",
                      date_minor_breaks = "1 week") + 
  theme(panel.grid.major.x =  element_line(colour = "#A9A9A9"),
        panel.grid.minor.x =  element_line(colour = "#D3D3D3"))



p_other <- lumped_na_df %>% 
  filter(customer_id == "Other") %>% 
  ggplot(aes(x = customer_id)) +
  geom_linerange(aes(ymin = .from, ymax = .to), alpha = 0.01) +
  geom_point(aes(y = .from), size = 1.2,
             shape = 4, alpha = 0.1) +
  geom_point(aes(y = .to), size = 1.2, shape = 4, alpha = 0.01) +
  coord_flip() +
  xlab("Rest") +
  ylab("Time gaps") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none"
  )  + scale_y_datetime("Month-year",
                        date_labels = "%b %y",
                        breaks = "1 month",
                        date_minor_breaks = "1 week") + 
  theme(panel.grid.major.x =  element_line(colour = "#A9A9A9"),
        panel.grid.minor.x =  element_line(colour = "#D3D3D3"))+
  theme(axis.text.x = element_text(angle = 90)) 

g = p_264 + p_other + patchwork::plot_layout(ncol = 1, heights = c(10, 1))

ggsave("figs/missing-data.png")

##----percentage_na_df

miss_obs_df <- count_na_df %>% 
  as_tibble() %>% 
  group_by(customer_id) %>% 
summarise(n_miss = sum(.n)) %>% 
  arrange(desc(n_miss))


total_obs_df <- elec_ts %>%
  filter(customer_id %in% miss_obs_df$customer_id) %>% 
  fill_gaps() %>% 
  as_tibble() %>% 
  group_by(customer_id) %>% 
  tally() %>% 
  rename("n_total" = "n") %>% 
  arrange(desc(n_total))
  

percent_obs_df <- total_obs_df %>%
  left_join(miss_obs_df, by = "customer_id") %>% 
  arrange(desc(n_miss), desc(n_total)) %>% 
  mutate(percent_miss = n_miss*100/n_total) %>% 
  mutate(percent_miss = case_when(
    percent_miss>50 ~ ">50%",
    percent_miss>30 ~ "30-50%",
    percent_miss>10 ~ "10-30%",
    percent_miss<10 ~ "<10%",
    TRUE~ as.character(percent_miss)
  )) %>% 
  group_by(percent_miss) %>% 
  tally()


## ----customer-data
# 
# customer_data <- write_rds(CustomerData, "data/customer-data.rds")


customer_data <- read_rds("data/customer-data.rds")

customer_sub_data <- customer_data %>% 
  filter(CUSTOMER_KEY %in% elec_ts$customer_id)


customer_sub_data %>% mutate(
  LOCAL_GOV_AREA_NAME =  if_else(as.character(LOCAL_GOV_AREA_NAME)=="",as.character(TRIAL_REGION_NAME),as.character(LOCAL_GOV_AREA_NAME))) %>% 
  group_by(LOCAL_GOV_AREA_NAME) %>% 
  tally() %>% arrange(desc(n))

## ---- elec-raw

sm_50 <- elec %>% 
  distinct(customer_id) %>% 
  slice(1:50)

elec %>%
  filter(customer_id %in% sm_50$customer_id) %>% 
  tibble() %>% 
  ggplot(aes(x=reading_datetime, 
             y= general_supply_kwh), alpha = 0.5) + 
  geom_line() +
  scale_fill_gradient2() +
  facet_wrap(~customer_id, ncol= 2) +
  theme_void() +
  theme(strip.text.x = element_blank()) 

## ---- elec-raw-all

sm_50 <- elec %>% 
  distinct(customer_id) %>% 
  slice(1:50)

elec %>%
  filter(customer_id %in% sm_50$customer_id) %>% 
  tibble() %>% 
  ggplot(aes(x = reading_datetime, 
             y= customer_id), alpha = 0.5) 



##----raw-50
sm_50 <- elec_ts %>%
  as_tibble() %>% 
  distinct(customer_id) %>%
  slice_sample(n = 50)

elec_ts %>%
  filter(customer_id %in% sm_50$customer_id) %>%
  tibble() %>%
  ggplot(aes(
    x = reading_datetime,
    y = general_supply_kwh
  ), alpha = 0.5) +
  geom_line() +
  scale_fill_gradient2() +
  facet_wrap(~customer_id, ncol = 2) +
  theme_void() +
  theme(strip.text.x = element_blank()) +
  xlab("Time [30m]") +
  ylab("electricity demand in kwh")

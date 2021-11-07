
set.seed(123)
# DGP2
# 
generate_design_DGP2 <- function(n = 300, #length of time series
                            mu11 = 0,
                            mu12 = 0,
                            mu21 = 0,
                            mu22 = 0 ,
                            mu23 = 0,
                            mu31 = 0,
                            mu32 = 0,
                            mu33 = 0,
                            mu34 = 0,
                            mu35 = 0)
{
  
  n <- n+500 # 500 burning observations
  t <- seq(0, n-1, 1)
  
  g1 <- t %%2
  g2 <- t %%3
  g3 <- t %%5
  
  str_gran <- bind_cols(index = t, g1 = g1, g2 = g2, g3 = g3)
  
  # calculation of g1
  g1_table <- bind_cols(g1 = unique(g1), dist_mean = c(mu11, mu12))
  
  g1_tally <- str_gran %>% group_by(g1) %>%
    count() %>%
    left_join(g1_table, by = "g1")
  
  g1_dist <- g1_tally %>%
    mutate(g1_d = list(rep(g1, each = n)),
           g1_dist = list(rnorm(n, dist_mean, 1))) %>% 
    ungroup() %>% 
    select(g1_d, g1_dist) %>% 
    unnest(cols = c(g1_d, g1_dist))
  
  g1_data <- str_gran %>% arrange(g1) %>% bind_cols(g1_dist = g1_dist$g1_dist) %>% arrange(index)
  
  # calculation of g2
  
  g2_table<- bind_cols(g2 = unique(g2),
                       dist_mean = c(mu21, mu22, mu23))
  
  g2_tally <- str_gran %>%
    group_by(g2) %>%
    count() %>% 
    left_join(g2_table, by = "g2")
  
  g2_dist <- g2_tally %>%
    mutate(g2_d = list(rep(g2, each = n)),
           g2_dist = list(rnorm(n, dist_mean, 1))) %>% 
    ungroup() %>% 
    select(g2_d, g2_dist) %>% 
    unnest(cols = c(g2_d, g2_dist))
  
  g2_data <- str_gran %>% arrange(g2) %>% bind_cols(g2_dist = g2_dist$g2_dist) %>% arrange(index)
  
  # calculation of g3
  
  g3_table<- bind_cols(g3 = unique(g3), dist_mean = c(mu31, mu32, mu33, mu34, mu35))
  
  g3_tally <- str_gran %>% group_by(g3) %>% count() %>% left_join(g3_table, by = "g3")
  
  g3_dist <- g3_tally %>%
    mutate(g3_d = list(rep(g3, each = n)),
           g3_dist = list(rnorm(n, dist_mean, 1))) %>% 
    ungroup() %>% 
    select(g3_d, g3_dist) %>% 
    unnest(cols = c(g3_d, g3_dist))
  
  g3_data <- str_gran %>% arrange(g3) %>% bind_cols(g3_dist = g3_dist$g3_dist) %>% arrange(index)
  
  
  innov_data <- g1_data %>% 
    left_join(g2_data %>% select(index, g2_dist), by = "index") %>% 
    left_join(g3_data %>% select(index, g3_dist), by = "index")
  
  # ar 0.1 ma 0.3
  # list(ar = c(0.8897, -0.4858), ma = c(-0.2279, 0.2488))
  
  if(time_seriesj=="arma22"){
    ts_data <- arima.sim(n=n, list(ar = c(0.8897, -0.4858), ma = c(-0.2279, 0.2488)),
                         innov =  innov_data$g1_dist + 
                           innov_data$g2_dist +
                           innov_data$g3_dist)
  }
  else
  {
    ts_data <- arima.sim(n=n, list(ar = c(0.3), ma = c(0)),
                         innov =  innov_data$g1_dist + 
                           innov_data$g2_dist +
                           innov_data$g3_dist)
  }
  
  
  nd_time <- innov_data %>% bind_cols(ts = as.numeric(ts_data))
  nd_time[-c(1:500),] %>% mutate(index = index - 500)
  
  # 500 burning observations considered
}

time_seriesj = "arma22"
  
design5 <- generate_design_DGP2(n=300,mu12=2, mu22=2, mu23 =4, mu32 = 2,mu33 = 4, mu34 = 2)

# DGP3


generate_design_DGP3 <- function(n = 300, #length of time series
                            mu11 = 0,
                            mu12 = 0,
                            mu21 = 0,
                            mu22 = 0 ,
                            mu23 = 0,
                            mu31 = 0,
                            mu32 = 0,
                            mu33 = 0,
                            mu34 = 0,
                            mu35 = 0)
{
  
  n <- n # 500 burning observations
  t <- seq(0, n-1, 1)
  
  g1 <- t %%2
  g2 <- t %%3
  g3 <- t %%5
  
  str_gran <- bind_cols(index = t, g1 = g1, g2 = g2, g3 = g3)
  
  # calculation of g1
  g1_table <- bind_cols(g1 = unique(g1), dist_mean = c(mu11, mu12))
  
  g1_tally <- str_gran %>% group_by(g1) %>%
    count() %>%
    left_join(g1_table, by = "g1")
  
  g1_dist <- g1_tally %>%
    mutate(g1_d = list(rep(g1, each = n)),
           g1_dist = list(rnorm(n, dist_mean, 1))) %>% 
    ungroup() %>% 
    select(g1_d, g1_dist) %>% 
    unnest(cols = c(g1_d, g1_dist))
  
  g1_data <- str_gran %>% arrange(g1) %>% bind_cols(g1_dist = g1_dist$g1_dist) %>% arrange(index)
  
  # calculation of g2
  
  g2_table<- bind_cols(g2 = unique(g2),
                       dist_mean = c(mu21, mu22, mu23))
  
  g2_tally <- str_gran %>%
    group_by(g2) %>%
    count() %>% 
    left_join(g2_table, by = "g2")
  
  g2_dist <- g2_tally %>%
    mutate(g2_d = list(rep(g2, each = n)),
           g2_dist = list(rnorm(n, dist_mean, 1))) %>% 
    ungroup() %>% 
    select(g2_d, g2_dist) %>% 
    unnest(cols = c(g2_d, g2_dist))
  
  g2_data <- str_gran %>% arrange(g2) %>% bind_cols(g2_dist = g2_dist$g2_dist) %>% arrange(index)
  
  # calculation of g3
  
  g3_table<- bind_cols(g3 = unique(g3), dist_mean = c(mu31, mu32, mu33, mu34, mu35))
  
  g3_tally <- str_gran %>% group_by(g3) %>% count() %>% left_join(g3_table, by = "g3")
  
  g3_dist <- g3_tally %>%
    mutate(g3_d = list(rep(g3, each = n)),
           g3_dist = list(rnorm(n, dist_mean, 1))) %>% 
    ungroup() %>% 
    select(g3_d, g3_dist) %>% 
    unnest(cols = c(g3_d, g3_dist))
  
  g3_data <- str_gran %>% arrange(g3) %>% bind_cols(g3_dist = g3_dist$g3_dist) %>% arrange(index)
  
  
  innov_data <- g1_data %>% 
    left_join(g2_data %>% select(index, g2_dist), by = "index") %>% 
    left_join(g3_data %>% select(index, g3_dist), by = "index")
  
  nd_time <- innov_data %>% mutate(ts = innov_data$g1_dist + innov_data$g2_dist + innov_data$g3_dist)
  nd_time
  
  # 500 burning observations considered
}



time_seriesj = "ar1"

design5_DGP2 <- generate_design_DGP2(n=300,mu12=2, mu22=2, mu23 =4, mu32 = 2,mu33 = 4, mu34 = 2)
#design5_DGP2 <- generate_design_DGP2(n=300)



design5_DGP3 <- generate_design_DGP3(n=300,mu12=2, mu22=2, mu23 =4, mu32 = 2,mu33 = 4, mu34 = 2)


design5_DGP2 %>% ggplot(aes(x=index, y = ts)) + geom_line()
design5_DGP3 %>% ggplot(aes(x=index, y = ts)) + geom_line()



design5_DGP2 %>% ggplot(aes(x=as.factor(g1), y = ts)) + geom_boxplot()
design5_DGP2 %>% ggplot(aes(x=as.factor(g2), y = ts)) + geom_boxplot()
design5_DGP2 %>% ggplot(aes(x=as.factor(g3), y = ts)) + geom_boxplot()


design5_DGP3 %>% ggplot(aes(x=as.factor(g1), y = ts)) + geom_boxplot()
design5_DGP3 %>% ggplot(aes(x=as.factor(g2), y = ts)) + geom_boxplot()
design5_DGP3 %>% ggplot(aes(x=as.factor(g3), y = ts)) + geom_boxplot()

library(fpp3)



p11 <- design5_DGP2 %>% ggplot(aes(x=as.factor(g1), y = ts)) + geom_boxplot()
p12 <- design5_DGP2 %>% ggplot(aes(x=as.factor(g2), y = ts)) + geom_boxplot()
p13 <- design5_DGP2 %>% ggplot(aes(x=as.factor(g3), y = ts)) + geom_boxplot()

p1boxplot <- p11 + p12+ p13 + ggtitle("with ts")


p21 <- design5_DGP3 %>% ggplot(aes(x=as.factor(g1), y = ts)) + geom_boxplot()
p22 <- design5_DGP3 %>% ggplot(aes(x=as.factor(g2), y = ts)) + geom_boxplot()
p23 <- design5_DGP3 %>% ggplot(aes(x=as.factor(g3), y = ts)) + geom_boxplot()

p2boxplot <- p21 + p22+ p23  + ggtitle("without ts")

p1 <- design5_DGP2 %>%
  as_tsibble(index = index) %>% 
  ACF(ts) %>%
  autoplot()+ labs(title = "ACF with ts")

p2 <- design5_DGP2 %>%
  as_tsibble(index = index) %>% 
  PACF(ts) %>%
  autoplot()   + labs(title = "PACF with ts")


p3 <- design5_DGP2 %>%
  as_tsibble(index = index) %>% 
  autoplot(ts) + labs(title = "line plot with ts")


p4 <- design5_DGP3 %>%
  as_tsibble(index = index) %>% 
  ACF(ts) %>%
  autoplot()    + labs(title = "ACF without ts")

p5 <- design5_DGP3 %>%
  as_tsibble(index = index) %>% 
  PACF(ts) %>%
  autoplot() + labs(title = "PACF without ts")

p6 <- design5_DGP3 %>%
  as_tsibble(index = index) %>% 
  autoplot(ts) + labs(title = "line plot without ts")



#(p3 + p1boxplot + p1  + p2)/ (p6 + p2boxplot + p4 + p5)


(p1 + p2 + p3) / (p4 + p5 + p6)

(p1boxplot / p2boxplot) 


generate_design <- function(ntimes, )





set.seed(12345)
g10 <- rnorm(6, 0, 1)
g11 <- rnorm(6, 2, 1)
data_frame <- tibble(row = 1:12, g1 = rep(c(0,1), 6))

nd <- data_frame %>% arrange(g1) %>% mutate(g1dist = c(g10, g11)) %>% arrange(row)

ggplot(nd, aes(x =  as.factor(g1), y =g1dist)) + geom_boxplot()

g10d <- arima.sim(n = 1200, list(ar = c(0.8897, -0.4858), ma = c(-0.2279, 0.2488)),
            innov =  nd$g1dist)


g10d <- arima.sim(n = 1200, list(ar = 0, ma = c(-0.2279, 0.2488)),
                  innov =  nd$g1dist)

library(forecast)
autoplot(g10d)


#ggplot(g10d, aes(x =  as.factor(g1), y =g1dist)) + geom_boxplot()

nd_time <- nd %>% bind_cols(x = as.numeric(g10d) )

ggplot(nd_time, aes(x =  as.factor(g1), y = x)) + geom_boxplot()

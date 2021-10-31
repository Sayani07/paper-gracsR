##----load-libraries
library(tidyverse)


##----possible-options

dg1 <- c("null", "vary")
dg2 <- c("null", "vary")
dg3 <- c("null", "vary")
data_set  = expand.grid(dg1, dg2, dg3) %>% mutate(index = row_number())


##----generate-design
generate_design <- function(t, mu11, mu12, # mean of g1
                            mu21, mu22, mu23, # mean of g2
                            mu31, mu32, mu33, mu34, mu35) # mean of g3{
  
  t <- seq(0, t, 1)
  g1 <- t %%2
  g2 <- t %%3
  g3 <- t %%5
  
  # null design
  g1_dnull <- rep( rep(0, each = length(unique(g1))), length.out= length(t))
  g2_dnull <- rep( rep(0, each = length(unique(g2))), length.out= length(t))
  g3_dnull <- rep( rep(0, each = length(unique(g3))), length.out= length(t))
  
  # mean changing across categories in varying ways
  
  g11_dvary <- rep(mu11, length.out= length(t))
  g12_dvary <- rep(mu12, length.out= length(t))
  g21_dvary <- rep(mu21, length.out= length(t))
  g22_dvary <- rep(mu22, length.out= length(t))
  g23_dvary <- rep(mu23, length.out= length(t))
  g3_dvary <- rep(0, length.out= length(t))
  
  design1 = distributional::dist_normal(g1_dnull + g2_dnull + g3_dnull)
  design2 = distributional::dist_normal(g11_dvary + g21_dvary + g3_dnull)
  design3 = distributional::dist_normal(g12_dvary + g22_dvary + g3_dnull)
  design4 = distributional::dist_normal(g1_dnull + g23_dvary + g3_dnull)
  
  data_bind <- tibble::tibble(
    index = t,
    g1 = g1,
    g2 = g2,
    g3 = g3,
    design1 = distributional::generate(design1, times = 1) %>% unlist(),
    design2 = distributional::generate(design2, times = 1) %>% unlist(),
    design3 = distributional::generate(design3, times = 1) %>% unlist(),
    design4 = distributional::generate(design4, times = 1) %>% unlist(),
    #design5 = distributional::generate(design5, times = 1) %>% unlist()
  ) %>% 
    pivot_longer(-c(1, 2, 3, 4), names_to = "design", values_to = "sim_data")
  
  data_bind
}

data_bind <- generate_design(t, mu21, mu22, mu23)

data_bind

```


# _Plot raw data_

```{r plot-linear , out.width="100%"}

# plot_linear_data <- function(data){
# ggplot(data,
#              aes(x = index, y = sim_data)) + 
#   geom_line() +
#   xlab("index")+
#   theme_bw() 
# }

ggplot(data_bind,
       aes(x = index, y = sim_data)) + 
  geom_line() +
  xlab("index")+
  theme_bw() +
  facet_wrap(~design, scales = "free_y",ncol =1)

```

# _Plot distribution across granularities_

```{r plot-gran, out.width="100%"}

p2 <- ggplot(data_bind,
             aes(x = as.factor(g1), y = sim_data)) + 
  geom_boxplot(alpha =0.5) + xlab("g1") + 
  theme_bw() +
  facet_wrap(~design, scales = "free_y", ncol = 1)+ stat_summary(
    fun = median,
    geom = 'line',
    aes(group = 1), size = 1, color = "blue")

p3 <- ggplot(data_bind, aes(x = as.factor(g2), y = sim_data)) + geom_boxplot(alpha =0.5) + xlab("g2") + theme_bw() +
  facet_wrap(~design, scales = "free_y",ncol = 1)+ stat_summary(
    fun = median,
    geom = 'line',
    aes(group = 1), size = 1, color = "blue")

p4 <- ggplot(data_bind, aes(x = as.factor(g3), y = sim_data)) + geom_boxplot(alpha =0.5) +
  xlab("g3") + theme_bw()+
  facet_wrap(~design, scales = "free_y", ncol = 1)+ stat_summary(
    fun = median,
    geom = 'line',
    aes(group = 1), size = 1, color = "blue")

(p2 + p3 + p4) * theme(
  strip.text = element_text(size = 8, margin = margin(b = 0, t = 0)))

# 
# 
# plot_cyclic_data( data_bind %>% filter(design=="design2"))
# plot_data( data_bind %>% filter(design=="design3"))
# plot_data( data_bind %>% filter(design=="design4"))
# plot_data( data_bind %>% filter(design=="design5"))
```

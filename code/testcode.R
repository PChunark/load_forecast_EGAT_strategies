d<- tibble(year = c(2010:2015),
       value = c(5,7,9,13,34,37),
       grw = c(0,0.01,0.02,-0.01,0.02,0.01))

d %>%filter(year == c(2010:2011)) %>%  mutate(new = if_else(grw>0,first(value) + first(value) * grw, 0))

d %>%
  filter(year == c(2010:2012)) %>%  
  mutate(new = if_else(grw>0,first(value) + first(value) * grw, 0))

df <- data.frame(id=rep(c("A","B"),each=5),
                 year=rep(1:5,2),
                 value = NA)

set.seed(123)
growth_rates <- data.frame(id=rep(c("A","B"),each=5),
                           year=rep(1:5,2),
                           value = runif(10,0.95,1.1))
initial_vals <- c(5, 7)

growth_rates %>%
  group_by(id) %>% 
  mutate(cumulative = lag(cumprod(value), default = 1)) %>%
  mutate(value = cumulative * initial_vals[match(id, c("A", "B"))]) %>%
  select(-cumulative)

newload3u %>% 
  select(!netGenPeak3U_mw) %>% 
  mutate(cumulative = lag(cumprod(grw_3u$growth),n = 0,default = 1)) %>% 
  mutate(value = newload3u %>% 
           select(!netGenPeak3U_mw) %>%
           nth(4) %>% 
           pull() * cumulative)

newload3u %>% 
  select(!netGenPeak3U_mw) %>% 
  mutate(cumulative = lag(cumprod(grw_3u$growth),n = 0,default = 1)) %>% 
  mutate(value = newload3u %>% 
           select(!netGenPeak3U_mw) %>%
           filter(year == previous_year_grw) %>% 
           select(netGenEne3u_gwh) %>% 
           pull() * cumulative)

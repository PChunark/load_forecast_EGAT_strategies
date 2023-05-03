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

# initial_value <-
pull(if_else(grw_3u$growth < 1,
        newload3u %>% 
          select(!netGenPeak3U_mw) %>% 
          mutate(x = lag(netGenEne3u_gwh)) 
          ,F))
        

newload3u %>% 
  select(!netGenPeak3U_mw) %>%
  nth(4) %>% 
   pull(netGenEne3u_gwh)


newload3u %>% 
  select(!netGenPeak3U_mw) %>%
  mutate(grw = grw_3u$growth) %>% 
  ดรสะำพ


# https://stackoverflow.com/questions/59778456/using-dplyr-within-groups-select-the-first-value-meeting-a-condition
set.seed(42)

df <- data.frame(
  id = sample(LETTERS[1:3], 20, replace = TRUE),
  time.var = sample(1:20, 20, replace = TRUE),
  x = sample(c(1:10), 20, replace = TRUE)
)
df %>% 
  arrange(id, time.var) %>% 
  group_by(id) %>% 
  mutate(previous.less.5 = tail(c(x[c((x[-n()] < 5), FALSE)]),1)) %>% 
  group_by(id) %>% 
  mutate(previous.less.5 = if_else(row_number() == n(), previous.less.5, NULL))

df %>%
  arrange(id, time.var) %>% 
  group_by(id) %>%   
  slice(1:(n()-1)) %>% 
  filter(x < 5) %>% 
  slice(n()) %>% 
  select(-time.var) %>% 
  right_join(df, ., by="id", suffix =c("",".y")) %>% 
  group_by(id) %>% 
  mutate(previous.less.5 = if_else(row_number() == n(), x.y, NULL)) %>%
  select(-x.y) 
library(tidyverse)
library(readxl)

## Extract growth for electricity from 3 utilities from excel control sheet
grw_3u_excel <-
  read_excel("process_data/excel_control.xlsx",
             sheet = "grw_3u",
             range = "A1:B83") %>% 
  drop_na()


grw_3u <- 
  grw_3u_excel %>% 
  select(growth) %>% 
  mutate(growth = 1+growth)


previous_year_grw <-
grw_3u_excel %>%
  mutate(years = growth<0 | growth > 0) %>% 
  filter(years == first(years)) %>% 
  last() %>% 
  select(year) %>% 
pull(year)

######################################################
## Extract growth for MEA demand from excel control sheet

grw_mea_dmd_excel <-
  read_excel("process_data/excel_control.xlsx",
             sheet = "grw_mea_dmd",
             range = "A1:B83") %>% 
  drop_na()


grw_mea_dmd <- 
  grw_mea_dmd_excel %>% 
  select(growth)%>% 
  mutate(growth = 1+growth)

previous_year_mea_dmd_grw <-
  grw_mea_dmd_excel %>%
  mutate(years = growth<0 | growth > 0) %>% 
  filter(years == first(years)) %>% 
  last() %>% 
  select(year) %>% 
  pull(year)

######################################################
## Extract growth for PEA demand from excel control sheet

grw_pea_dmd_excel <-
  read_excel("process_data/excel_control.xlsx",
             sheet = "grw_pea_dmd",
             range = "A1:B83") %>% 
  drop_na()


grw_pea_dmd <- 
  grw_pea_dmd_excel %>% 
  select(growth)%>% 
  mutate(growth = 1+growth)

previous_year_pea_dmd_grw <-
  grw_pea_dmd_excel %>%
  mutate(years = growth<0 | growth > 0) %>% 
  filter(years == first(years)) %>% 
  last() %>% 
  select(year) %>% 
  pull(year)

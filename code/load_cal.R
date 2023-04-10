library(tidyverse) # Package for data manipulation
library(readxl) #Package for reading excel file. It is not a core package in tidyverse package

b_load <- read_excel("raw_data/load25aug2022andbudegetload.xlsx", skip = 3) %>% 
  select(year_th, year, peak_budget20oct2022, ene_budget20oct2022) %>% 
  drop_na()

pdp2022_load <- read_excel("raw_data/load25aug2022andbudegetload.xlsx", skip = 3) %>% 
  select(year_th, year, peak_bau_nd_ee70, ene_bau_nd_ee70) %>% 
  drop_na() %>% 
  filter(year >= 2029)

inner_join(x = b_load, y = pdp2022_load, by = year)

# "peak_budget20oct2022"="peak_bau_nd_ee70", "ene_budget20oct2022" = "ene_bau_nd_ee70"))
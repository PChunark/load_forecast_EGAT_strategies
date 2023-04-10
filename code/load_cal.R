library(tidyverse) # Package for data manipulation
library(readxl) #Package for reading excel file. It is not a core package in tidyverse package

## Budget load 20 Oct 2022
b_load <- 
  read_excel("raw_data/load25aug2022andbudegetload.xlsx", skip = 3) %>% 
  select(year_th, year, peak_budget20oct2022, ene_budget20oct2022) %>% 
  drop_na() %>% 
  rename(a = peak_budget20oct2022, b = ene_budget20oct2022)  %>% 
  
## PDP2022 load BAU ND EE70%
pdp2022_load <- read_excel("raw_data/load25aug2022andbudegetload.xlsx", skip = 3) %>% 
  select(year_th, year, peak_bau_nd_ee70, ene_bau_nd_ee70) %>% 
  drop_na() %>% 
  filter(year >= 2029) %>% 
  rename(a = peak_bau_nd_ee70, b = ene_bau_nd_ee70)

## Combine 2 loads budget load till 2028 & load PDP2022 from 2029-2030
newload3u <- rbind(x = b_load, y = pdp2022_load) %>% 
  rename(newpeak3u = a, newene3u=b)

newload3u %>% 
  mutate()
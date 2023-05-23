library(tidyverse)
library(readxl)

read_excel("raw_data/raw_data_profiles/02_Hourly Sale_NetGen_2019.xlsx",
           sheet = "Load Curve",
           range = "C3:D17523"
          ) %>% ggplot()+geom_line()

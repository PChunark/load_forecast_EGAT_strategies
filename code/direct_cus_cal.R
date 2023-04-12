library(tidyverse)

# Direct customer for budget load @ September 2022

b_dc_ene <-
  
read_excel("raw_data/01 EGAT_20Oct2022_งบ66 67_Final_Sent อศง.xlsx",
           sheet = "DCs",
           range = "A6:K44",
           col_names = c("direct_cus", 2019:2028)) %>% 
  drop_na() %>% 
  filter(str_detect(direct_cus, "Energy Demand ")) %>% 
  pivot_longer(-direct_cus, names_to = "year", values_to = "dc_gwh") %>% 
  mutate(year = as.numeric(year),
         year_th = year + 543)

# Direct customer for PDP2022 case7 @ March 2022

pdp2022_dc_ene <-
  
read_excel("raw_data/93 EGAT_25Aug2022_13 Energy ภาพรวม NIDA_BAU_ND_EE70__Sent.xlsx",
           sheet = "DCs_Mar_BAU",
           range = "A6:V51",
           col_names = c("direct_cus", 2017:2037)) %>% 
  drop_na() %>% 
  filter(str_detect(direct_cus, "Energy Demand ")) %>% 
  pivot_longer(-direct_cus, names_to = "year", values_to = "dc_gwh") %>%
  filter(direct_cus == "Energy Demand of  OVER ALL") %>% 
  mutate(year = as.numeric(year),
         year_th = year + 543) %>% 
  filter(year >= 2029)

# Combine direct customer from budget load @ September 2022 and pdp2022 @ March 2022
newdc_ene <- rbind(x = b_dc_ene, y = pdp2022_dc_ene)
  
newdc_ene %>% 
  ggplot(aes(x = year, y = dc_gwh)) +
  geom_line()

library(tidyverse)
library(readxl)

mea_vspp_ene_ext <-

read_excel("raw_data/01 EGAT_20Oct2022_งบ66 67_Final.xlsx",
           sheet = "I_Actual",
           range = "CM7:CM31",
           col_names = "mea_vspp_gwh")%>%
  mutate(year = 1997:2021) %>% 
  mutate(mea_vspp_gwh = if_else(is.na(mea_vspp_gwh), 0, mea_vspp_gwh)) #%>%
  # ggplot(aes(x = year, y = mea_vspp_gwh)) +
  # geom_line()

pea_vspp_ene_ext <-
  
  read_excel("raw_data/25-24-65 ข้อมูล VSPP-IPS ส่ง สนพ_PEA_rev2cal.xlsx",
           sheet = "VSPP_forecast",
           range = "A3:I38",
           col_names = c("fuel", "variable", 2015:2021))%>%
  filter(variable == "GWh") %>% 
  pivot_longer(-fuel&-variable, names_to = "year",values_to = "pea_existing_gwh") %>% 
  mutate(year_th = as.numeric(year)+543) #%>%
  # ggplot(aes(x = year, y = pea_existing_gwh, group = fuel,  color = fuel)) + 
  # geom_line() +
  # facet_wrap(~fuel, scales = "free_y")

pea_dede_ene <-
  
read_excel("raw_data/01 EGAT_20Oct2022_งบ66 67_Final.xlsx",
           sheet = "I_Actual",
           range = "ET7:ET31",
           col_names = "pea_dede_gwh") %>% 
  mutate(year = 1997:2021,
         year_th = year +543)

pea_sfgen_ene <-
  
read_excel("raw_data/01 EGAT_20Oct2022_งบ66 67_Final.xlsx",
           sheet = "I_Actual",
           range = "EZ7:EZ31",
           col_names = "pea_sfgen_gwh") %>% 
  mutate(year = 1997:2021,
         year_th = year +543) %>% print(n=50)


#


cbind(mea_vspp_ene_ext %>% 
        filter(fuel == "Total") %>% 
        select(year, year_th, mea_existing_gwh), 
      pea_vspp_ene_ext %>% 
        filter(fuel == "Total") %>% 
        select(pea_existing_gwh),
      pea_dede_ene %>% 
        filter(year >= 2015) %>% 
        select(pea_dede_gwh),
      pea_sfgen_ene %>% 
        filter(year >= 2015) %>% 
        select(pea_sfgen_gwh)
        ) %>% 
  mutate(tot_vspp_ext = mea_existing_gwh + pea_existing_gwh + pea_dede_gwh + pea_sfgen_gwh)

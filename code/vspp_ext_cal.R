library(tidyverse)

mea_vspp_ene_ext <-

read_excel("raw_data/ข้อมูล VSPP - IPS MEA_cal.xlsx",
           sheet = "VSPP COD (หน่วยตรงเดือน)",
           range = "A3:I38",
           col_names = c("fuel", "variable", 2015:2021))%>%
 filter(variable == "GWh") %>% 
 pivot_longer(-fuel&-variable, names_to = "year",values_to = "mea_existing_gwh") %>% 
  mutate(year_th = as.numeric(year)+543) #%>%
  # ggplot(aes(x = year, y = mea_existing_gwh, group = fuel, , color = fuel)) + 
  # geom_line()+
  # facet_wrap(~fuel, scales = "free_y")

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

cbind(mea_vspp_ene_ext %>% 
        filter(fuel == "Total"), 
      pea_vspp_ene_ext %>% 
        filter(fuel == "Total") %>% 
        select(pea_existing_gwh)  
        )
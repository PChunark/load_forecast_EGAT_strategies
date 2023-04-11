library(tidyverse)  # Package for data manipulation
library(readxl) #Package for reading excel file. It is not a core package in tidyverse package

vsppcontractcapPDP2018R1 <-
  
  read_excel("raw_data/Domeสรุป VSPP_PDP18R1 and ร่าง PDP2022 เคส 7_เพิ่ม vspp.xlsx",
              sheet = "สรุป VSPP PDP18R1 Fuel",
              range = "A1:U13")%>%
  rename(vspp_mw = "VSPP (MW)") %>% 
  pivot_longer(-vspp_mw, names_to = "year_th",values_to = "contract_mw") %>% 
  mutate(year = as.numeric(year_th)-543) %>%
  pivot_wider(names_from = vspp_mw, values_from = contract_mw) %>% 
  rename_all(tolower) %>% 
  rename(ind_waste = "industrial waste",
         total = "รวม") %>% 
  pivot_longer(-year_th&-year, names_to = "fuel",values_to = "contract_mw") #%>% 
  # ggplot(aes(x = year, y = contract_mw, group = fuel, , color = fuel)) + 
  # geom_line()


vsppenergyPDP2018R1 <-
  read_excel("raw_data/Domeสรุป VSPP_PDP18R1 and ร่าง PDP2022 เคส 7_เพิ่ม vspp.xlsx",
             sheet = "สรุป VSPP PDP18R1 Fuel",
             range = "A16:U28")%>%
  rename(vspp_gwh = "VSPP (Energy)") %>% 
  pivot_longer(-vspp_gwh, names_to = "year_th",values_to = "energy_gwh") %>% 
  mutate(year = as.numeric(year_th)-543) %>%
  pivot_wider(names_from = vspp_gwh, values_from = energy_gwh) %>% 
  rename_all(tolower) %>% 
  rename(ind_waste = "industrial waste",
         total = "รวม") %>% 
  pivot_longer(-year_th&-year, names_to = "fuel",values_to = "vspp_gwh") #%>% 
  # ggplot(aes(x = year, y = vspp_gwh, group = fuel, , color = fuel)) + 
  # geom_line()


vsppcontractcapPDP2022C7<-
  
  read_excel("raw_data/Domeสรุป VSPP_PDP18R1 and ร่าง PDP2022 เคส 7_เพิ่ม vspp.xlsx",
           sheet = "สรุป VSPP PDP2022 Fuel",
           range = "A1:Q23")%>%
  rename(vspp_mw = "VSPP-Contract Capacity") %>% 
  pivot_longer(-vspp_mw, names_to = "year",values_to = "contract_mw") %>% 
  mutate(year_th = as.numeric(year)+543) %>%
  pivot_wider(names_from = vspp_mw, values_from = contract_mw) %>% 
  rename_all(tolower) %>% 
  pivot_longer(-year_th&-year, names_to = "fuel",values_to = "vspp_mw") #%>% 
  # ggplot(aes(x = as.factor(year), y = vspp_mw, group = fuel, color = fuel)) + 
  # geom_line()


vsppenergyPDP2022C7 <-
  
  read_excel("raw_data/Domeสรุป VSPP_PDP18R1 and ร่าง PDP2022 เคส 7_เพิ่ม vspp.xlsx",
             sheet = "สรุป VSPP PDP2022 Fuel",
             range = "A25:Q47")%>%
  rename(vspp_gwh = "VSPP-Generation") %>% 
  pivot_longer(-vspp_gwh, names_to = "year",values_to = "energy_gwh") %>% 
  mutate(year_th = as.numeric(year)+543) %>%
  pivot_wider(names_from = vspp_gwh, values_from = energy_gwh) %>% 
  rename_all(tolower) %>% 
  pivot_longer(-year_th&-year, names_to = "fuel",values_to = "vspp_gwh") #%>% 
  # ggplot(aes(x = as.factor(year), y = vspp_gwh, group = fuel, color = fuel)) + 
  # geom_line()
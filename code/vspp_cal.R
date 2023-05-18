source("code/vspp_ext_cal.R")

library(tidyverse)  # Package for data manipulation
library(readxl) #Package for reading excel file. It is not a core package in tidyverse package

# Equation for VSPP estimation
### VSPP[t] = MEA_VSPP[t] + PEA_VSPP[t] + PEA_DEDE[t] + PEA_SFGEN[t]


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
  # filter(fuel != "total") %>% 
  #  ggplot(aes(x = year, y = vspp_gwh, group = fuel, color = fuel)) +
  #  geom_line()


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
  # filter(fuel != "total") %>% 
  # ggplot(aes(x = as.factor(year), y = vspp_gwh, group = fuel, color = fuel)) +
  # geom_line()

## Merge existing vspp data and forecast vspp data in PDP2018R1

merge_vspp_ext <- 
  
tot_vspp_ext %>% 
  filter(vspp == "tot_vspp_ext_gwh",
         year >= 2019) %>% 
  select(!vspp)

merge_vspp_pdp2018r1 <-
  
vsppenergyPDP2018R1 %>% 
  filter(fuel =="total",
         year >= 2022) %>% 
  select(!fuel)

merge_vspp_ext_pdp <-
  full_join(merge_vspp_ext,
          merge_vspp_pdp2018r1,
          by = c("ene_vspp_ext_gwh" = "vspp_gwh"))

## Merge existing vspp data and forecast vspp data in PDP2023 Case7

merge_vspp_pdp2023c7 <-
  
  vsppenergyPDP2022C7 %>% 
  filter(fuel =="total",
         year >= 2022) %>% 
  select(!fuel)

merge_vspp_ext_pdp2023c7 <-
  full_join(merge_vspp_ext,
            merge_vspp_pdp2023c7,
            by = c("ene_vspp_ext_gwh" = "vspp_gwh"))

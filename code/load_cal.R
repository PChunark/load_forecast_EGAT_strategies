source("code/vspp_cal.R")

# Equations of electricity load in Thailand power system
## EGT_TOT_SLE[t] = MEA[t] + PEA[t] + DSE[t]
## EGT_DST[t] = EGT_TOT_SLE[t] + LSS[t]
## EGT_NET_GEN[t] = EGT_DBT[t] + USE[t] + PUMP[t]
## NET_GEN_3U[t] = EGT_NET_GEN[t] + VSPP[t]
## GEN_THA_SYS[t] = NET_GEN_3U[t] + IPS[t]



## Budget load 20 Oct 2022
b_load <- 
  read_excel("raw_data/load25aug2022andbudegetload.xlsx", skip = 3) %>% 
  select(year_th, year, peak_budget20oct2022, ene_budget20oct2022) %>% 
  drop_na() %>% 
  rename(a = peak_budget20oct2022, b = ene_budget20oct2022) 
  
## PDP2022 load BAU ND EE70%
pdp2022_load <- read_excel("raw_data/load25aug2022andbudegetload.xlsx", skip = 3) %>% 
  select(year_th, year, peak_bau_nd_ee70, ene_bau_nd_ee70) %>% 
  drop_na() %>% 
  filter(year >= 2029) %>% 
  rename(a = peak_bau_nd_ee70, b = ene_bau_nd_ee70)

## Combine 2 loads budget load till 2028 & load PDP2022 from 2029-2030
newload3u <- rbind(x = b_load, y = pdp2022_load) %>% 
  rename(netGenPeak3U_mw = a, netGenEne3u_gwh=b)

newload3u %>% 
  select(!netGenPeak3U_mw) %>% 
  mutate(vsppenergyPDP2018R1 %>% 
         filter(year >= 2019,
               fuel == "total") %>% 
         select(vspp_gwh),
         egt_net_gen = netGenEne3u_gwh - vspp_gwh
         ) 
  

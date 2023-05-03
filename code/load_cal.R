source("code/excel_control.R")
source("code/vspp_ext_cal.R")
source("code/vspp_cal.R")
source("code/use_and_pump_cal.R")
source("code/tran_loss_percent.R")
source("code/direct_cus_cal.R")
source("code/mea_ene_cal.R")
source("code/pea_ene_cal.R")

# Equations of electricity load in Thailand power system
## EGT_TOT_SLE[t] = MEA[t] + PEA[t] + DSE[t]
## EGT_DBT[t] = EGT_TOT_SLE[t] + LSS[t]
### LSS[t] = EGT_TOT_SLE[t] * %LSS[t]
### EGT_TOT_SLE = EGT_DBT[T] / (1 + %LSS[T])
## EGT_NET_GEN[t] = EGT_DBT[t] + USE[t] + PUMP[t]
## NET_GEN_3U[t] = EGT_NET_GEN[t] + VSPP[t]
### VSPP[t] = MEA_VSPP[t] + PEA_VSPP[t] + PEA_DEDE[t] + PEA_SFGEN[t]
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

#Calculate the electricity sale in the base case
basecase_netgen3u <-
  
  
newload3u %>% 
  select(!netGenPeak3U_mw) %>% 
  mutate(merge_vspp_ext_pdp %>% 
           select(ene_vspp_ext_gwh),  
         egt_net_gen = netGenEne3u_gwh - ene_vspp_ext_gwh,
         total_usepump %>% filter(year >= 2019) %>% select(total_usepump),
         egt_dbt = egt_net_gen-total_usepump,
         egt_dbt / (1 + tran_loss_percent %>% filter(year >= 2019) %>% select(percent_loss)),
         newdc_ene %>% select(dc_gwh),
         mea_pea = percent_loss-dc_gwh,
         mea_pea * shr_mea_pea_pdp2018r1 %>% select(shr_egatsle_mea),
         mea_pea * shr_mea_pea_pdp2018r1 %>% select(shr_egatsle_pea)
         ) 

# bestcase_netgen3u <-

newload3u %>% 
  select(!netGenPeak3U_mw) %>% 
  mutate(merge_vspp_ext_pdp2023c7 %>% 
           select(ene_vspp_ext_gwh),  
         egt_net_gen = netGenEne3u_gwh - ene_vspp_ext_gwh,
         total_usepump %>% filter(year >= 2019) %>% select(total_usepump),
         egt_dbt = egt_net_gen-total_usepump,
         egt_dbt / (1 + tran_loss_percent %>% filter(year >= 2019) %>% select(percent_loss)),
         newdc_ene %>% select(dc_gwh),
         mea_pea = percent_loss-dc_gwh,
         mea_pea * shr_mea_pea_pdp2022c7 %>% select(shr_egatsle_mea),
         mea_pea * shr_mea_pea_pdp2022c7 %>% select(shr_egatsle_pea)
  ) %>% 
  arrange(year) %>% 
  mutate(diff_year = year - lag(year),
         diff_growth = shr_egatsle_pea - lag(shr_egatsle_pea))

# worstcase_netgen3u <-
  
newload3u %>% 
  select(!netGenPeak3U_mw) %>%
  mutate(netGenEne3u_gwh1 = netGenEne3u_gwh*(1+grw_3u))
  mutate(merge_vspp_ext_pdp2023c7 %>% 
           select(ene_vspp_ext_gwh),  
         egt_net_gen = netGenEne3u_gwh - ene_vspp_ext_gwh,
         total_usepump %>% filter(year >= 2019) %>% select(total_usepump),
         egt_dbt = egt_net_gen-total_usepump,
         egt_dbt / (1 + tran_loss_percent %>% filter(year >= 2019) %>% select(percent_loss)),
         newdc_ene %>% select(dc_gwh),
         mea_pea = percent_loss-dc_gwh,
         mea_pea * mea_pea_ene %>% select(shr_mea),
         mea_pea * mea_pea_ene %>% select(shr_pea)
  ) 

library(tidyverse) #For data manipulation
library(readxl) #For reading the excel sheet
library(scales)
library(glue)

#----- Create general theme for figures -----####
ThemeLine <- 
  theme_bw() +
  theme(
    panel.border=element_rect(fill=NA),
    panel.grid.minor = element_line(color = NA), 
    #    axis.title=element_text(size=5),
    #    axis.text.x = element_text(hjust=1,size = 10, angle = 0),
    axis.line=element_line(colour="black"),
    panel.background=element_rect(fill = "white"),
    panel.grid.major.x=element_line(linetype="dashed",colour="grey",size=0.5),
    panel.grid.major.y = element_blank(),
    # panel.grid.major=element_blank(),
    strip.background=element_rect(fill="white", colour="white"),
    strip.text.x = element_text(size=10, colour = "black", angle = 0,face="bold"),
    axis.text.x=element_text(size = 10,angle=45, vjust=0.9, hjust=1, margin = unit(c(t = 0.3, r = 0, b = 0, l = 0), "cm")),
    axis.text.y=element_text(size = 10,margin = unit(c(t = 0, r = 0.3, b = 0, l = 0), "cm")),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    axis.ticks.length=unit(-0.15,"cm")
  )
linepalette1 <- c("#4DAF4A","#FF7F00","#377EB8","#E41A1C","#984EA3","#F781BF","#8DD3C7","#FB8072","#80B1D3","#FDB462","#B3DE69","#FCCDE5","#D9D9D9","#BC80BD","#CCEBC5","#FFED6F","#7f878f","#A65628","#FFFF33")
linepalette2 <- c("#E41A1C","#FF7F00","#377EB8","#B3DE69","#4DAF4A","#984EA3","#F781BF","#8DD3C7","#FB8072","#80B1D3","#FDB462","#FCCDE5","#D9D9D9","#BC80BD","#CCEBC5","#FFED6F","#7f878f","#A65628","#FFFF33")



#----- Make a list of profile to store data -----####

profiledata <- list()
profilefigure <- list()
summarydata <- list()

#----- Make variable for output directory -----####
outfigdir <- c("figures/")

#----- ____ -----####

#----- The 2019 MEA EGAT sale profile ----####
# Profile data ####
profile <- 
  read_excel("raw_data/raw_data_profiles/02_Hourly Sale_NetGen_2019.xlsx",
           sheet = "Load Curve",
           range = "C3:D17523"
            ) %>% 
  select(datetime = `Date/Time`, MEA) %>% 
  mutate(date = date(datetime),
         time = format(as.POSIXct(datetime),"%H:%M:%S"),
         year = year(datetime),
         month = month(datetime),
         day = day(datetime)) %>% 
  select(datetime, date, time, year, month, day, MEA) #%>% 
  # mutate(strdatetime = as.factor(strptime(glue("{year}-{month}-{day} {time}"), "%Y-%m-%d %H:%M:%S")))


# Summary data ####
maxv <- ceiling(max(profile$MEA)) # Get a peak MW
minv <- floor(min(profile$MEA)) # Get a min MW
energy <- sum(profile$MEA)/2000 # Calculate the energy
peak_day <- profile %>% #Find a peak day
            group_by(year) %>% 
            filter(MEA == max(MEA)) %>% 
            pull(datetime)
min_day <- profile %>% #Find a peak day
           group_by(year) %>% 
           filter(MEA == min(MEA)) %>% 
           pull(datetime)
load_factor <- percent((energy*10^3)/(maxv*8760), 
                       accuracy = 0.01, 
                       decimal.mark = ".")
summary <- tibble(peak_day = peak_day,
                  min_day = min_day,
                  peak_mw = maxv, 
                  min_mw = minv, 
                  energy_gwh = energy,
                  load_factor = load_factor) # combine all data in 1 table
# Plot a profile ####
 
profile_plot <-
    ggplot() + 
    geom_line(data=profile, 
              aes(x = datetime, 
                  y = MEA,
                  group = month,
                  color = as.factor(month)),
              show.legend = FALSE) +
    ThemeLine +
    labs(x = NULL,
         y = "EGAT sale to MEA (MW)")+
    scale_x_datetime(breaks=date_breaks("1 month"), 
                     labels=date_format("%b %y")) +
    scale_y_continuous(breaks = seq(0, round(maxv,-3),1000),
                       limits = c(0, round(maxv, -3))) +  
    scale_color_manual(values = linepalette1) +
    geom_point(data=summary,
               aes(x = peak_day, y = peak_mw))+
    geom_text(data = summary,
              aes(x = peak_day, y = round(maxv, -3)),
              label = glue("Peak {maxv} MW \n@ {peak_day}"))+
    geom_point(data=summary,
               aes(x = min_day, y = min_mw))+
    geom_text(data = summary,
              aes(x = min_day, y = round(minv, -3)),
              label = glue("Minimum {minv} MW \n@ {min_day}"),
              hjust = 0,
              vjust = 1.5)

# Save the output ####
outputfigure <- paste0(outfigdir, "mea_egtsle_2019.png")
ggsave(profile_plot, file = outputfigure, dpi = 150, width = 15, height = 5, units = "in", limitsize = FALSE)
profiledata <- c(profiledata, list("mea_egtsle_2019" = profile))
profilefigure <- c(profilefigure, list("mea_egtsle_2019" = profile_plot))
summarydata <- c(summarydata, list("sum_mea_egtsle_2019" = summary))

#----- ____ -----####

#----- The 2019 PEA-R1 (Central region) EGAT sale profile ----####
# Profile data ####
profile <- 
  read_excel("raw_data/raw_data_profiles/02_Hourly Sale_NetGen_2019.xlsx",
             sheet = "Load Curve",
             range = "C3:E17523"
  ) %>% 
  select(datetime = `Date/Time`, PEA_R1) %>% 
  mutate(date = date(datetime),
         time = format(as.POSIXct(datetime),"%H:%M:%S"),
         year = year(datetime),
         month = month(datetime),
         day = day(datetime)) %>% 
  select(datetime, date, time, year, month, day, PEA_R1) #%>% 
# mutate(strdatetime = as.factor(strptime(glue("{year}-{month}-{day} {time}"), "%Y-%m-%d %H:%M:%S")))


# Summary data ####
maxv <- ceiling(max(profile$PEA_R1)) # Get a peak MW
minv <- floor(min(profile$PEA_R1)) # Get a min MW
energy <- sum(profile$PEA_R1)/2000 # Calculate the energy
peak_day <- profile %>% #Find a peak day
  group_by(year) %>% 
  filter(PEA_R1 == max(PEA_R1)) %>% 
  pull(datetime)
min_day <- profile %>% #Find a peak day
  group_by(year) %>% 
  filter(PEA_R1 == min(PEA_R1)) %>% 
  pull(datetime)
load_factor <- percent((energy*10^3)/(maxv*8760), 
                       accuracy = 0.01, 
                       decimal.mark = ".")
summary <- tibble(peak_day = peak_day,
                  min_day = min_day,
                  peak_mw = maxv, 
                  min_mw = minv, 
                  energy_gwh = energy,
                  load_factor = load_factor) # combine all data in 1 table
# Plot a profile ####

profile_plot <-
  ggplot() + 
  geom_line(data=profile, 
            aes(x = datetime, 
                y = PEA_R1,
                group = month,
                color = as.factor(month)),
            show.legend = FALSE) +
  ThemeLine +
  labs(x = NULL,
       y = "EGAT sale to PEA R1 (Central region) (MW)")+
  scale_x_datetime(breaks=date_breaks("1 month"), 
                   labels=date_format("%b %y")) +
  scale_y_continuous(breaks = seq(0, round(maxv,-3),1000),
                     limits = c(0, round(maxv, -3))) +  
  scale_color_manual(values = linepalette1) +
  geom_point(data=summary,
             aes(x = peak_day, y = peak_mw))+
  geom_text(data = summary,
            aes(x = peak_day, y = round(maxv, -3)),
            label = glue("Peak {maxv} MW \n@ {peak_day}"))+
  geom_point(data=summary,
             aes(x = min_day, y = min_mw))+
  geom_text(data = summary,
            aes(x = min_day, y = round(minv, -3)),
            label = glue("Minimum {minv} MW \n@ {min_day}"),
            hjust = 0,
            vjust = 1.5)

# Save the output ####
outputfigure <- paste0(outfigdir, "pea_r1_central_egtsle_2019.png")
ggsave(profile_plot, file = outputfigure, dpi = 150, width = 15, height = 5, units = "in", limitsize = FALSE)
profiledata <- c(profiledata, list("pea_r1_central_egtsle_2019" = profile))
profilefigure <- c(profilefigure, list("pea_r1_central_egtsle_2019" = profile_plot))
summarydata <- c(summarydata, list("sum_pea_r1_central_egtsle_2019" = summary))


#----- ____ -----####

#----- The 2019 PEA-R2 (North Eastern region) EGAT sale profile ----####
# Profile data ####
profile <- 
  read_excel("raw_data/raw_data_profiles/02_Hourly Sale_NetGen_2019.xlsx",
             sheet = "Load Curve",
             range = "C3:F17523"
  ) %>% 
  select(datetime = `Date/Time`, PEA_R2) %>% 
  mutate(date = date(datetime),
         time = format(as.POSIXct(datetime),"%H:%M:%S"),
         year = year(datetime),
         month = month(datetime),
         day = day(datetime)) %>% 
  select(datetime, date, time, year, month, day, PEA_R2) #%>% 
# mutate(strdatetime = as.factor(strptime(glue("{year}-{month}-{day} {time}"), "%Y-%m-%d %H:%M:%S")))


# Summary data ####
maxv <- ceiling(max(profile$PEA_R2)) # Get a peak MW
minv <- floor(min(profile$PEA_R2)) # Get a min MW
energy <- sum(profile$PEA_R2)/2000 # Calculate the energy
peak_day <- profile %>% #Find a peak day
  group_by(year) %>% 
  filter(PEA_R2 == max(PEA_R2)) %>% 
  pull(datetime)
min_day <- profile %>% #Find a peak day
  group_by(year) %>% 
  filter(PEA_R2 == min(PEA_R2)) %>% 
  pull(datetime)
load_factor <- percent((energy*10^3)/(maxv*8760), 
                       accuracy = 0.01, 
                       decimal.mark = ".")
summary <- tibble(peak_day = peak_day,
                  min_day = min_day,
                  peak_mw = maxv, 
                  min_mw = minv, 
                  energy_gwh = energy,
                  load_factor = load_factor) # combine all data in 1 table
# Plot a profile ####

profile_plot <-
  ggplot() + 
  geom_line(data=profile, 
            aes(x = datetime, 
                y = PEA_R2,
                group = month,
                color = as.factor(month)),
            show.legend = FALSE) +
  ThemeLine +
  labs(x = NULL,
       y = "EGAT sale to PEA R2 (North Eastern region) (MW)")+
  scale_x_datetime(breaks=date_breaks("1 month"), 
                   labels=date_format("%b %y")) +
  scale_y_continuous(breaks = seq(0, round(maxv,-3),1000),
                     limits = c(0, round(maxv, -3))) +  
  scale_color_manual(values = linepalette1) +
  geom_point(data=summary,
             aes(x = peak_day, y = peak_mw))+
  geom_text(data = summary,
            aes(x = peak_day, y = round(maxv, -3)),
            label = glue("Peak {maxv} MW \n@ {peak_day}"))+
  geom_point(data=summary,
             aes(x = min_day, y = min_mw))+
  geom_text(data = summary,
            aes(x = min_day, y = round(minv, -3)),
            label = glue("Minimum {minv} MW \n@ {min_day}"),
            hjust = 0,
            vjust = 1.5)

# Save the output ####
outputfigure <- paste0(outfigdir, "pea_r2_northeastern_egtsle_2019.png")
ggsave(profile_plot, file = outputfigure, dpi = 150, width = 15, height = 5, units = "in", limitsize = FALSE)
profiledata <- c(profiledata, list("pea_r2_northeastern_egtsle_2019" = profile))
profilefigure <- c(profilefigure, list("pea_r2_northeastern_egtsle_2019" = profile_plot))
summarydata <- c(summarydata, list("sum_pea_r2_northeastern_egtsle_2019" = summary))


#----- ____ -----####

#----- The 2019 PEA-R3 (Southern region) EGAT sale profile ----####
# Profile data ####
profile <- 
  read_excel("raw_data/raw_data_profiles/02_Hourly Sale_NetGen_2019.xlsx",
             sheet = "Load Curve",
             range = "C3:G17523"
  ) %>% 
  select(datetime = `Date/Time`, PEA_R3) %>% 
  mutate(date = date(datetime),
         time = format(as.POSIXct(datetime),"%H:%M:%S"),
         year = year(datetime),
         month = month(datetime),
         day = day(datetime)) %>% 
  select(datetime, date, time, year, month, day, PEA_R3) #%>% 
# mutate(strdatetime = as.factor(strptime(glue("{year}-{month}-{day} {time}"), "%Y-%m-%d %H:%M:%S")))


# Summary data ####
maxv <- ceiling(max(profile$PEA_R3)) # Get a peak MW
minv <- floor(min(profile$PEA_R3)) # Get a min MW
energy <- sum(profile$PEA_R3)/2000 # Calculate the energy
peak_day <- profile %>% #Find a peak day
  group_by(year) %>% 
  filter(PEA_R3 == max(PEA_R3)) %>% 
  pull(datetime)
min_day <- profile %>% #Find a peak day
  group_by(year) %>% 
  filter(PEA_R3 == min(PEA_R3)) %>% 
  pull(datetime)
load_factor <- percent((energy*10^3)/(maxv*8760), 
                       accuracy = 0.01, 
                       decimal.mark = ".")
summary <- tibble(peak_day = peak_day,
                  min_day = min_day,
                  peak_mw = maxv, 
                  min_mw = minv, 
                  energy_gwh = energy,
                  load_factor = load_factor) # combine all data in 1 table
# Plot a profile ####

profile_plot <-
  ggplot() + 
  geom_line(data=profile, 
            aes(x = datetime, 
                y = PEA_R3,
                group = month,
                color = as.factor(month)),
            show.legend = FALSE) +
  ThemeLine +
  labs(x = NULL,
       y = "EGAT sale to PEA R3 (Southern region) (MW)")+
  scale_x_datetime(breaks=date_breaks("1 month"), 
                   labels=date_format("%b %y")) +
  scale_y_continuous(breaks = seq(0, round(maxv,-3),1000),
                     limits = c(0, round(maxv, -3))) +  
  scale_color_manual(values = linepalette1) +
  geom_point(data=summary,
             aes(x = peak_day, y = peak_mw))+
  geom_text(data = summary,
            aes(x = peak_day, y = round(maxv, -3)),
            label = glue("Peak {maxv} MW \n@ {peak_day}"))+
  geom_point(data=summary,
             aes(x = min_day, y = min_mw))+
  geom_text(data = summary,
            aes(x = min_day, y = round(minv, -3)),
            label = glue("Minimum {minv} MW \n@ {min_day}"),
            hjust = 0,
            vjust = 1.5)

# Save the output ####
outputfigure <- paste0(outfigdir, "pea_r3_southern_egtsle_2019.png")
ggsave(profile_plot, file = outputfigure, dpi = 150, width = 15, height = 5, units = "in", limitsize = FALSE)
profiledata <- c(profiledata, list("pea_r3_southern_egtsle_2019" = profile))
profilefigure <- c(profilefigure, list("pea_r3_southern_egtsle_2019" = profile_plot))
summarydata <- c(summarydata, list("sum_pea_r3_southern_egtsle_2019" = summary))


#----- ____ -----####

#----- The 2019 PEA-R4 (Northern region) EGAT sale profile ----####
# Profile data ####
profile <- 
  read_excel("raw_data/raw_data_profiles/02_Hourly Sale_NetGen_2019.xlsx",
             sheet = "Load Curve",
             range = "C3:H17523"
  ) %>% 
  select(datetime = `Date/Time`, PEA_R4) %>% 
  mutate(date = date(datetime),
         time = format(as.POSIXct(datetime),"%H:%M:%S"),
         year = year(datetime),
         month = month(datetime),
         day = day(datetime)) %>% 
  select(datetime, date, time, year, month, day, PEA_R4) #%>% 
# mutate(strdatetime = as.factor(strptime(glue("{year}-{month}-{day} {time}"), "%Y-%m-%d %H:%M:%S")))


# Summary data ####
maxv <- ceiling(max(profile$PEA_R4)) # Get a peak MW
minv <- floor(min(profile$PEA_R4)) # Get a min MW
energy <- sum(profile$PEA_R4)/2000 # Calculate the energy
peak_day <- profile %>% #Find a peak day
  group_by(year) %>% 
  filter(PEA_R4 == max(PEA_R4)) %>% 
  pull(datetime)
min_day <- profile %>% #Find a peak day
  group_by(year) %>% 
  filter(PEA_R4 == min(PEA_R4)) %>% 
  pull(datetime)
load_factor <- percent((energy*10^3)/(maxv*8760), 
                       accuracy = 0.01, 
                       decimal.mark = ".")
summary <- tibble(peak_day = peak_day,
                  min_day = min_day,
                  peak_mw = maxv, 
                  min_mw = minv, 
                  energy_gwh = energy,
                  load_factor = load_factor) # combine all data in 1 table
# Plot a profile ####

profile_plot <-
  ggplot() + 
  geom_line(data=profile, 
            aes(x = datetime, 
                y = PEA_R4,
                group = month,
                color = as.factor(month)),
            show.legend = FALSE) +
  ThemeLine +
  labs(x = NULL,
       y = "EGAT sale to PEA R4 (Northern region) (MW)")+
  scale_x_datetime(breaks=date_breaks("1 month"), 
                   labels=date_format("%b %y")) +
  scale_y_continuous(breaks = seq(0, round(maxv,-3)*1.2,1000),
                     limits = c(0, round(maxv, -3)*1.2)) +  
  scale_color_manual(values = linepalette1) +
  geom_point(data=summary,
             aes(x = peak_day, y = peak_mw))+
  geom_text(data = summary,
            aes(x = peak_day, y = round(maxv, -3)*1.2),
            label = glue("Peak {maxv} MW \n@ {peak_day}"))+
  geom_point(data=summary,
             aes(x = min_day, y = min_mw))+
  geom_text(data = summary,
            aes(x = min_day, y = round(minv, -3)),
            label = glue("Minimum {minv} MW \n@ {min_day}"),
            hjust = 0.5,
            vjust = 1.5)

# Save the output ####
outputfigure <- paste0(outfigdir, "pea_r4_northern_egtsle_2019.png")
ggsave(profile_plot, file = outputfigure, dpi = 150, width = 15, height = 5, units = "in", limitsize = FALSE)
profiledata <- c(profiledata, list("pea_r4_northern_egtsle_2019" = profile))
profilefigure <- c(profilefigure, list("pea_r4_northern_egtsle_2019" = profile_plot))
summarydata <- c(summarydata, list("sum_pea_r4_northern_egtsle_2019" = summary))


#----- ____ -----####

#----- The 2019 PEA (All region) EGAT sale profile ----####
# Profile data ####
profile <- 
  read_excel("raw_data/raw_data_profiles/02_Hourly Sale_NetGen_2019.xlsx",
             sheet = "Load Curve",
             range = "C3:I17523"
  ) %>% 
  select(datetime = `Date/Time`, PEA) %>% 
  mutate(date = date(datetime),
         time = format(as.POSIXct(datetime),"%H:%M:%S"),
         year = year(datetime),
         month = month(datetime),
         day = day(datetime)) %>% 
  select(datetime, date, time, year, month, day, PEA) #%>% 
# mutate(strdatetime = as.factor(strptime(glue("{year}-{month}-{day} {time}"), "%Y-%m-%d %H:%M:%S")))


# Summary data ####
maxv <- ceiling(max(profile$PEA)) # Get a peak MW
minv <- floor(min(profile$PEA)) # Get a min MW
energy <- sum(profile$PEA)/2000 # Calculate the energy
peak_day <- profile %>% #Find a peak day
  group_by(year) %>% 
  filter(PEA == max(PEA)) %>% 
  pull(datetime)
min_day <- profile %>% #Find a peak day
  group_by(year) %>% 
  filter(PEA == min(PEA)) %>% 
  pull(datetime)
load_factor <- percent((energy*10^3)/(maxv*8760), 
                       accuracy = 0.01, 
                       decimal.mark = ".")
summary <- tibble(peak_day = peak_day,
                  min_day = min_day,
                  peak_mw = maxv, 
                  min_mw = minv, 
                  energy_gwh = energy,
                  load_factor = load_factor) # combine all data in 1 table
# Plot a profile ####

profile_plot <-
  ggplot() + 
  geom_line(data=profile, 
            aes(x = datetime, 
                y = PEA,
                group = month,
                color = as.factor(month)),
            show.legend = FALSE) +
  ThemeLine +
  labs(x = NULL,
       y = "EGAT sale to PEA (All regions) (MW)")+
  scale_x_datetime(breaks=date_breaks("1 month"), 
                   labels=date_format("%b %y")) +
  scale_y_continuous(breaks = seq(0, round(maxv,-3)*1.1,1000),
                     limits = c(0, round(maxv, -3)*1.1)) +  
  scale_color_manual(values = linepalette1) +
  geom_point(data=summary,
             aes(x = peak_day, y = peak_mw))+
  geom_text(data = summary,
            aes(x = peak_day, y = round(maxv, -3)*1.1),
            label = glue("Peak {maxv} MW \n@ {peak_day}"))+
  geom_point(data=summary,
             aes(x = min_day, y = min_mw))+
  geom_text(data = summary,
            aes(x = min_day, y = round(minv, -3)),
            label = glue("Minimum {minv} MW \n@ {min_day}"),
            hjust = -0.1,
            vjust = 1.5)

# Save the output ####
outputfigure <- paste0(outfigdir, "pea_allregion_egtsle_2019.png")
ggsave(profile_plot, file = outputfigure, dpi = 150, width = 15, height = 5, units = "in", limitsize = FALSE)
profiledata <- c(profiledata, list("pea_allregion_egtsle_2019" = profile))
profilefigure <- c(profilefigure, list("pea_allregion_egtsle_2019" = profile_plot))
summarydata <- c(summarydata, list("sum_pea_allregion_egtsle_2019" = summary))


#----- ____ -----####

#----- The 2019 Direct customer in PEA R1 (Central region) EGAT sale profile ----####
# Profile data ####
profile <- 
  read_excel("raw_data/raw_data_profiles/02_Hourly Sale_NetGen_2019.xlsx",
             sheet = "Load Curve",
             range = "C3:J17523"
  ) %>% 
  select(datetime = `Date/Time`, DCs_R1) %>% 
  mutate(date = date(datetime),
         time = format(as.POSIXct(datetime),"%H:%M:%S"),
         year = year(datetime),
         month = month(datetime),
         day = day(datetime)) %>% 
  select(datetime, date, time, year, month, day, DCs_R1) #%>% 
# mutate(strdatetime = as.factor(strptime(glue("{year}-{month}-{day} {time}"), "%Y-%m-%d %H:%M:%S")))


# Summary data ####
maxv <- ceiling(max(profile$DCs_R1)) # Get a peak MW
minv <- floor(min(profile$DCs_R1)) # Get a min MW
energy <- sum(profile$DCs_R1)/2000 # Calculate the energy
peak_day <- profile %>% #Find a peak day
  group_by(year) %>% 
  filter(DCs_R1 == max(DCs_R1)) %>% 
  pull(datetime)
min_day <- profile %>% #Find a peak day
  group_by(year) %>% 
  filter(DCs_R1 == min(DCs_R1)) %>% 
  pull(datetime)
load_factor <- percent((energy*10^3)/(maxv*8760), 
                       accuracy = 0.01, 
                       decimal.mark = ".")
summary <- tibble(peak_day = peak_day,
                  min_day = min_day,
                  peak_mw = maxv, 
                  min_mw = minv, 
                  energy_gwh = energy,
                  load_factor = load_factor) # combine all data in 1 table
# Plot a profile ####

profile_plot <-
  ggplot() + 
  geom_line(data=profile, 
            aes(x = datetime, 
                y = DCs_R1,
                group = month,
                color = as.factor(month)),
            show.legend = FALSE) +
  ThemeLine +
  labs(x = NULL,
       y = "EGAT sale to direct customers in PEA R1 (Central region) (MW)")+
  scale_x_datetime(breaks=date_breaks("1 month"), 
                   labels=date_format("%b %y")) +
  scale_y_continuous(breaks = seq(0, round(maxv,-2)*1.2,100),
                     limits = c(0, round(maxv, -2)*1.2)) +  
  scale_color_manual(values = linepalette1) +
  geom_point(data=summary,
             aes(x = peak_day, y = peak_mw))+
  geom_text(data = summary,
            aes(x = peak_day, y = round(maxv, -2)*1.2),
            label = glue("Peak {maxv} MW \n@ {peak_day}"))+
  geom_point(data=summary,
             aes(x = min_day, y = min_mw))+
  geom_text(data = summary,
            aes(x = min_day, y = round(minv, -2)),
            label = glue("Minimum {minv} MW \n@ {min_day}"),
            hjust = 0,
            vjust = 1.5)

# Save the output ####
outputfigure <- paste0(outfigdir, "dc_r1_central_egtsle_2019.png")
ggsave(profile_plot, file = outputfigure, dpi = 150, width = 15, height = 5, units = "in", limitsize = FALSE)
profiledata <- c(profiledata, list("dc_r1_central_egtsle_2019" = profile))
profilefigure <- c(profilefigure, list("dc_r1_central_egtsle_2019" = profile_plot))
summarydata <- c(summarydata, list("sum_dc_r1_central_egtsle_2019" = summary))


#----- ____ -----####

#----- The 2019 Direct customer in PEA R2 (North Eastern region) EGAT sale profile ----####
# Profile data ####
profile <- 
  read_excel("raw_data/raw_data_profiles/02_Hourly Sale_NetGen_2019.xlsx",
             sheet = "Load Curve",
             range = "C3:K17523"
  ) %>% 
  select(datetime = `Date/Time`, DCs_R2) %>% 
  mutate(date = date(datetime),
         time = format(as.POSIXct(datetime),"%H:%M:%S"),
         year = year(datetime),
         month = month(datetime),
         day = day(datetime)) %>% 
  select(datetime, date, time, year, month, day, DCs_R2) #%>% 
# mutate(strdatetime = as.factor(strptime(glue("{year}-{month}-{day} {time}"), "%Y-%m-%d %H:%M:%S")))


# Summary data ####
maxv <- ceiling(max(profile$DCs_R2)) # Get a peak MW
minv <- floor(min(profile$DCs_R2)) # Get a min MW
energy <- sum(profile$DCs_R2)/2000 # Calculate the energy
peak_day <- profile %>% #Find a peak day
  group_by(year) %>% 
  filter(DCs_R2 == max(DCs_R2)) %>% 
  pull(datetime)
min_day <- profile %>% #Find a min day
  group_by(year) %>% 
  filter(DCs_R2 == min(DCs_R2)) %>% 
  last() %>%  
  pull(datetime)
load_factor <- percent((energy*10^3)/(maxv*8760), 
                       accuracy = 0.01, 
                       decimal.mark = ".")
summary <- tibble(peak_day = peak_day,
                  min_day = min_day,
                  peak_mw = maxv, 
                  min_mw = minv, 
                  energy_gwh = energy,
                  load_factor = load_factor) # combine all data in 1 table
# Plot a profile ####

profile_plot <-
  ggplot() + 
  geom_line(data=profile, 
            aes(x = datetime, 
                y = DCs_R2,
                group = month,
                color = as.factor(month)),
            show.legend = FALSE) +
  ThemeLine +
  labs(x = NULL,
       y = "EGAT sale to direct customers in PEA R2 (North Eastern region) (MW)")+
  scale_x_datetime(breaks=date_breaks("1 month"), 
                   labels=date_format("%b %y")) +
  scale_y_continuous(breaks = seq(0, round(maxv,-2)*1.2,100),
                     limits = c(0, round(maxv, -2)*1.2)) +  
  scale_color_manual(values = linepalette1) +
  geom_point(data=summary,
             aes(x = peak_day, y = peak_mw))+
  geom_text(data = summary,
            aes(x = peak_day, y = round(maxv, -2)*1.1),
            label = glue("Peak {maxv} MW \n@ {peak_day}"))+
  geom_point(data=summary,
             aes(x = min_day, y = min_mw))+
  geom_text(data = summary,
            aes(x = min_day, y = round(minv, -2)),
            label = glue("Minimum {minv} MW \n@ {min_day}"),
            hjust = 0,
            vjust = 0)

# Save the output ####
outputfigure <- paste0(outfigdir, "dc_r2_northeastern_egtsle_2019.png")
ggsave(profile_plot, file = outputfigure, dpi = 150, width = 15, height = 5, units = "in", limitsize = FALSE)
profiledata <- c(profiledata, list("dc_r2_northeastern_egtsle_2019" = profile))
profilefigure <- c(profilefigure, list("dc_r2_northeastern_egtsle_2019" = profile_plot))
summarydata <- c(summarydata, list("sum_dc_r2_northeastern_egtsle_2019" = summary))


#----- ____ -----####

#----- The 2019 Direct customer in PEA R3 (Southern region) EGAT sale profile ----####
# Profile data ####
profile <- 
  read_excel("raw_data/raw_data_profiles/02_Hourly Sale_NetGen_2019.xlsx",
             sheet = "Load Curve",
             range = "C3:L17523"
  ) %>% 
  select(datetime = `Date/Time`, DCs_R3) %>% 
  mutate(date = date(datetime),
         time = format(as.POSIXct(datetime),"%H:%M:%S"),
         year = year(datetime),
         month = month(datetime),
         day = day(datetime)) %>% 
  select(datetime, date, time, year, month, day, DCs_R3) #%>% 
# mutate(strdatetime = as.factor(strptime(glue("{year}-{month}-{day} {time}"), "%Y-%m-%d %H:%M:%S")))


# Summary data ####
maxv <- ceiling(max(profile$DCs_R3)) # Get a peak MW
minv <- floor(min(profile$DCs_R3)) # Get a min MW
energy <- sum(profile$DCs_R3)/2000 # Calculate the energy
peak_day <- profile %>% #Find a peak day
  group_by(year) %>% 
  filter(DCs_R3 == max(DCs_R3)) %>% 
  pull(datetime)
min_day <- profile %>% #Find a min day
  group_by(year) %>% 
  filter(DCs_R3 == min(DCs_R3)) %>% 
  last() %>%  
  pull(datetime)
load_factor <- percent((energy*10^3)/(maxv*8760), 
                       accuracy = 0.01, 
                       decimal.mark = ".")
summary <- tibble(peak_day = peak_day,
                  min_day = min_day,
                  peak_mw = maxv, 
                  min_mw = minv, 
                  energy_gwh = energy,
                  load_factor = load_factor) # combine all data in 1 table
# Plot a profile ####

profile_plot <-
  ggplot() + 
  geom_line(data=profile, 
            aes(x = datetime, 
                y = DCs_R3,
                group = month,
                color = as.factor(month)),
            show.legend = FALSE) +
  ThemeLine +
  labs(x = NULL,
       y = "EGAT sale to direct customers in PEA R3 (Southern region) (MW)")+
  scale_x_datetime(breaks=date_breaks("1 month"), 
                   labels=date_format("%b %y")) +
  scale_y_continuous(breaks = seq(0, round(maxv,-2),100),
                     limits = c(0, round(maxv, -2))) +  
  scale_color_manual(values = linepalette1) +
  geom_point(data=summary,
             aes(x = peak_day, y = peak_mw))+
  geom_text(data = summary,
            aes(x = peak_day, y = round(maxv, -2)),
            label = glue("Peak {maxv} MW \n@ {peak_day}"))+
  geom_point(data=summary,
             aes(x = min_day, y = min_mw))+
  geom_text(data = summary,
            aes(x = min_day, y = round(minv, -2)),
            label = glue("Minimum {minv} MW \n@ {min_day}"),
            hjust = 0,
            vjust = 0)

# Save the output ####
outputfigure <- paste0(outfigdir, "dc_r3_southern_egtsle_2019.png")
ggsave(profile_plot, file = outputfigure, dpi = 150, width = 15, height = 5, units = "in", limitsize = FALSE)
profiledata <- c(profiledata, list("dc_r3_southern_egtsle_2019" = profile))
profilefigure <- c(profilefigure, list("dc_r3_southern_egtsle_2019" = profile_plot))
summarydata <- c(summarydata, list("sum_dc_r3_southern_egtsle_2019" = summary))


#----- ____ -----####

#----- The 2019 Direct customer in PEA R4 (Northern region) EGAT sale profile ----####
# Profile data ####
profile <- 
  read_excel("raw_data/raw_data_profiles/02_Hourly Sale_NetGen_2019.xlsx",
             sheet = "Load Curve",
             range = "C3:M17523"
  ) %>% 
  select(datetime = `Date/Time`, DCs_R4) %>% 
  mutate(date = date(datetime),
         time = format(as.POSIXct(datetime),"%H:%M:%S"),
         year = year(datetime),
         month = month(datetime),
         day = day(datetime)) %>% 
  select(datetime, date, time, year, month, day, DCs_R4) #%>% 
# mutate(strdatetime = as.factor(strptime(glue("{year}-{month}-{day} {time}"), "%Y-%m-%d %H:%M:%S")))


# Summary data ####
maxv <- ceiling(max(profile$DCs_R4)) # Get a peak MW
minv <- floor(min(profile$DCs_R4)) # Get a min MW
energy <- sum(profile$DCs_R4)/2000 # Calculate the energy
peak_day <- profile %>% #Find a peak day
  group_by(year) %>% 
  filter(DCs_R4 == max(DCs_R4)) %>% 
  pull(datetime)
min_day <- profile %>% #Find a min day
  group_by(year) %>% 
  filter(DCs_R4 == min(DCs_R4)) %>% 
  last() %>%  
  pull(datetime)
load_factor <- percent((energy*10^3)/(maxv*8760), 
                       accuracy = 0.01, 
                       decimal.mark = ".")
summary <- tibble(peak_day = peak_day,
                  min_day = min_day,
                  peak_mw = maxv, 
                  min_mw = minv, 
                  energy_gwh = energy,
                  load_factor = load_factor) # combine all data in 1 table
# Plot a profile ####

profile_plot <-
  ggplot() + 
  geom_line(data=profile, 
            aes(x = datetime, 
                y = DCs_R4,
                group = month,
                color = as.factor(month)),
            show.legend = FALSE) +
  ThemeLine +
  labs(x = NULL,
       y = "EGAT sale to direct customers in PEA R4 (Northern region) (MW)")+
  scale_x_datetime(breaks=date_breaks("1 month"), 
                   labels=date_format("%b %y")) +
  scale_y_continuous(breaks = seq(0, round(maxv,-1),10),
                     limits = c(0, round(maxv, -1))) +  
  scale_color_manual(values = linepalette1) +
  geom_point(data=summary,
             aes(x = peak_day, y = peak_mw))+
  geom_text(data = summary,
            aes(x = peak_day, y = round(maxv, -1)),
            label = glue("Peak {maxv} MW \n@ {peak_day}"),
            hjust = 0.5) +
  geom_point(data=summary,
             aes(x = min_day, y = min_mw))+
  geom_text(data = summary,
            aes(x = min_day, y = round(minv, -1)),
            label = glue("Minimum {minv} MW \n@ {min_day}"),
            hjust = 1,
            vjust = -0.5)

# Save the output ####
outputfigure <- paste0(outfigdir, "dc_r4_northern_egtsle_2019.png")
ggsave(profile_plot, file = outputfigure, dpi = 150, width = 15, height = 5, units = "in", limitsize = FALSE)
profiledata <- c(profiledata, list("dc_r4_northern_egtsle_2019" = profile))
profilefigure <- c(profilefigure, list("dc_r4_northern_egtsle_2019" = profile_plot))
summarydata <- c(summarydata, list("dc_r4_northern_egtsle_2019" = summary))

#----- ____ -----####

#----- The 2019 Direct customer in PEA (All regions) EGAT sale profile ----####
# Profile data ####
profile <- 
  read_excel("raw_data/raw_data_profiles/02_Hourly Sale_NetGen_2019.xlsx",
             sheet = "Load Curve",
             range = "C3:N17523"
  ) %>% 
  select(datetime = `Date/Time`, DCs) %>% 
  mutate(date = date(datetime),
         time = format(as.POSIXct(datetime),"%H:%M:%S"),
         year = year(datetime),
         month = month(datetime),
         day = day(datetime)) %>% 
  select(datetime, date, time, year, month, day, DCs) #%>% 
# mutate(strdatetime = as.factor(strptime(glue("{year}-{month}-{day} {time}"), "%Y-%m-%d %H:%M:%S")))


# Summary data ####
maxv <- ceiling(max(profile$DCs)) # Get a peak MW
minv <- floor(min(profile$DCs)) # Get a min MW
energy <- sum(profile$DCs)/2000 # Calculate the energy
peak_day <- profile %>% #Find a peak day
  group_by(year) %>% 
  filter(DCs == max(DCs)) %>% 
  pull(datetime)
min_day <- profile %>% #Find a min day
  group_by(year) %>% 
  filter(DCs == min(DCs)) %>% 
  last() %>%  
  pull(datetime)
load_factor <- percent((energy*10^3)/(maxv*8760), 
                       accuracy = 0.01, 
                       decimal.mark = ".")
summary <- tibble(peak_day = peak_day,
                  min_day = min_day,
                  peak_mw = maxv, 
                  min_mw = minv, 
                  energy_gwh = energy,
                  load_factor = load_factor) # combine all data in 1 table
# Plot a profile ####

profile_plot <-
  ggplot() + 
  geom_line(data=profile, 
            aes(x = datetime, 
                y = DCs,
                group = month,
                color = as.factor(month)),
            show.legend = FALSE) +
  ThemeLine +
  labs(x = NULL,
       y = "EGAT sale to direct customers in PEA (All regions) (MW)")+
  scale_x_datetime(breaks=date_breaks("1 month"), 
                   labels=date_format("%b %y")) +
  scale_y_continuous(breaks = seq(0, round(maxv,-2)*1.2,100),
                     limits = c(0, round(maxv, -2)*1.2)) +  
  scale_color_manual(values = linepalette1) +
  geom_point(data=summary,
             aes(x = peak_day, y = peak_mw))+
  geom_text(data = summary,
            aes(x = peak_day, y = round(maxv, -2)*1.2),
            label = glue("Peak {maxv} MW \n@ {peak_day}"),
            hjust = 0.5,
            vjust = 1.5) +
  geom_point(data=summary,
             aes(x = min_day, y = min_mw))+
  geom_text(data = summary,
            aes(x = min_day, y = round(minv, -2)),
            label = glue("Minimum {minv} MW \n@ {min_day}"),
            # hjust = 0,
            vjust = 1)

# Save the output ####
outputfigure <- paste0(outfigdir, "dc_allregions_egtsle_2019.png")
ggsave(profile_plot, file = outputfigure, dpi = 150, width = 15, height = 5, units = "in", limitsize = FALSE)
profiledata <- c(profiledata, list("dc_allregions_egtsle_2019" = profile))
profilefigure <- c(profilefigure, list("dc_allregions_egtsle_2019" = profile_plot))
summarydata <- c(summarydata, list("sum_dc_allregions_egtsle_2019" = summary))

#----- ____ -----####

#----- The 2019 PEA R1 & DC R1 (Central region) EGAT sale profile ----####
# Profile data ####
profile <- 
  
  profiledata$pea_r1_central_egtsle_2019%>%
  mutate(dc_r1 = profiledata$dc_r1_central_egtsle_2019$DCs_R1,
         r1_egt_sle = PEA_R1 + dc_r1) %>% 
  select(-PEA_R1, -dc_r1)

# Summary data ####
maxv <- ceiling(max(profile$r1_egt_sle)) # Get a peak MW
minv <- floor(min(profile$r1_egt_sle)) # Get a min MW
energy <- sum(profile$r1_egt_sle)/2000 # Calculate the energy
peak_day <- profile %>% #Find a peak day
  group_by(year) %>% 
  filter(r1_egt_sle == max(r1_egt_sle)) %>% 
  pull(datetime)
min_day <- profile %>% #Find a min day
  group_by(year) %>% 
  filter(r1_egt_sle == min(r1_egt_sle)) %>% 
  last() %>%  
  pull(datetime)
load_factor <- percent((energy*10^3)/(maxv*8760), 
                       accuracy = 0.01, 
                       decimal.mark = ".")
summary <- tibble(peak_day = peak_day,
                  min_day = min_day,
                  peak_mw = maxv, 
                  min_mw = minv, 
                  energy_gwh = energy,
                  load_factor = load_factor) # combine all data in 1 table

# Plot a profile ####

profile_plot <-
  ggplot() + 
  geom_line(data=profile, 
            aes(x = datetime, 
                y = r1_egt_sle,
                group = month,
                color = as.factor(month)),
            show.legend = FALSE) +
  ThemeLine +
  labs(x = NULL,
       y = "EGAT sale to PEA R1 & direct customers (Central region) (MW)")+
  scale_x_datetime(breaks=date_breaks("1 month"), 
                   labels=date_format("%b %y")) +
  scale_y_continuous(breaks = seq(0, round(maxv,-3)*1.1,1000),
                     limits = c(0, round(maxv, -3)*1.1)) +  
  scale_color_manual(values = linepalette1) +
  geom_point(data=summary,
             aes(x = peak_day, y = peak_mw))+
  geom_text(data = summary,
            aes(x = peak_day, y = round(maxv, -3)*1.1),
            label = glue("Peak {maxv} MW \n@ {peak_day}"),
            hjust = 0.5) +
  geom_point(data=summary,
             aes(x = min_day, y = min_mw))+
  geom_text(data = summary,
            aes(x = min_day, y = round(minv, -3)*1.1),
            label = glue("Minimum {minv} MW \n@ {min_day}"),
            hjust = -0.05,
            vjust = 0.5)

# Save the output ####
outputfigure <- paste0(outfigdir, "r1+dcr1_egtsle_2019.png")
ggsave(profile_plot, file = outputfigure, dpi = 150, width = 15, height = 5, units = "in", limitsize = FALSE)
profiledata <- c(profiledata, list("r1+dcr1_egtsle_2019" = profile))
profilefigure <- c(profilefigure, list("r1+dcr1_egtsle_2019" = profile_plot))
summarydata <- c(summarydata, list("sum_r1+dcr1_egtsle_2019" = summary))

#----- ____ -----####

#----- The 2019 PEA R2 & DC R2 (North Eastern region) EGAT sale profile ----####
# Profile data ####
profile <-

profiledata$pea_r2_northeastern_egtsle_2019%>%
  mutate(dc_r2 = profiledata$dc_r2_northeastern_egtsle_2019$DCs_R2,
         r2_egt_sle = PEA_R2 + dc_r2) %>% 
  select(-PEA_R2, -dc_r2)

# Summary data ####
maxv <- ceiling(max(profile$r2_egt_sle)) # Get a peak MW
minv <- floor(min(profile$r2_egt_sle)) # Get a min MW
energy <- sum(profile$r2_egt_sle)/2000 # Calculate the energy
peak_day <- profile %>% #Find a peak day
  group_by(year) %>% 
  filter(r2_egt_sle == max(r2_egt_sle)) %>% 
  pull(datetime)
min_day <- profile %>% #Find a min day
  group_by(year) %>% 
  filter(r2_egt_sle == min(r2_egt_sle)) %>% 
  last() %>%  
  pull(datetime)
load_factor <- percent((energy*10^3)/(maxv*8760), 
                       accuracy = 0.01, 
                       decimal.mark = ".")
summary <- tibble(peak_day = peak_day,
                  min_day = min_day,
                  peak_mw = maxv, 
                  min_mw = minv, 
                  energy_gwh = energy,
                  load_factor = load_factor) # combine all data in 1 table

# Plot a profile ####

profile_plot <-
  ggplot() + 
  geom_line(data=profile, 
            aes(x = datetime, 
                y = r2_egt_sle,
                group = month,
                color = as.factor(month)),
            show.legend = FALSE) +
  ThemeLine +
  labs(x = NULL,
       y = "EGAT sale to PEA R2 & direct customers (Northern Eastern region) (MW)")+
  scale_x_datetime(breaks=date_breaks("1 month"), 
                   labels=date_format("%b %y")) +
  scale_y_continuous(breaks = seq(0, round(maxv,-3)*1.1,1000),
                     limits = c(0, round(maxv, -3)*1.1)) +  
  scale_color_manual(values = linepalette1) +
  geom_point(data=summary,
             aes(x = peak_day, y = peak_mw))+
  geom_text(data = summary,
            aes(x = peak_day, y = round(maxv, -3)*1.1),
            label = glue("Peak {maxv} MW \n@ {peak_day}"),
            hjust = 0.5) +
  geom_point(data=summary,
             aes(x = min_day, y = min_mw))+
  geom_text(data = summary,
            aes(x = min_day, y = round(minv, -3)*1.1),
            label = glue("Minimum {minv} MW \n@ {min_day}"),
            hjust = -0.05,
            vjust = 0.5)

# Save the output ####
outputfigure <- paste0(outfigdir, "r2+dcr2_egtsle_2019.png")
ggsave(profile_plot, file = outputfigure, dpi = 150, width = 15, height = 5, units = "in", limitsize = FALSE)
profiledata <- c(profiledata, list("r2+dcr2_egtsle_2019" = profile))
profilefigure <- c(profilefigure, list("r2+dcr2_egtsle_2019" = profile_plot))
summarydata <- c(summarydata, list("sum_r2+dcr2_egtsle_2019" = summary))

#----- ____ -----####

#----- The 2019 PEA R3 & DC R3 (Southern region) EGAT sale profile ----####
# Profile data ####
profile <-
  
  profiledata$pea_r3_southern_egtsle_2019%>%
  mutate(dc_r3 = profiledata$dc_r3_southern_egtsle_2019$DCs_R3,
         r3_egt_sle = PEA_R3 + dc_r3) %>% 
  select(-PEA_R3, -dc_r3)

# Summary data ####
maxv <- ceiling(max(profile$r3_egt_sle)) # Get a peak MW
minv <- floor(min(profile$r3_egt_sle)) # Get a min MW
energy <- sum(profile$r3_egt_sle)/2000 # Calculate the energy
peak_day <- profile %>% #Find a peak day
  group_by(year) %>% 
  filter(r3_egt_sle == max(r3_egt_sle)) %>% 
  pull(datetime)
min_day <- profile %>% #Find a min day
  group_by(year) %>% 
  filter(r3_egt_sle == min(r3_egt_sle)) %>% 
  last() %>%  
  pull(datetime)
load_factor <- percent((energy*10^3)/(maxv*8760), 
                       accuracy = 0.01, 
                       decimal.mark = ".")
summary <- tibble(peak_day = peak_day,
                  min_day = min_day,
                  peak_mw = maxv, 
                  min_mw = minv, 
                  energy_gwh = energy,
                  load_factor = load_factor) # combine all data in 1 table

# Plot a profile ####

profile_plot <-
  ggplot() + 
  geom_line(data=profile, 
            aes(x = datetime, 
                y = r3_egt_sle,
                group = month,
                color = as.factor(month)),
            show.legend = FALSE) +
  ThemeLine +
  labs(x = NULL,
       y = "EGAT sale to PEA R3 & direct customers (Southern region) (MW)")+
  scale_x_datetime(breaks=date_breaks("1 month"), 
                   labels=date_format("%b %y")) +
  scale_y_continuous(breaks = seq(0, round(maxv,-3),1000),
                     limits = c(0, round(maxv, -3))) +  
  scale_color_manual(values = linepalette1) +
  geom_point(data=summary,
             aes(x = peak_day, y = peak_mw))+
  geom_text(data = summary,
            aes(x = peak_day, y = round(maxv, -3)),
            label = glue("Peak {maxv} MW \n@ {peak_day}"),
            hjust = 0.5) +
  geom_point(data=summary,
             aes(x = min_day, y = min_mw))+
  geom_text(data = summary,
            aes(x = min_day, y = round(minv, -3)*1.1),
            label = glue("Minimum {minv} MW \n@ {min_day}"),
            hjust = -0.05,
            vjust = 0.5)

# Save the output ####
outputfigure <- paste0(outfigdir, "r3+dcr3_egtsle_2019.png")
ggsave(profile_plot, file = outputfigure, dpi = 150, width = 15, height = 5, units = "in", limitsize = FALSE)
profiledata <- c(profiledata, list("r3+dcr3_egtsle_2019" = profile))
profilefigure <- c(profilefigure, list("r3+dcr3_egtsle_2019" = profile_plot))
summarydata <- c(summarydata, list("sum_r3+dcr3_egtsle_2019" = summary))

#----- ____ -----####

#----- The 2019 PEA R4 & DC R4 (Northern region) EGAT sale profile ----####
# Profile data ####
profile <-
  
  profiledata$pea_r4_northern_egtsle_2019%>%
  mutate(dc_r4 = profiledata$dc_r4_northern_egtsle_2019$DCs_R4,
         r4_egt_sle = PEA_R4 + dc_r4) %>% 
  select(-PEA_R4, -dc_r4)

# Summary data ####
maxv <- ceiling(max(profile$r4_egt_sle)) # Get a peak MW
minv <- floor(min(profile$r4_egt_sle)) # Get a min MW
energy <- sum(profile$r4_egt_sle)/2000 # Calculate the energy
peak_day <- profile %>% #Find a peak day
  group_by(year) %>% 
  filter(r4_egt_sle == max(r4_egt_sle)) %>% 
  pull(datetime)
min_day <- profile %>% #Find a min day
  group_by(year) %>% 
  filter(r4_egt_sle == min(r4_egt_sle)) %>% 
  last() %>%  
  pull(datetime)
load_factor <- percent((energy*10^3)/(maxv*8760), 
                       accuracy = 0.01, 
                       decimal.mark = ".")
summary <- tibble(peak_day = peak_day,
                  min_day = min_day,
                  peak_mw = maxv, 
                  min_mw = minv, 
                  energy_gwh = energy,
                  load_factor = load_factor) # combine all data in 1 table

# Plot a profile ####

profile_plot <-
  ggplot() + 
  geom_line(data=profile, 
            aes(x = datetime, 
                y = r4_egt_sle,
                group = month,
                color = as.factor(month)),
            show.legend = FALSE) +
  ThemeLine +
  labs(x = NULL,
       y = "EGAT sale to PEA R4 & direct customers (Northern region) (MW)")+
  scale_x_datetime(breaks=date_breaks("1 month"), 
                   labels=date_format("%b %y")) +
  scale_y_continuous(breaks = seq(0, round(maxv,-3)*1.2,1000),
                     limits = c(0, round(maxv, -3)*1.2)) +  
  scale_color_manual(values = linepalette1) +
  geom_point(data=summary,
             aes(x = peak_day, y = peak_mw))+
  geom_text(data = summary,
            aes(x = peak_day, y = round(maxv, -3)*1.2),
            label = glue("Peak {maxv} MW \n@ {peak_day}"),
            hjust = 0.5) +
  geom_point(data=summary,
             aes(x = min_day, y = min_mw))+
  geom_text(data = summary,
            aes(x = min_day, y = round(minv, -3)*1.1),
            label = glue("Minimum {minv} MW \n@ {min_day}"),
            hjust = 1,
            vjust = 1)

# Save the output ####
outputfigure <- paste0(outfigdir, "r4+dcr4_egtsle_2019.png")
ggsave(profile_plot, file = outputfigure, dpi = 150, width = 15, height = 5, units = "in", limitsize = FALSE)
profiledata <- c(profiledata, list("r4+dcr4_egtsle_2019" = profile))
profilefigure <- c(profilefigure, list("r4+dcr4_egtsle_2019" = profile_plot))
summarydata <- c(summarydata, list("sum_r4+dcr4_egtsle_2019" = summary))

#----- ____ -----####

#----- The 2019 EGAT sale profile ----####
# Profile data ####
profile <-
  
  profiledata$mea_egtsle_2019%>%
  mutate(r1_dc1 = profiledata$"r1+dcr1_egtsle_2019"$r1_egt_sle,
         r2_dc2 = profiledata$"r2+dcr2_egtsle_2019"$r2_egt_sle,
         r3_dc3 = profiledata$"r3+dcr3_egtsle_2019"$r3_egt_sle,
         r4_dc4 = profiledata$"r4+dcr4_egtsle_2019"$r4_egt_sle,
         egt_sle = MEA + r1_dc1 + r2_dc2 + r3_dc3 + r4_dc4)
          


library(tidyverse) #For data manipulation
library(readxl) #For reading the excel sheet
library(scales)
library(glue)

## Create general theme for figures
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



# Make a list of profile to store data

profiledata <- list()
summarydata <- list()

# The 2019 MEA EGAT sale profile
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

 
    
#profile_plot <-
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

profiledata <- c(profiledata, list("mea_egtsle_2019"=data1))

#data1<- 
  read_excel("raw_data/raw_data_profiles/02_Hourly Sale_NetGen_2019.xlsx",
                   sheet = "Load Curve",
                   range = "C3:I17523"
                  ) %>% 
        select(datetime = `Date/Time`, PEA)    


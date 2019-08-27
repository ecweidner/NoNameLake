##  ------------------------------------------------------------------------- ##

## DAYMET data for No Name Lake (area)

## -------------------------------------------------------------------------- ##


 # packages
   library(RNRCS) 
   library(daymetr)
   library(tidyverse)
   library(reshape2)
   library(lubridate)
   library(zoo) #as.yearmon
   library(ggfortify)
   library(dataRetrieval) # addwateryear
   library(ggpubr)



## Get SNOTEL data ---------------------------------------------------------- ##


 # download snotel information using RNRCS package
   snotel <- grabNRCS.data(network   = "SNTL",
                           site_id   = 815,
                           timescale = "daily",
                           DayBgn    = "1980-10-01",
                           DayEnd    = "2018-09-30"
                           )


 # rename columns
   snotel.rename <- snotel %>%
     rename(Date        = "Date",
            MeanAirTemp = "Air.Temperature.Average..degF.",
            MaxAirTemp  = "Air.Temperature.Maximum..degF.",
            MinAirTemp  = "Air.Temperature.Minimum..degF.",
            ObsAirTemp  = "Air.Temperature.Observed..degF..Start.of.Day.Values",
            PrcpAccum   = "Precipitation.Accumulation..in..Start.of.Day.Values",
            SnowDepth   = "Snow.Depth..in..Start.of.Day.Values",
            SnowWaterEq = "Snow.Water.Equivalent..in..Start.of.Day.Values"
            )



 # Some of these data look suspect. Lots of NAs for the firet 8 years, to be 
 # expected. When AirTemps come online, many months were MinAirTemp and 
 # MaxAirTemp are 32 degrees F.


## Format ------------------------------------------------------------------- ##


 # change date as character to as.Date and make new column for year and month
   snotel.format <- snotel.rename %>%
     
     # parse dates
     mutate(Date  = ymd(Date)) %>%
  
     # make two new columns for year and month - use year as a grouping var
     mutate(Year     = lubridate::year(Date), 
            Month    = lubridate::month(Date)) %>%
  
     # change year to factor and use constant year for x-axis
     # this starts in jan...would like it to start in october
     mutate(Year = factor(year(Date))) %>%
  
     # mutate new column for cm (swe), C (temp), and mm (prcp)
     mutate(SnowWaterEq_cm = SnowWaterEq * 2.54) %>%
     mutate(PrcpAccum_cm   = PrcpAccum * 2.54) %>%
     mutate(MeanAirTemp_C  = (MeanAirTemp - 32) / 1.8) %>%
     mutate(MaxAirTemp_C   = (MaxAirTemp - 32) / 1.8) %>%
     mutate(MinAirTemp_C   = (MinAirTemp - 32) / 1.8) %>%
     
     # select new columns
     select(Date,
            Year,
            Month,
            MeanAirTemp_C, 
            MaxAirTemp_C, 
            MinAirTemp_C, 
            PrcpAccum_cm,
            SnowWaterEq_cm)

 # add water year to our dataframe
   snotel.format <- addWaterYear(snotel.format)     


 # rename column for consistency
   snotel.format <- rename(snotel.format,
                           WaterYear = waterYear) 


 # add day of water year
   snotel.wy <- snotel.format %>%
     
     # group water year to add day of the water year
     group_by(WaterYear) %>%
     
     # give me day of year (doy)
     mutate(WyDoy = seq(1:n())) %>%
     
     # mutate a water year month oct = 1, nov = 2 etc.]
     mutate(WaterYearMonth = recode(Month,
                                    '10' = '1',
                                    '11' = '2',
                                    '12' = '3',
                                    '1'  = '4',
                                    '2'  = '5',
                                    '3'  = '6',
                                    '4'  = '7',
                                    '5'  = '8',
                                    '6'  = '9',
                                    '7'  = '10',
                                    '8'  = '11',
                                    '9'  = '12')) %>%
                                    
     # reorder columns
     select(Date, WaterYear, WaterYearMonth, WyDoy, 
            MeanAirTemp_C, MaxAirTemp_C, MinAirTemp_C,
            PrcpAccum_cm, SnowWaterEq_cm)

   
 # create a dataframe with only 2017 and 2018
   snotel.wy.filtered <- snotel.wy %>%
     filter(WaterYear == 2016 | WaterYear == 2017 | WaterYear == 2018)  


## Create a layered plot for SWE -------------------------------------------- ##


 # plot daily swe
   plot.daily.swe  <- ggplot() +
     
     # define all years
     geom_line(data = snotel.wy,
               color = alpha("dark grey", 0.9),
               aes(x     = WyDoy, 
                   y     = SnowWaterEq_cm, 
                   group = WaterYear)
               ) +
  
     # define highlighted lines
     geom_line(data = snotel.wy.filtered, 
               size = 0.9,
               aes(x     = WyDoy, 
                   y     = SnowWaterEq_cm, 
                   group = WaterYear, 
                   color = factor(WaterYear))
               ) +
  
     # map colors
     scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  
     # define scale of x-axis
     scale_x_continuous(breaks = c(1, 32, 62, 93, 
                                   123, 151, 182, 212, 
                                   243, 273, 304, 335, 365  ), 
                        labels = c("Oct 1", "Nov 1", "Dec 1", "Jan 1",
                                   "Feb 1", "Mar 1", "Apr 1", "May 1",
                                   "Jun 1", "Jul 1", "Aug 1", "Sep 1",
                                   "Oct 1")
                        ) +
     
     # set theme (for publication)
     theme_pubr() +
     
     # adjust theme elements...i should add these into the theme_pubr
     theme(plot.title   = element_text(size   = 16),
           axis.text.x  = element_text(angle  = 45,
                                       size   = 12,
                                       vjust  = 1.0,
                                       hjust  = 1.0),
           axis.title.x = element_text(size   = 14),
           axis.text.y  = element_text(size   = 12),
           axis.title.y = element_text(size   = 14),
           strip.text.x = element_text(face   = "bold"),
           plot.margin  = margin(1, 1, 1, 1, "cm")) +
  
     # label axes and give graphic a title
     labs(x        = "\nMonth",
          y        = "Snow Water Equivalent (cm)\n",
          title    = "Three Creeks Meadow SNOTEL Site (1980 - 2018)",
          color     = "Water Year")


## Plot monthly averaged swe from 1980-2018 --------------------------------- ##

   
   # aggregate daily data to month, take max swe for the month
   snotel.wy.month <- snotel.wy %>%
     
     # group by month
     group_by(WaterYearMonth, WaterYear) %>%
     
     # summarize by month
     summarise(SnowWaterEq_max = max(SnowWaterEq_cm))
   
   
 # subset to 2016 - 2018 water years
   snotel.wy.month.filtered <- snotel.wy.month %>%
     
     # filter from Oct 2015 (start of wy 2016) to Sep 2018 (end of wy 2018)
     filter(WaterYear == 2016 | WaterYear == 2017 | WaterYear == 2018)  
   
   
 # specify level order by water year months for both dataframes
   snotel.wy.month$WaterYearMonth <- factor(snotel.wy.month$WaterYearMonth,
                                                     levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
   
   snotel.wy.month.filtered$WaterYearMonth <- factor(snotel.wy.month.filtered$WaterYearMonth,
                                                     levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))


 # mean swe plot
   plot.mean.swe <- ggplot(data = snotel.wy.month,
                           aes(x = WaterYearMonth,
                               y = SnowWaterEq_max)) +
     
     # define points
     geom_point(color = "black",
                alpha = 1/5) +
    
     # summary stats
     stat_summary(fun.y = "mean",
                  geom  = "point",
                  size  = 3,
                  color = "red") +
    
     geom_point(data = snotel.wy.month.filtered,
                aes(x = WaterYearMonth,
                    y = SnowWaterEq_max,
                    group = WaterYear, 
                    color = factor(WaterYear)),
                size = 2) +
    
     # map colors
     scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
    
    
     scale_x_discrete(labels = c("Oct", "Nov", "Dec",
                                 "Jan", "Feb", "Mar", 
                                 "Apr", "May", "Jun", 
                                 "Jul", "Aug", "Sep")) +
    
     # set theme (for publication)
     theme_pubr() +
    
    # adjust theme elements...i should add these into the theme_pubr
     theme(plot.title   = element_text(size   = 16),
           axis.text.x  = element_text(angle  = 45,
                                       size   = 12,
                                       vjust  = 1.0,
                                       hjust  = 1.0),
           axis.title.x = element_text(size   = 14),
           axis.text.y  = element_text(size   = 12),
           axis.title.y = element_text(size   = 14),
           strip.text.x = element_text(face   = "bold"),
           plot.margin  = margin(1, 1, 1, 1, "cm")) +
    
     # label axes and give graphic a title
     labs(x        = "\nMonth",
          y        = "Snow Water Equivalent (cm)\n",
          title    = "Three Creeks Meadow SNOTEL Site (1980 - 2018)",
          subtitle = "",
          color    = "Water Year")


## Precipitation ------------------------------------------------------------ ##
   

 # aggregate daily data to month, take max swe for the month
   snotel.wy.month.prcp <- snotel.wy %>%
      
      # group by month
      group_by(WaterYearMonth, WaterYear) %>%
      
      # summarize by month
      summarise(PrcpAccum_max = max(PrcpAccum_cm))
   
   
   # subset to 2016 - 2018 water years
   snotel.wy.month.prcp.filtered <- snotel.wy.month.prcp %>%
      
      # filter from Oct 2015 (start of wy 2016) to Sep 2018 (end of wy 2018)
      filter(WaterYear == 2016 | WaterYear == 2017 | WaterYear == 2018)  
   
   
   # specify level order by water year months for both dataframes
   snotel.wy.month.prcp$WaterYearMonth <- factor(snotel.wy.month.prcp$WaterYearMonth,
                                            levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
   
   snotel.wy.month.prcp.filtered$WaterYearMonth <- factor(snotel.wy.month.prcp.filtered$WaterYearMonth,
                                                     levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
   
   
   # mean swe plot
   plot.mean.prcp <- ggplot(data = snotel.wy.month.prcp,
                           aes(x = WaterYearMonth,
                               y = PrcpAccum_max)) +
      
      # define points
      geom_point(color = "black",
                 alpha = 1/5) +
      
      # summary stats
      stat_summary(fun.y = "mean",
                   geom  = "point",
                   size  = 3,
                   color = "red") +
      
      geom_point(data = snotel.wy.month.prcp.filtered,
                 aes(x = WaterYearMonth,
                     y = PrcpAccum_max,
                     group = WaterYear, 
                     color = factor(WaterYear)),
                 size = 2) +
      
      # map colors
      scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
      
      
      scale_x_discrete(labels = c("Oct", "Nov", "Dec",
                                  "Jan", "Feb", "Mar", 
                                  "Apr", "May", "Jun", 
                                  "Jul", "Aug", "Sep")) +
      
      # set theme (for publication)
      theme_pubr() +
      
      # adjust theme elements...i should add these into the theme_pubr
      theme(plot.title   = element_text(size   = 16),
            axis.text.x  = element_text(angle  = 45,
                                        size   = 12,
                                        vjust  = 1.0,
                                        hjust  = 1.0),
            axis.title.x = element_text(size   = 14),
            axis.text.y  = element_text(size   = 12),
            axis.title.y = element_text(size   = 14),
            strip.text.x = element_text(face   = "bold"),
            plot.margin  = margin(1, 1, 1, 1, "cm")) +
      
      # label axes and give graphic a title
      labs(x        = "\nMonth",
           y        = "Precipitation Accumulation (cm)\n",
           title    = "Three Creeks Meadow SNOTEL Site (1980 - 2018)",
           subtitle = "",
           color    = "Water Year")
   
   
 # save workspace
   save.image(file = "SNOTEL_NoNameLake.RData")

   
## -------------------------------------------------------------------------- ##
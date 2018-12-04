library(plyr)
library(readr)
library(ggplot2)
library(DataExplorer)

url1 = "https://raw.githubusercontent.com/frm1789/landslide-ea/master/global_landslide_catalog_export.csv"
df <- read_csv(url1)

v <- c("1/","2/","3/","4/","5/","6/","7/","8/","9/")
df$year <- substring(df$event_date, 9, 10) #year

## 1.2 How many events by year?
count_byyear <- dplyr::count(df, year = df$year)

a <- ggplot(df, aes(df$year)) +
  geom_histogram(stat="count", binwidth = 1, fill = "#453781FF") +
  #scale_fill_manual(guide = FALSE) + 
  labs(
    title ="Landslide 2007 - 2016", subtitle = "Distribution of events by year", 
    caption = "source: GLC by NASA Goddard Space Flight Center \n by thinkingondata.com"
  ) +
  theme_minimal() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())
a
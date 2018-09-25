library(readr)
library(ggplot2)
library(dplyr)
library(countrycode)

url_land <- 'https://raw.githubusercontent.com/frm1789/landslide-ea/master/landslides.csv'
df_land <- read_csv(url(url_land))
df <- df_land

## 1. Data format
v <- c("1/","2/","3/","4/","5/","6/","7/","8/","9/")
df$date <- ifelse((substring(df$date, 1, 2) %in% v), (paste0('0',df$date)), df$date) #day
df$year <- substring(df$date, 6, 8) #year
df$year <-gsub("/", "", df$year)

## 1.2 Delete columns with more than 80% of NA
df <- subset(df, select=-c(time,continent_code,location_description))

## Change some values extremely large
df[which(df[,3] == "Dominican Republic"), 3] <- "Dominican R."
df[which(df[,3] == "Trinidad and Tobago"), 3] <- "Trinidad & Tob."
df[which(df[,3] == "Saint Vincent and the Grenadines"), 3] <- "St. Vincent"
df[which(df[,3] == "U.S. Virgin Islands"), 3] <- "US Virgin Isl."

## without NA = date
df <- df[complete.cases(df[ , 2]),]

## 1. Exploratory analysis
## 1.1 Q events
nrow(df) # 1690
b
## 1.2 Set of colors for each graph from Viridis Palette
#453781FF #287d8EFF #55C667FF #fde725ff

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

## 1.3 How many events by country?
df_cou <- dplyr::count(df, country = df$country_name)
df_cou <- df_cou[order(df_cou$n, decreasing = TRUE), ]  # sort
df_cou$country <- factor(df_cou$country, levels = df_cou$country)  # convert to factor to retain sorted order in plot.

value <- mean(df()

b <- ggplot(data=df_cou, aes(x = country, 
                             y = n))  +
  geom_bar(stat="identity", fill = "#287d8EFF") +
  geom_vline(aes(xintercept = mean(n)), linetype="longdash", color="black")  +
  # geom_smooth (method = 'loess', se = FALSE, color="red") +
  labs(
    title ="Landslide 2007 - 2016", subtitle = "Ranking by quantity of events per country", 
    caption = "source: GLC by NASA Goddard Space Flight Center \n by thinkingondata.com",
    x="Countries", y = "Q"
  ) +
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45,hjust=1),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())
b


## 1.4 Q people afected by landslide by country

df_pop <- df %>% 
  group_by(country = country_name) %>% 
  summarise(n = sum(population))

df_pop$n <- round(df_pop$n/1000, digits=0)


df_pop <- df_pop[order(df_pop$n, decreasing = TRUE), ]  # sort
df_pop$country <- factor(df_pop$country, levels = df_pop$country)  # convert to factor to retain sorted order in plot.

c <- ggplot(data=df_pop, aes(x = country, 
                             y = n))  +
  geom_bar(stat="identity", fill = "#3cbb75ff") +
  # geom_smooth (method = 'loess', se = FALSE, color="red") +
  labs(
    title ="Landslide 2007 - 2016", subtitle = "Total people affected by landslides per country (in thousands)", 
    caption = "source: GLC by NASA Goddard Space Flight Center \n by thinkingondata.com",
    x="Countries", y = "Q"
  ) +
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45,hjust=1),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())
c

#4. Worst disaster by % affected by population
#1. Crear tabla temporal
df_t <- df %>%
  select(id, country_name, country_code, population, year)

#2. Buscar tabla con codigos de 3 letras y hacer el match
url_code <- 'https://raw.githubusercontent.com/frm1789/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv'
df_code <- read_csv(url(url_code))

df_t <- dplyr::mutate(df_t, (df_t$country_new_code = countrycode(df_t$country_name, 'country.name', 'iso3c')))
df_t$year <- paste0('20',df_t$year)
colnames(df_t)[6] <- "country_3"              

url_population <- 'https://raw.githubusercontent.com/frm1789/landslide-ea/master/population_all_countries_2007_2016.csv'
df_population <- read_csv(url(url_population))
colnames(df_population)[2] <- "country_3" 

df_t$year = as.numeric(df_t$year)

library(tidyverse)
df_t <- df_population %>%
  gather(year, value_that_year, -country_3) %>%
  mutate(year = as.numeric(sub("X", "", year))) %>%
  right_join(df_t)

#2.1 Obtener proporcion entre poblacion afectada y poblacion total
colnames(df_t)[3] <- "total_pop"
colnames(df_t)[7] <- "affected_pop"
df_t$affected_pop = as.numeric(df_t$affected_pop)
df_t$total_pop = as.numeric(df_t$total_pop)

df_t$perc_pop = (df_t$affected_pop / df_t$total_pop)*100

#2.2. Obtener en total cuales fueron los peores desastres 
df_t$r_perc_pop = round((df_t$affected_pop / df_t$total_pop)*100, digits = 2)

df_t <- df_t[order(df_t$r_perc_pop, decreasing = TRUE), ]  # sort


df_t$id <- factor(df_t$id, levels = df_t$id)  # convert to factor to retain sorted order in plot.

top_10 <- dplyr::top_n(df_t, 10)
top_10$id <-  seq(1, 10, by=1)
top_10$label <- paste(top_10$id, ".", sep="")
top_10$label <- paste(top_10$label, top_10$country_name, top_10$year, sep=" ")

top_10$label <- factor(top_10$label, levels = top_10$label)  # convert to factor to retain sorted order in plot.

d <- ggplot(data=top_10, aes(x = label, y = r_perc_pop))  +
              geom_bar(stat="identity", fill = "#fde725ff") +
              # geom_smooth (method = 'loess', se = FALSE, color="red") +
              labs(
                title ="Landslide 2007 - 2016", subtitle = "Worst disasters by people affected in relation to total population", 
                caption = "source: GLC by NASA Goddard Space Flight Center \n by thinkingondata.com",
                x="Countries", y = "%"
              ) +
              theme_minimal()+
              theme(axis.text.x=element_text(angle=45,hjust=1),
                    axis.title.x=element_blank(),
                    axis.title.y=element_blank())
d 


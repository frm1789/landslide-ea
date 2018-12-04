library(readr)
library(ggplot2)
library(gridExtra)
library(forcats)

url_new = "https://raw.githubusercontent.com/frm1789/landslide-ea/master/Landslides_clean.csv"
df_new = read_csv(url_new)



a = ggplot(df_new, aes(x=fct_infreq(df_new$landslide_size))) +
  geom_histogram(stat="count", fill = "#453781FF") +
  theme_minimal() +
  labs(
    title ="Histogram for Landslides sizes",
    #caption = "source: Global Landslide Catalog (GLC) from NASA\nauthor: thinkingondata.com",
    x = "Q - Total landslides by size",
    y = "Types of sizes")+
  theme(axis.text.x=element_text(angle=45,hjust=1))

b = ggplot(df_new, aes(x=fct_infreq(df_new$landslide_category))) +
  geom_histogram(stat="count", fill = "#453781FF") +
  theme_minimal() +
  labs(
    title ="Histogram for Landslides categories",
    #caption = "source: Global Landslide Catalog (GLC) from NASA\nauthor: thinkingondata.com",
    x = "Q - Total landslides by category",
    y = "Categories")+
  theme(axis.text.x=element_text(angle=45,hjust=1))

df_new$landslide_category = ifelse(df_new$landslide_category == "snow_avalanche", "snow",df_new$landslide_category)
df_new$landslide_category = ifelse(df_new$landslide_category == "riverbank_collapse", "riverbank",df_new$landslide_category)
df_new$landslide_category = ifelse(df_new$landslide_category == "translational_slide", "trans_slide",df_new$landslide_category)


c = ggplot(df_new, aes(x=fct_infreq(df_new$landslide_trigger))) +
  geom_histogram(stat="count", fill = "#453781FF") +
  theme_minimal() +
  labs(
    title ="Histogram for Landslides Triggers",
    caption = "source: Global Landslide Catalog (GLC) from NASA",
    x = "Q - Total landslides by trigger",
    y = "Types of Triggers")+
  theme(axis.text.x=element_text(angle=45,hjust=1))

df_new$landslide_trigger = ifelse(df_new$landslide_trigger == "dam_embankment_collapse", "embank_coll",df_new$landslide_trigger)
df_new$landslide_trigger = ifelse(df_new$landslide_trigger == "no_apparent_trigger", "no_trigger",df_new$landslide_trigger)
df_new$landslide_trigger = ifelse(df_new$landslide_trigger == "snowfall_snowmelt", "snowfall",df_new$landslide_trigger)


d= ggplot(df_new, aes(x=fct_infreq(df_new$landslide_setting))) +
  geom_histogram(stat="count", fill = "#453781FF") +
  theme_minimal() +
  labs(
    title ="Histogram for Landslides settings",
    caption = "author: thinkingondata.com",
    x = "Q - Total landslides by settings",
    y = "Types of settings")+
  theme(axis.text.x=element_text(angle=45,hjust=1))

##histogram

gridExtra::grid.arrange(a,b,c,d)
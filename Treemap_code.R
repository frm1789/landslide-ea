library(readr)
library(ggplot2)
library(treemapify)

url_t = "https://raw.githubusercontent.com/frm1789/landslide-ea/master/df_treemap.csv"
df_t = read_csv(url_t)

ggplot2::ggplot(df_t, ggplot2::aes(area = n, label = country)) +
  geom_treemap(aes(alpha = v_alpha), fill = "#453781FF") +
  geom_treemap_text(fontface = "italic", colour = "white", place = "centre",
                    grow = TRUE)
library(fastmatch)

df$landslide_size = fmatch(df$landslide_size, c("small","medium", "large","very_large", "catastrophic", "unknown","NA"))

df$landslide_category = fmatch(df$landslide_category, cat)
#categories
cat = c("mudslide", "landslide", "complex", "rock_fall","debris_flow", 
  "snow", "unknown", "other","trans_slide", "earth_flow", "lahar",
  "creep", "topple", "NA")

df$landslide_trigger = fmatch(df$landslide_trigger, tri)

tri = c("downpour", "rain", "unknown", "continuos_rain","tropical_cyclone", 
        "snow_fall", "moonsoon", "mining","earthquake", "construction", "flooding",
        "no_trigger", "freeze_thaw", "other", "embank_coll", "leaking_pipe", "vibration",
        "volcano","NA")

df$landslide_setting = fmatch(df$landslide_setting, set)

set = c("unknown","above_road","natural_slope", "urban","below_road","mine","above_river",
        "deforested_slope","other","bluff","retaining_wall","burned_area","engineered_slope",
        "above_coast","NA")

df = df[ , -which(names(df) %in% c("X1","injury_count","country_name","year"))]

#Remember the 1s are because everything is perfectly correlated with itself, 
#and the NAs are because there are NAs in your variables. To avoid that we are using: use = "complete.obs"

v_cor = cor(df, use = "complete.obs")

corrplot(v_cor, method="square",type = "upper", order = "hclust", 
         tl.col = "black")



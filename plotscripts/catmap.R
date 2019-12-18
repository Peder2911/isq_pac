
source("prep.R")

# ================================================
library(sf)

data <- predictions_2010_2018 %>% 
   filter(year == 2018)

cinfo <- read.csv("data/country_info.csv", stringsAsFactors = FALSE) %>%
   select(gwcode, alpha3)

shapes <- read_sf("data/ne/ne_50m_admin_0_countries_lakes.shp") %>%
   merge(cinfo, by.x = "ADM0_A3_US", by.y = "alpha3")

data <- data %>%
   mutate(cat = case_when(
      major_prob > 0.1 ~ "p(major) > 0.1",
      minor_prob > 0.3 & major_prob < 0.1 ~ "p(minor)>0.3 & p(major) < 0.1",
      minor_prob + major_prob > 0.1 ~ "p(minor) + p(major) > 0.1",
      TRUE ~ "p(minor) + p(major) < 0.1"),

      cat = fct_relevel(cat,
      "p(major) > 0.1",
      "p(minor) + p(major) > 0.1",
      "p(minor)>0.3 & p(major) < 0.1",
      "p(minor) + p(major) < 0.1")
   ) 
   #merge(shapes, "gwcode", all.y = TRUE)
   
data <- merge(shapes,data,"gwcode",all.y = TRUE)

data <- data[!is.na(data$cat), ]

dots <- data %>%
   select(minor_actual, major_actual, geometry) %>%
   filter(major_actual == 1 | minor_actual) %>%
   mutate(

      conflict = case_when(
      major_actual == 1 ~ "Major",
      minor_actual == 1 ~ "Minor",
      TRUE ~ "No conflict"),

      conflict = fct_relevel(conflict,
         "Major",
         "Minor",
         "No conflict")
   ) %>%
   st_centroid()

map <- ggplot(data) +
   geom_sf(aes(geometry = geometry, fill = cat)) +
   geom_sf(data = dots, aes(color = conflict), size = 5) +
   scale_fill_manual(values = c(
      "#d7191c",
      "#fdae61",
      "#abdda4",
      "#2b83ba"
   )) +
   scale_color_manual(values = c(
      "#ffff33",      
      "#ff3933",
      "#ffff33"      
   ))

size = 14
ggsave(glue("{PLOTFOLDER}/catmap.pdf"),map,device = "pdf", height = 1 * size, width = 1.414 * size)

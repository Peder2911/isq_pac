source("prep.R")

## @knitr figure_5_prep

con <- dbConnect(SQLite(),DB)

# N countries is given by number of unique country-codes in prediction
# dataset
n_countries <- 169
nCountries <- dbGetQuery(con,"
                         SELECT * FROM ncountries")

propInConflict <- dbGetQuery(con, "
   SELECT DISTINCT year, gwcode, MAX(intensity_level) AS intensity_level FROM acd
   WHERE year > 1969
   GROUP BY year, gwcode") %>%
   merge(nCountries,"year") %>%
   group_by(year) %>%
   summarize(countries = length(unique(gwcode)),
             minor = sum(intensity_level == 1),
             major = sum(intensity_level == 2),
             #inminor = (countries - major) / n_countries,
             inmajor = (countries - minor) / max(n_countries),
             inconflict = countries / max(n_countries)) %>%
   select(year,
          inmajor,
          #inminor,
          inconflict) %>%
   gather(var,val,-year)

propPredicted <- dbGetQuery(con,"
   SELECT year,minor,major,combined FROM predictions_2010_2050 
   WHERE year > 2008 AND year < 2050") %>%
   merge(nCountries,"year") %>%
   group_by(year) %>%
   summarise(#major50 = sum(major > 0.5) / nCountries,
             #major30 = sum(major > 0.3) / nCountries,
             #inmajor = sum(major > 0.3) / max(n_countries),
             inmajor = mean(major),
             #minor50 = sum(minor > 0.5) / nCountries,
             #minor30 = sum(minor > 0.3) / nCountries,
             #inminor = sum(minor > 0.3) / max(n_countries),
             #comb50 = sum(combined > 0.5) / nCountries,
             #comb30 = sum(combined > 0.3) / nCountries) %>%
             inconflict = mean(combined)) %>%
             #inconflict = sum(combined > 0.3) / max(n_countries)) %>%
   gather(var,val,-year)

propInConflict$type <- "Actual"
propPredicted$type <- "Predicted"
both <- rbind(propInConflict, propPredicted)

both$var <- fct_recode(both$var,
                       Minor = "inminor",
                       Major = "inmajor",
                       Either = "inconflict")

confint <- dbGetQuery(con,"SELECT * FROM confint") %>% 
   pivot_longer(cols = c(ends_with("lower"),ends_with("upper")),"type","value") %>% 
   separate("type",into = c("int","type"),sep = "_") %>% 
   pivot_wider(names_from=c("type"),values_from=c("value")) %>%
   mutate(int = str_to_title(int))

dbDisconnect(con)

## @knitr figure_5_output

colors <- c(comb30 = "#800080",
            inconflict = "#bf08a4",
            Either = "#bf08a4",

            inmajor = "#7eb6ff",
            Major = "#7eb6ff",
            major30 = "#7eb6ff",

            inminor  = "#c8001f",
            Minor = "#c8001f",
            minor30 = "#c8001f")

fig5 <- ggplot(both,aes(x=year)) + 
   geom_ribbon(data=confint,aes(x=year,ymin=lower*100,ymax=upper*100,fill=int), alpha = 0.3, show.legend = FALSE) +
   geom_line(size = 2,aes(linetype=type,color=var,y=val * 100)) + 
   scale_x_continuous(breaks = seq(1970,2050,5), 
                      limits = c(1970,2048), expand = c(0,0)) +
   scale_discrete_manual(c("color","fill"),values = colors) + 
   geom_vline(xintercept = 2018) +
   geom_vline(xintercept = 2010) +
   theme(
   ) +
   labs(x = "Year", y = "% In conflict", color = "", linetype = "") +
   theme_classic() +
   theme(
      legend.position = "bottom",
      panel.grid.major.y = element_line(size = 1, color = "lightgrey"),
      text = element_text(size = 20),
      panel.border = element_rect(fill = "transparent", size = 1),
      axis.line = element_blank(), 
      axis.ticks.length = unit(0.25,"cm"),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 18)
      )
ggsave(glue("{PLOTFOLDER}/timeline.{DEVICE}"),fig5,device = DEVICE,height = 6, width = 12) 
fig5

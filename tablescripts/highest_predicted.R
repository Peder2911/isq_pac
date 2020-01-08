
library(UnidecodeR)
library(tictoc)

source("prep.R")

# ================================================

data_t3 <- predictions_2010_2018 


latestConflicts <- occurrence %>%
   select(gwcode,year,either_actual) %>%
   group_by(gwcode) %>% 
   filter(either_actual == 1) %>%
   summarize(latestyear = max(year))

before09 <- occurrence %>%
   select(gwcode,year,either_actual) %>%
   filter(year < 2010) %>%
   group_by(gwcode) %>%
   filter(either_actual == 1) %>%
   summarize(before09 = max(year))

data_t3 <- data_t3 %>%
   filter(year == 2018) %>%
   merge(latestConflicts, "gwcode",all.x = TRUE) %>%
   merge(before09, "gwcode",all.x = TRUE) %>%
   mutate(`2018` = case_when(
      major_actual == 1 ~ "Major",
      minor_actual == 1 ~ "Minor",
      TRUE ~ "None"),
      `Before 2018` = ifelse(!is.na(latestyear),
                             as.character(latestyear),
                             " - "),
      `Before 2009` = ifelse(!is.na(before09),
                             as.character(before09),
                             " - ")) %>%
   mutate(Country = unidecode(name,"all")) %>%
   select(Country, 
          `Before 2018`,
          `Before 2009`,
          `2018`,
          Either = combined,
          Minor = minor_prob,
          Major = major_prob) %>%
   arrange(-Either) 

saveRDS(data_t3,"/tmp/tee.rds")

ilprob <- data_t3[data_t3$Country == "Israel","Either"]

err <- data_t3 %>%
   filter(Either < ilprob,
      `2018` != "None")

top45 <- head(data_t3, 45)

top50 <- rbind(top45, err)



latex_table <- kable(top50, "latex", booktabs = TRUE, digits = 4) %>%
   kable_styling(latex_options = c("hold_position"),
                 font_size = 7) %>%
   pack_rows("Highest combined probability", 1, 45) %>% 
   pack_rows("Notable prediction errors", 46, 48) %>% 
   add_header_above(c("","Latest observed" = 2,"Observed","Predicted 2018" = 3)) %>%
   stripTableEnvir()

cat("Writing table\n")
writeLines(latex_table,glue("{TABLEFOLDER}/highest_predicted.tex"))

# ================================================


source("prep.R")

# ================================================
data <- predictions_2010_2018 %>% filter(year == 2018)

ilprob <- data[data$name == "Israel","combined"]
cat(ilprob)

coi <- data %>%
   filter(combined < ilprob,
      either_actual == 1) %>%
   mutate(
      Occurrence = case_when(
         major_actual == 1 ~ "Major conflict",
         minor_actual == 1 ~ "Minor conflict",
         TRUE ~ "No conflict"
      )
   ) %>%
   select(
      Country = name,
      `p(combined)` = combined,
      `p(minor)` = minor_prob,
      `p(major)` = major_prob,
      Occurrence
) %>%
   arrange(-`p(combined)`)

kable(coi, "rst")

kable(coi, "latex", booktabs = TRUE, digits = 4) %>%
   writeLines(glue("{TABLEFOLDER}/errors.tex"))

# ================================================

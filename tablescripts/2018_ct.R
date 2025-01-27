
source("prep.R")

# ================================================



data <- predictions_2010_2018 %>% 
   filter(year == 2018)

nrow(data)

data <- data %>%
   mutate(cat = case_when(
      major_prob > 0.1 ~ "a",
      minor_prob > 0.3 & major_prob < 0.1 ~ "b",
      minor_prob + major_prob > 0.1 ~ "c",
      TRUE ~ "d"
   )) %>%
   group_by(cat) %>%
   summarize(
      none = n()-sum(either_actual),
      minor = sum(minor_actual),
      major = sum(major_actual),
      cnt = n()
   )

data <- data.frame(rbind(
   data,
   c("",sapply(data[2:ncol(data)],sum))
))

data$cat <- c(
   "p(major) > 0.1",
   "p(minor) > 0.3 & p(major) < 0.1",
   "p(minor) + p(major) > 0.1",
   "p(minor) + p(major) < 0.1",
   "Sum"
)

names(data) <- c(
   "Category",
   "No Conflict",
   "Minor conflict",
   "Major conflict",
   "Total"
)

#   )) %>%
#   summarize(
#      `p(major) > 0.1` = sum(cat == "a"),
#      `p(minor) > 0.3 & p(major) < 0.1` = sum(cat == "b"),
#      `p(minor) + p(major) > 0.1` = sum(cat == "c"),
#      `p(minor) + p(major) < 0.1` = sum(cat == "d")
#   ) %>%
#   gather(Class,Count)

write.csv(data,"/tmp/dat.csv")
kable(data,"rst")
#sum(data[,2])

kable(data,"latex", booktabs = TRUE) %>%
   stripTableEnvir() %>%
   writeLines(glue("{TABLEFOLDER}/2018_ct.tex"))

# ================================================

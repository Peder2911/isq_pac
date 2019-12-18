# =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  =  = 
# Preparation and constants 
#
#

shh <- suppressPackageStartupMessages
shh(library(evallib))
shh(library(dplyr))
shh(library(zoo))
shh(library(ggplot2))
shh(library(xtable))
shh(library(glue))
shh(library(knitr))
shh(library(kableExtra))
shh(library(gridExtra))
shh(library(stringr))
shh(library(RSQLite))
shh(library(tidyr))
shh(library(grid))
shh(library(forcats))

source("functions.R")

set.seed(1618915)

NITER <- 20
DB <- "data/pac.sqlite"

DEVICE <- "pdf"
PLOTHEIGHT <- 7 
PLOTWIDTH <- 9.9 

PLOTFOLDER <- "plots"
TABLEFOLDER <- "tables"

predictions <- " SELECT DISTINCT * FROM predictions_2010_2050 WHERE year <= 2018 " %>%
   getfrom(DB)

oos <- "SELECT * FROM predictions_2001_2009" %>% 
   getfrom(DB)

# ================================================
# Conflict years in the United States, except for 2001, are filtered out. This
# is because the conflicts do not occur in the United States proper, even
# though the United States is a party to the conflicts. 

occurrence <- "SELECT * FROM acd" %>%
   getfrom(DB) %>%
   filter(!(gwcode == 2 & ! year == 2001),
      year > 1988) 

years <- do.call(seq,as.list(range(occurrence$year)))
countries <- unique(predictions$gwcode)
blank <- lapply(countries, function(cntry){
   data.frame(gwcode = cntry, year = years)
}) %>%
   do.call(rbind, .)

occurrence <- merge(blank,occurrence, all.x = TRUE)
occurrence[is.na(occurrence)] <- 0

occurrence <- occurrence %>%
   group_by(gwcode) %>%
   onsetAndTerm(minor_actual, major_actual, either_actual) %>%
   ungroup()

cinfo <- "SELECT countries.gwcode, name, regions.regionname FROM countries
          JOIN regions ON countries.isqregion = regions.isqregion" %>%
   getfrom(DB)

# ================================================
# Functions that prepare the prediction data for
# further analysis.

# this first one adds thresholded "outcome" values: 

addThresholdedOutcomes <- function(predictions){
   predictions %>%
      rename(minor_prob = minor,
             major_prob = major) %>%
      mutate(minor_pred_30 = as.numeric(minor_prob > 0.3),
             minor_pred_50 = as.numeric(minor_prob > 0.5),
             major_pred_30 = as.numeric(major_prob > 0.3),
             major_pred_50 = as.numeric(major_prob > 0.5),
             either_pred_30 = as.numeric(minor_pred_30 | major_pred_30),
             either_pred_50 = as.numeric(minor_pred_50 | major_pred_50))
}

# the second one merges the predictions with "actual" occurrence
# values 

addOccurrence <- function(predictions, occurrence, cinfo){
   res <- predictions %>%
      merge(occurrence, c("gwcode","year"), all.y = TRUE) %>%
      merge(cinfo,by="gwcode")
   res[is.na(res)] <- 0
   res
}

# An alias for applying both of the above, plus removing
# duplicates and arranging by year / gwcode 

prepPredictions <- function(predictions){
   predictions %>%
   addThresholdedOutcomes()  %>%
   addOccurrence(occurrence, cinfo) %>%
   unique() %>%
   arrange(gwcode, year)
}

# These are the data-frames that are subsequently analyzed
predictions_2010_2018 <- prepPredictions(predictions) %>% filter(
   year > 2009 & year < 2019)
predictions_2001_2009 <- prepPredictions(oos) %>% filter(
   year > 2000 & year < 2010)

# Just some tests for peace-of-mind

purrr::walk(list(predictions_2010_2018, predictions_2001_2009), function(dat){
   # Is each row a unique country-year?
   if(assertthat::are_equal(nrow(dat), 
      length(unique(dat$gwcode)) * length(unique(dat$year)))){
      cat("Each row is a unique country-year\n")
   } else {
      stop("Failed assertion: Each row is not a unique country-year!")
   }
})

cat("The data are ready!!\n")

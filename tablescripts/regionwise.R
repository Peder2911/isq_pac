
source("prep.R")

# ================================================
## @knitr table_4_prep

data_t4 <- predictions_2010_2018


get_region_results <- function(data){
   getMetric <- function(data,metric,thresh){
      # Get metric using a confusion matrix of predictions,
      # given thresh, and actual outcomes. 
      withConfmat(as.numeric(data[[paste0("major_pred_",thresh)]]|
                             data[[paste0("minor_pred_",thresh)]]),
                  as.numeric(data$major_actual|data$minor_actual), metric)
   }

   regions <- unique(data$regionname)
   lapply(regions, function(region){
      # Build a list of results of several metrics
      sub <- data[data$regionname == region,]
      res <- tryCatch(aucWithCI(sub$combined, 
         as.numeric(sub$major_actual | sub$minor_actual)), 
         error = function(e){list(score = NA, quantiles = c(NA,NA))})

      #res$q1 <- res$quantiles[1]
      #res$q2 <- res$quantiles[2]
      res$quantiles <- NULL

      #res$tpr_50 <- getMetric(sub,recall,"50") 
      #res$fpr_50 <- getMetric(sub,fallout,"50") 
      #res$tpr_30 <- getMetric(sub,recall,"30") 
      #res$fpr_30 <- getMetric(sub,fallout,"30") 
      res
   }) %>%
      bind_rows()
}



## @knitr table_4_output
options(knitr.kable.NA = " - ")

regions <- unique(predictions_2010_2018$region)
results <- do.call(cbind, lapply(list(predictions_2001_2009,predictions_2010_2018),
   get_region_results))

results <- cbind(regions,results)
colnames(results) <- c("Region","2001-2009","2010-2018")
kable(results,"rst")

str <- knitr::kable(results, "latex", 
                  booktabs = TRUE, digits = 3, linesep="") %>%
   add_header_above(c("","AUROC" = 2)) 
str %>%
   stripTableEnvir() %>%
   writeLines(glue("{TABLEFOLDER}/regionwise.tex"))

# ================================================

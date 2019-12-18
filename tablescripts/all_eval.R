
source("prep.R")

## @knitr table_2_prep
library(parallel)

data_t2 <- predictions_2010_2018 

curves <- list()

# ===================================================
# Functions

auprc <- function(pred,act){
   d <- metricCurve(pred,act, x = recall, y = precision)
   curves <<- c(list(d), curves)
   abs(auc(x = d$recall, y = d$precision))
}


summaryTable <- function(data){

   # Calculating recall and fallout
   posrates <- function(data,thresh){
      preds <- as.numeric(data[[paste0("major_pred_",thresh)]] |
                          data[[paste0("minor_pred_",thresh)]])
      actual <- either(data, major_actual, minor_actual)

      tpr = withConfmat(preds,actual,recall)
      fpr = withConfmat(preds,actual,fallout)

      list(tpr = tpr,fpr = fpr)
   }

   p50 <- posrates(data,50)
   p30 <- posrates(data,30)

   variables <- c(onset = "onset_",
                  term = "term_",
                  incidence = "")

   curves <- lapply(variables, function(var){

      probs <- data$combined
      outcomes <- as.numeric(data[[paste0(var,"either_actual")]])

      list(auc = aucWithCI(probs, outcomes),
           auprc = auprc(probs, outcomes))

   })
   
   putprob <- function(p) c(round(p, digits = 3),rep("-",2))
   cidisplay <- function(pair){
      p <- glue("({round(pair[1],digits = 3)} - {round(pair[2], digits = 3)})")
      p
   }

   tibble::tibble(
      ` ` = c("Incidence","Onset","Termination"),

      Score = c(curves$incidence$auc$score,
                curves$onset$auc$score,
                curves$term$auc$score),

      `DeLong CI` = sapply(list(
         curves$incidence$auc$quantiles,
         curves$onset$auc$quantiles,
         curves$term$auc$quantiles), cidisplay),

      AUPRC = c(curves$incidence$auprc,
                curves$onset$auprc,
                curves$term$auprc),

      `TPR_5` = putprob(p50$tpr),
      `FPR_5` = putprob(p50$fpr),
      `TPR_3` = putprob(p30$tpr),
      `FPR_3` = putprob(p30$fpr))
}

renderSummaryTable <- function(table){
   fnames <- names(table)
   fnames[4] <- "Score"
   fnames[c(5,7)] <- "TPR"
   fnames[c(6,8)] <- "FPR"
   knitr::kable(table,"latex",booktabs = TRUE, digits = 3, col.names = fnames) %>%
      add_header_above(c("","AUROC"=2,"AUPRC","p > 0.5" = 2, "p > 0.3" = 2)) %>%
      pack_rows("2001-2009",1,3) %>%
      pack_rows("2007-2009",4,6) %>%
      pack_rows("2010-2018",7,9) %>%
      pack_rows("2016-2018",10,12)
}


# These are rendered below:
table <- mclapply(list(
      predictions_2001_2009,
      filter(predictions_2001_2009, year %in% c(2007,2008,2009)),
      predictions_2010_2018,
      filter(predictions_2010_2018, year %in% c(2016,2017,2018))
   ), summaryTable, mc.cores = detectCores()-1) %>%
   do.call(rbind, .)

knitr::kable(table,"rst")

renderSummaryTable(table) %>%
   stripTableEnvir() %>%
   writeLines(glue("{TABLEFOLDER}/all_eval.tex"))

## @knitr table_2_output
#x <- renderSummaryTable(tab)
#writeLines(x,glue("{TABLEFOLDER}/table_2_1.tex"))

## @knitr table_2_output_16_18
#x <- renderSummaryTable(tab_16_18)
#writeLines(x,glue("{TABLEFOLDER}/table_2_2.tex"))


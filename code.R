
## @knitr prepData 
# Here data is retrieved from the database and prepared for analysis.

NITER <- 20
DB <- "data/pac.sqlite"

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

# the second one merges the predicitons with "actual" occurrence
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

## @knitr table_2_prep

curves <- list()
auprc <- function(pred,act){
   d <- metricCurve(pred,act, x = recall, y = precision)
   curves <<- c(list(d), curves)
   abs(auc(x = d$recall, y = d$precision))
}

data_t2 <- predictions_2010_2018 

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

   # AUC From two kinds of curves
   variables <- c(onset = "onset_",
                  term = "term_",
                  incidence = "")

   curves <- lapply(variables, function(var){

      probs <- data$combined
      # Might change this to either_actual?
      outcomes <- as.numeric(data[[paste0(var,"either_actual")]])
      #outcomes <- as.numeric(data[[paste0(var,"major_actual")]]|
      #                       data[[paste0(var,"minor_actual")]])

      list(auc = aucWithCI(probs, outcomes),
           auprc = auprc(probs, outcomes))

   })
   
   # Just to make the table prettier
   putprob <- function(p) c(round(p, digits = 3),rep("-",2))

   tibble::tibble(

      AUROC = c(curves$incidence$auc$score,
                curves$onset$auc$score,
                curves$term$auc$score),

      `0.25th` = c(curves$incidence$auc$quantiles[1],
                   curves$onset$auc$quantiles[1],
                   curves$term$auc$quantiles[1]),

      `97.5th` = c(curves$incidence$auc$quantiles[2],
                   curves$onset$auc$quantiles[2],
                   curves$term$auc$quantiles[2]),

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
   fnames[c(5,7)] <- "TPR"
   fnames[c(6,8)] <- "FPR"
   row.names(table) <- c("Incidence", "Onset", "Termination")
   knitr::kable(table,"latex",booktabs = TRUE, digits = 3, col.names = fnames) %>%
      add_header_above(c("","","DeLong Quantiles" = 2,"","0.5" = 2, "0.3" = 2))
}

# These are rendered below:
tab <- summaryTable(data_t2)
tab_16_18 <- summaryTable(filter(data_t2, year %in% c(2016,2017,2018)))

## @knitr table_2_output
x <- renderSummaryTable(tab)
writeLines(x,glue("{TABLEFOLDER}/table_2_1.tex"))
x
saveRDS(data_t2,"tee/t2.rds")

## @knitr table_2_output_16_18
x <- renderSummaryTable(tab_16_18)
writeLines(x,glue("{TABLEFOLDER}/table_2_2.tex"))
x

## @knitr table_4_prep

data_t4 <- predictions_2010_2018

regions <- unique(data_t4$regionname)

region_results <- lapply(regions, function(region){
      # Build a list of results of several metrics

      sub <- data_t4[data_t4$regionname == region,]
      message(glue::glue("{region}: {unique(as.numeric(sub$major_actual|sub$minor_actual))}"))
      res <- tryCatch(aucWithCI(sub$combined, as.numeric(sub$major_actual | sub$minor_actual)), error = function(e){list(score = NA, quantiles = c(NA,NA))})
      getMetric <- function(data,metric,thresh){
         # Get metric using a confusion matrix of predictions,
         # given thresh, and actual outcomes. 
         withConfmat(as.numeric(data[[paste0("major_pred_",thresh)]]|
                                data[[paste0("minor_pred_",thresh)]]),
                     as.numeric(data$major_actual|data$minor_actual), metric)
      }

      res$q1 <- res$quantiles[1]
      res$q2 <- res$quantiles[2]
      res$quantiles <- NULL

      res$tpr_50 <- getMetric(sub,recall,"50") 
      res$fpr_50 <- getMetric(sub,fallout,"50") 
      res$tpr_30 <- getMetric(sub,recall,"30") 
      res$fpr_30 <- getMetric(sub,fallout,"30") 

      res
   }) %>%
   bind_rows()
region_results <- cbind(regions,region_results)

## @knitr table_4_output

columnNames <- c("Region","AUC", "0.25th","97.5th","TPR","FPR","TPR","FPR")

x <- knitr::kable(region_results, "latex", 
                  booktabs = TRUE, digits = 3, 
                  col.names = columnNames)%>%
   add_header_above(c("","","Quantiles" = 2,".50" = 2, ".30" = 2))

writeLines(x,glue("{TABLEFOLDER}/table_4.tex"))
x

## @knitr figure_4_prep
message("start of chunk")

data_f4 <- predictions_2010_2018

variables <- c(
   onset = "onset_",
   incidence = ""
)

prc <- function(p,a) metricCurve(p,a,recall,precision)
roc <- function(p,a) metricCurve(p,a,fallout,recall)

curvetypes <- list(
   pr = prc,
   roc = roc
)

message("starting")

curves <- lapply(list(oos = predictions_2001_2009, neo = predictions_2010_2018),
                 function(dat){

      lapply(curvetypes , function(fn){
         data <- lapply(variables, function(var){
            call <- c(list(dat$combined, dat[[paste0(var, "either_actual")]]))
            c <- do.call(fn, call)

            #c <- metricCurve(dat$combined, dat[[paste0(var,"either_actual")]],
            #   fnset[[1]], fnset[[2]])
            if(var == "") var <- "Incidence"
            c$type <- str_replace_all(var,"[^A-Za-z]","")%>%
               str_to_title()
            c
         }) %>% do.call(rbind, .)
      })
   })
message("got it done")

for(n in c("pr","roc")){
   curvenames <- c("01-09","10-18")
   for(i in c(1,2)){
      curves[[i]][[n]]$name <- curvenames[i]
   }
}

colors <- c("01-09" = "#0093c9",
            "10-18" = "#c8001f")

scalesettings <- list(
   limits = c(0,1), expand = c(0,0), breaks = seq(0,1,0.1)
)

combroc <- ggplot(rbind(curves[[1]]$roc,curves[[2]]$roc), 
                  aes(x = fallout, y = recall, color = name, linetype = type))+
   geom_path(size = 1, alpha = 0.8) + 
   scale_color_manual(values=colors) +
   scale_linetype_manual(values=c("solid","dotted"))+
   labs(x="False Positive Rate",y="True Positive Rate", color = "Years", linetype = "Type") +
   #scale_x_continuous(limits = c(0,1), expand = c(0,0)) + 
   #scale_y_continuous(limits = c(0,1), expand = c(0,0)) + 
   do.call(scale_x_continuous, scalesettings) + 
   do.call(scale_y_continuous, scalesettings) + 
   theme_classic() +
   theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
      text = element_text(size = 25)) +
   coord_fixed()

combpr <- ggplot(rbind(curves[[1]]$pr,curves[[2]]$pr), 
                  aes(x = recall, y = precision, color = name, linetype = type))+
   geom_path(size = 1, alpha = 0.8) + 
   scale_color_manual(values=colors) +
   labs(x = "Recall",y="Precision", color = "Years", linetype = "Type") +
   scale_linetype_manual(values=c("solid","dotted"))+
   #scale_x_continuous(limits = c(0,1), expand = c(0,0)) + 
   #scale_y_continuous(limits = c(0,1), expand = c(0,0)) + 
   do.call(scale_x_continuous, scalesettings) + 
   do.call(scale_y_continuous, scalesettings) + 
   theme_classic() +
   theme(panel.border = element_rect(color = "black", fill = NA, size = 1),
      text = element_text(size=25))+
   coord_fixed()

aucs <- sapply(curves, function(cohort){
   sapply(cohort, function(curve){
      curve <- filter(curve, type == "Incidence")
      r <- auc(curve[[2]], curve[[3]])      
      message(r)
      r
   })
})

message(aucs)
#aucs <- sapply(list(list(curves[[1]]$roc$fallout,curves[[1]]$roc$recall),
                    #list(curves[[2]]$roc$fallout,curves[[2]]$roc$recall),
                    #list(curves[[1]]$pr$recall,curves[[1]]$pr$precision),
                    #list(curves[[2]]$pr$recall,curves[[2]]$pr$precision)),
               #function(args){do.call(auc,args)})

aucs <- data.frame(Groups = c("01-09","10-18","01-09","10-18"),
                   AUC = abs(aucs))

## @knitr figure_4_output
# Deprecated?

auc <- plt$results$auc
dgts <- 4
aucCaption <- glue("AUC: {round(auc$score, dgts)} ({round(auc$quantiles[1], dgts)} - {round(auc$quantiles[2], dgts)})")

x <- plt$plot + 
   labs(x = "Specificity",
        y = "Sensitivity",
        title = "Bootstrap ROC curve",
        subtitle = aucCaption, 
        caption = glue("({NITER} random draws)"))

ggsave(glue("{PLOTFOLDER}/figure_4.{DEVICE}"), x,
       device = DEVICE,height = PLOTHEIGHT, width = PLOTWIDTH) 

x

## @knitr figure_4_prc
ggsave(glue("{PLOTFOLDER}/rocs.{DEVICE}"),combroc,device = DEVICE,height = PLOTHEIGHT, width = PLOTWIDTH) 
combpr

## @knitr figure_4_ROC
ggsave(glue("{PLOTFOLDER}/precrec.{DEVICE}"),combpr,device = DEVICE,height = PLOTHEIGHT, width = PLOTWIDTH) 
combroc

## @knitr figure_4_table

x <- kable(aucs,"latex",booktabs = TRUE, digits = 4) %>%
   kable_styling(latex_options = c("hold_position")) %>%
   pack_rows("ROC Curve",1,2) %>%
   pack_rows("Precision-Recall Curve", 3,4)

writeLines(x,glue("{TABLEFOLDER}/curves_AUCs.tex"))

x

## @knitr confmat_prep

data_confmat <- predictions_2010_2018

regions <- unique(data_confmat$regionname)
names(regions) <- regions
regions <- c(".*",regions)

results <- lapply(c("0.5"=0.5,"0.3"=0.3), function(threshold){

   results <- lapply(regions, function(region){
      sub <- data_confmat[str_detect(data_confmat$regionname,region),]
      confmat(as.numeric(sub$combined > threshold),
              either(sub, major_actual, minor_actual)) %>%
         as.numeric()
   }) %>%
      bind_rows() %>%
      t()

}) %>%
   do.call(cbind, .) %>%
   as.data.frame()

## @knitr confmat_output

names(results) <- rep(c("TN","FN","FP","TP"),2)
regions[1] <- "All"
row.names(results) <- regions 

x <- knitr::kable(results, "latex", booktabs = TRUE, row.names = TRUE) %>%
   kable_styling(latex_options = c("hold_position")) %>%
   add_header_above(c("","P > 0.5" = 4, "P > 0.3" = 4))
writeLines(x,glue("{TABLEFOLDER}/conftable.tex"))
x

## @knitr table_3

data_t3 <- predictions_2010_2018 

latestConflicts <- occurrence %>%
   group_by(gwcode) %>% 
   summarize(latestyear = max(year))

before09 <- occurrence %>%
   select(gwcode,year) %>%
   filter(year < 2010) %>%
   group_by(gwcode) %>%
   summarize(before09 = max(year))

top50 <- data_t3 %>%
   filter(year == 2018) %>%
   merge(latestConflicts, "gwcode") %>%
   merge(before09, "gwcode") %>%
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
   select(Country = name,
          `Before 2018`,
          `Before 2009`,
          `2018`,
          Either = combined,
          Minor = minor_prob,
          Major = major_prob) %>%
   arrange(-Either) %>%
   head(50)

x <- kable(top50, "latex", booktabs = TRUE, digits = 4) %>%
   kable_styling(latex_options = c("hold_position"),
                 font_size = 7) %>%
   add_header_above(c("","Latest observed" = 2,"Observed","Predicted 2018" = 3))
writeLines(x,glue("{TABLEFOLDER}/table_3.tex"))
x

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

fig5 <- ggplot(both,aes(x=year,y=val * 100,color=var, linetype = type)) + 
   geom_line(size = 2) + 
   scale_x_continuous(breaks = seq(1970,2050,2), 
                      limits = c(1970,2048), expand = c(0,0)) +
   scale_color_manual(values = colors) + 
   geom_vline(xintercept = 2018) +
   geom_vline(xintercept = 2009) +
   theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7)) +
   labs(x = "Year", y = "% In conflict", color = "", linetype = "") +
   theme(legend.position = "bottom")
ggsave(glue("{PLOTFOLDER}/figure_5.{DEVICE}"),fig5,device = DEVICE,height = PLOTHEIGHT, width = PLOTWIDTH) 
fig5

## @knitr oos_table_2

predictions_2001_2009 <- predictions_2001_2009 %>%
   group_by(gwcode) %>%
   onsetAndTerm(minor_actual,major_actual,
                minor_pred_50,minor_pred_30,
                major_pred_50,major_pred_30) %>%
   escalation(actual, pred_30, pred_50) %>%
   ungroup()

incidence01_09 <- list()

incidence01_09$score <- aucFromPA(predictions_2001_2009$combined, 
                                  predictions_2001_2009$either_actual)

incidence01_09$quantiles <- pROC::ci.auc(predictions_2001_2009$either_actual, 
                                         predictions_2001_2009$combined)

subset <- predictions_2001_2009 %>% filter(year > 2006)

onset <- bootstrappedROC(subset$combined, as.numeric(subset$onset_minor_actual |
                                                     subset$onset_major_actual),
                         res = 0.1, draws = NITER,
                         parallel = TRUE)[[2]]

term <- bootstrappedROC(subset$combined, as.numeric(subset$term_minor_actual |
                                                    subset$term_major_actual),
                         res = 0.1, draws = NITER,
                         parallel = TRUE)[[2]]

incidence <- list()
incidence$score <- aucFromPA(subset$combined, subset$either_actual)
incidence$quantiles <- pROC::ci.auc(subset$either_actual, subset$combined)

getMetric <- function(data,thresh,metric){ 
   tpr = withConfmat(as.numeric(data$combined > thresh), 
                      either(data, major_actual, minor_actual), metric)
}

getRates <- function(data, functions = list(recall = recall, fallout = fallout), 
                     digits = NULL){ 
   lapply(list(th_05 = 0.5, th_03 = 0.3),
          function(th){
      lapply(functions,
             function(met) {
         m <- getMetric(data,th,met)
         if(!is.null(digits)){m <- round(m,digits = digits)}
         m
      })
   })
}


rates <- lapply(list(full = predictions_2001_2009,post07 = subset), 
                function(dat){
                   getRates(dat)
                })


res <- tibble(Type = c("Incidence",
                       "Incidence",
                       "Onset",
                       "Termination"),
              Score = c(incidence01_09$score,
                      incidence$score,
                      onset$score,
                      term$score),
              `0.25th` = c(incidence01_09$quantiles[1],
                           incidence$quantiles[1],
                           onset$quantiles[1],
                           term$quantiles[1]),
              `97.5th` = c(incidence01_09$quantiles[3],
                           incidence$quantiles[3],
                           onset$quantiles[2],
                           term$quantiles[2]),
              `TPR > 0.5` = c(rates$full$th_05$recall,
                      rates$post07$th_05$recall,
                      NA, NA),
              `FPR > 0.5` = c(rates$full$th_05$fallout,
                      rates$post07$th_05$fallout,
                      NA, NA), 
              `TPR > 0.3` = c(rates$full$th_03$recall,
                      rates$post07$th_03$recall,
                      NA, NA),
              `FPR > 0.3` = c(rates$full$th_03$fallout,
                      rates$post07$th_03$fallout,
                      NA, NA) 
              )

fixnames <- names(res)
fixnames[1] <- ""
fixnames[c(5,7)] <- "TPR"
fixnames[c(6,8)] <- "FPR"

x <- kable(res, "latex", digits = 3, booktabs = TRUE, col.names = fixnames) %>%
   add_header_above(c(" " = 1, "AUC" = 3, "0.5" = 2, "0.3" = 2)) %>%
   pack_rows("01-09",1,1) %>%
   pack_rows("07-09",2,4)
writeLines(x,"tables/table_2_oos.tex")
x

## @knitr oos_figure_4

plt <- cintervalplot(predictions_2001_2009$combined, 
                     predictions_2001_2009$either_actual, res = 0.001)
ggsave(glue("{PLOTFOLDER}/figure_4_oos.{DEVICE}"), plt$plot,
       device = DEVICE,height = PLOTHEIGHT, width = PLOTWIDTH) 
plt$plot

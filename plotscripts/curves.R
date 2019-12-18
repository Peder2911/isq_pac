
source("prep.R")

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

## @knitr figure_4_prc
ggsave(glue("{PLOTFOLDER}/rocs.{DEVICE}"),combroc,device = DEVICE,height = PLOTHEIGHT, width = PLOTWIDTH) 

## @knitr figure_4_ROC
ggsave(glue("{PLOTFOLDER}/precrec.{DEVICE}"),combpr,device = DEVICE,height = PLOTHEIGHT, width = PLOTWIDTH) 

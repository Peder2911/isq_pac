
library(separationplot)
source("prep.R")

#separationplot <- function(x,y){
   #drop <- is.na(x) | is.na(y)
   #x <- x[!drop]
   #y <- y[!drop]
   #separationplot(x,y)
#}

predictions_2010_2018 <- predictions_2010_2018[c("combined","major_prob","either_actual","major_actual")]
predictions_2010_2018 <- predictions_2010_2018[complete.cases(predictions_2010_2018),]

predictions_2001_2009 <- predictions_2001_2009[c("combined","major_prob","either_actual","major_actual")]
predictions_2001_2009 <- predictions_2001_2009[complete.cases(predictions_2001_2009),]

separationplot(predictions_2010_2018$combined,predictions_2010_2018$either_actual)
dev.off()

separationplot(predictions_2001_2009$combined,predictions_2001_2009$either_actual)
dev.off()

separationplot(predictions_2010_2018$major_prob,predictions_2010_2018$major_actual)
dev.off()

separationplot(predictions_2001_2009$major_prob,predictions_2001_2009$major_actual)
dev.off()


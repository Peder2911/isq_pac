library(RSQLite)

source("prep.R")
cinfo <- getfrom("SELECT gwcode, alpha3 FROM countries","data/pac.sqlite")
predictions_2010_2018 <- merge(predictions_2010_2018,cinfo,on="gwcode")
predictions_2010_2018$label <- paste0(predictions_2010_2018$alpha3,"-",predictions_2010_2018$year)
write.csv(predictions_2010_2018,"cache/p2010_2018.csv",row.names=FALSE)
write.csv(predictions_2010_2018[predictions_2010_2018$year>=2016&predictions_2010_2018$year<=2018,],"cache/p2016_2018.csv",row.names=FALSE)


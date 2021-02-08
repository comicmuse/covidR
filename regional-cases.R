library(curl)
library(zoo)
library(tidyr)
library(dplyr)



cases<-read.csv(curl("https://coronavirus.data.gov.uk/api/v1/data?filters=areaType=region;date>2021-01-04&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22newCasesByPublishDate%22:%22newCasesByPublishDate%22,%22cumCasesByPublishDate%22:%22cumCasesByPublishDate%22%7D&format=csv"), header=TRUE, stringsAsFactors=FALSE)

casesbyregion<- cases %>% select (-areaCode, -areaType, -cumCasesByPublishDate) %>% pivot_wider(names_from = areaName, values_from=newCasesByPublishDate, values_fn=mean)
dates=casesbyregion$date
casesbyregion$date<-NULL

drawingzoo=zoo(casesbyregion, order.by=as.Date(dates))

drawingzoo=rollmean(drawingzoo, 7, align=c("right"))

areas=colnames(casesbyregion)
jColors <- c('chartreuse2', 'cornflowerblue', 'darkgoldenrod1', 'peachpuff3',
             'mediumorchid2', 'turquoise3', 'wheat4', 'slategray2', 'red')



plot.zoo(drawingzoo, plot.type="single", log="y", col=jColors, lwd=2,  main="7-day Average of new cases - English Regions", ylab="Cases - Log scale", xlab="Date")
grid(equilogs=F)
legend("topright", areas, col=jColors, lwd=2, lty=c(1, 1, 1, 1, 1, 1, 1), cex=.8, bg="white")

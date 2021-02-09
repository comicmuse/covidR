library(curl)
library(zoo)
library(tidyr)
library(dplyr)
library(ggplot2)


cases<-read.csv(curl("https://coronavirus.data.gov.uk/api/v1/data?filters=areaType=region;date>2020-12-01&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22newCasesByPublishDate%22:%22newCasesByPublishDate%22,%22cumCasesByPublishDate%22:%22cumCasesByPublishDate%22%7D&format=csv"), header=TRUE, stringsAsFactors=FALSE)

casesbyregion<- cases %>% select (-areaCode, -areaType, -cumCasesByPublishDate) %>% pivot_wider(names_from = areaName, values_from=newCasesByPublishDate, values_fn=mean)
dates=casesbyregion$date
casesbyregion$date<-NULL

drawingzoo=zoo(casesbyregion, order.by=as.Date(dates))

drawingzoo=rollmean(drawingzoo, 7, align=c("right"))

autoplot.zoo(drawingzoo, facets=NULL)+geom_smooth(se=F) + scale_y_continuous(trans='log10') + 
labs(title="English Regions 7-day Ave New Cases", x="Date", y="Cases (log scale)") 



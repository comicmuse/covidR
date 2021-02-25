library(curl)
library(zoo)
library(tidyr)
library(dplyr)
library(ggplot2)

cases<-read.csv(curl("https://coronavirus.data.gov.uk/api/v1/data?filters=areaType=region;date>2021-01-18&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22newCasesByPublishDate%22:%22newCasesByPublishDate%22,%22cumCasesByPublishDate%22:%22cumCasesByPublishDate%22%7D&format=csv"), header=TRUE, stringsAsFactors=FALSE)
nations<-read.csv(curl("https://coronavirus.data.gov.uk/api/v1/data?filters=areaType=nation;date>2021-01-18&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22newCasesByPublishDate%22:%22newCasesByPublishDate%22,%22cumCasesByPublishDate%22:%22cumCasesByPublishDate%22%7D&format=csv"), header=TRUE, stringsAsFactors=FALSE)

casesbyregion<- cases %>% select (-areaCode, -areaType, -cumCasesByPublishDate) %>% pivot_wider(names_from = areaName, values_from=newCasesByPublishDate, values_fn=mean)
casesbynation<- nations %>% select (-areaCode, -areaType, -cumCasesByPublishDate) %>% pivot_wider(names_from = areaName, values_from=newCasesByPublishDate, values_fn=mean)

casesbyregion <- casesbyregion %>%
inner_join (casesbynation)

dates=casesbyregion$date
casesbyregion$date<-NULL

drawingzoo=zoo(casesbyregion, order.by=as.Date(dates))

drawingzoo=rollmean(drawingzoo, 7, align=c("right"))

#drawingzoo$"East Midlands" <- drawingzoo$"East Midlands" / 4.8
#drawingzoo$"East of England" <- drawingzoo$"East of England" / 6.2
#drawingzoo$"London" <- drawingzoo$"London" / 8.8
#drawingzoo$"North East" <- drawingzoo$"North East" / 2.6
#drawingzoo$"North West" <- drawingzoo$"North West" / 7.3
#drawingzoo$"South East" <- drawingzoo$"South East" / 9.1
#drawingzoo$"South West" <- drawingzoo$"South West" / 5.6
#drawingzoo$"West Midlands" <- drawingzoo$"West Midlands" / 5.9
#drawingzoo$"Yorkshire and The Humber" <- drawingzoo$"Yorkshire and The Humber" / 5.4
#drawingzoo$Scotland<- drawingzoo$Scotland /5.45
#drawingzoo$Wales<- drawingzoo$Wales /3.13
#drawingzoo$"Northern Ireland"<- drawingzoo$"Northern Ireland"/1.885
drawingzoo$England <-NULL


drawingzoo$"South East"<-NULL
drawingzoo$"South West"<-NULL


autoplot.zoo(drawingzoo, facets=NULL)+geom_smooth(method="lm", se=F) + scale_y_continuous(trans='log10') + 
labs(title="English Regions 7-day Ave New Cases", x="Date", y="Cases(log scale)")




# "East Midlands" 4.8
# "East of England" 6.2
# "London" 8.8
# "North East" 2.6
# "North West" 7.3
# "South East" 9.1
# "South West" 5.6 
# "West Midlands" 5.9
# "Yorkshire and The Humber" 5.4
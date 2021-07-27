library(curl)
library(zoo)
library(tidyr)
library(dplyr)
library(ggplot2)
library(ragg)

cases<-read.csv(curl("https://coronavirus.data.gov.uk/api/v1/data?filters=areaType=region;date>2021-01-01&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22newCasesByPublishDate%22:%22newCasesByPublishDate%22,%22cumCasesByPublishDate%22:%22cumCasesByPublishDate%22%7D&format=csv"), header=TRUE, stringsAsFactors=FALSE)
nations<-read.csv(curl("https://coronavirus.data.gov.uk/api/v1/data?filters=areaType=nation;date>2021-01-01&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22newCasesByPublishDate%22:%22newCasesByPublishDate%22,%22cumCasesByPublishDate%22:%22cumCasesByPublishDate%22%7D&format=csv"), header=TRUE, stringsAsFactors=FALSE)

casesbyregion<- cases %>% select (-areaCode, -areaType, -cumCasesByPublishDate) %>% pivot_wider(names_from = areaName, values_from=newCasesByPublishDate, values_fn=mean)
casesbynation<- nations %>% select (-areaCode, -areaType, -cumCasesByPublishDate) %>% pivot_wider(names_from = areaName, values_from=newCasesByPublishDate, values_fn=mean)

casesbyregion <- casesbyregion %>%
inner_join (casesbynation)

dates=casesbyregion$date
casesbyregion$date<-NULL

drawingzoo=zoo(casesbyregion, order.by=as.Date(dates))

drawingzoo=rollmean(drawingzoo, 7, align=c("right"))

drawingzoo$"East Midlands" <- drawingzoo$"East Midlands" / 4.8
drawingzoo$"East of England" <- drawingzoo$"East of England" / 6.2
drawingzoo$"London" <- drawingzoo$"London" / 8.8
drawingzoo$"North East" <- drawingzoo$"North East" / 2.6
drawingzoo$"North West" <- drawingzoo$"North West" / 7.3
drawingzoo$"South East" <- drawingzoo$"South East" / 9.1
drawingzoo$"South West" <- drawingzoo$"South West" / 5.6
drawingzoo$"West Midlands" <- drawingzoo$"West Midlands" / 5.9
drawingzoo$"Yorkshire and The Humber" <- drawingzoo$"Yorkshire and The Humber" / 5.4
drawingzoo$Scotland<- drawingzoo$Scotland /5.45
drawingzoo$Wales<- drawingzoo$Wales /3.13
drawingzoo$"Northern Ireland"<- drawingzoo$"Northern Ireland"/1.885
drawingzoo$England <-NULL

#agg_tiff("Outputs/CaseTrendRegions.tiff", units="in", width=8, height=7, res=800)
#autoplot.zoo(drawingzoo, facets=NULL)  + scale_y_continuous(trans='log10') + #geom_smooth(method="lm", se=F) + 
#labs(title="Nations and English Regions 7-day Ave New Cases/million", x="Date", y="Cases/million (log scale)")
#dev.off()



# "East Midlands" 4.8
# "East of England" 6.2
# "London" 8.8
# "North East" 2.6
# "North West" 7.3
# "South East" 9.1
# "South West" 5.6 
# "West Midlands" 5.9
# "Yorkshire and The Humber" 5.4




southzoo=merge(London=drawingzoo$London, "South East"=drawingzoo$"South East", "South West"=drawingzoo$"South West", "East of England"=drawingzoo$"East of England", Wales=drawingzoo$Wales)
northzoo=merge("North East"=drawingzoo$"North East", "North West"=drawingzoo$"North West", "East Midlands"=drawingzoo$"East Midlands", "West Midlands"=drawingzoo$"West Midlands", "Yorkshire and The Humber"=drawingzoo$"Yorkshire and The Humber")
celticnorthzoo=merge(Scotland=drawingzoo$Scotland, "Northern Ireland"=drawingzoo$"Northern Ireland")

agg_png("Outputs/CaseTrendNorth.png", units="in", width=8, height=7, res=800)
autoplot.zoo(northzoo, facets=NULL) + scale_y_continuous(trans='log10', limits=c(10,2500) ) + 
labs(title="The North", x="Date", y="Cases/million (log scaldev.newe)") + theme(legend.position="bottom")
#+
# geom_smooth(method="lm", se=F, lwy=1) 
dev.off()


agg_png("Outputs/CaseTrendSouth.png", units="in", width=8, height=7, res=800)
autoplot.zoo(southzoo, facets=NULL) + scale_y_continuous(trans='log10', limits=c(10,2500)) + 
labs(title="The South", x="Date", y="Cases/million (log scale)")+ theme(legend.position="bottom")
#+
# geom_smooth(method="lm", se=F, lwy=1) 
dev.off()


agg_png("Outputs/CaseTrendCelticNorth.png", units="in", width=8, height=7, res=800)
autoplot.zoo(celticnorthzoo, facets=NULL) + scale_y_continuous(trans='log10', limits=c(10,2500)) + 
labs(title="Scotland/NI", x="Date", y="Cases/million (log scale)")+ theme(legend.position="bottom")
#+ geom_smooth(method="lm", se=F, lwy=1) 
dev.off()


library (curl)
library (zoo)

#Get the data
data <- read.csv(curl("https://coronavirus.data.gov.uk/api/v1/data?filters=areaType=overview&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22newPeopleVaccinatedFirstDoseByPublishDate%22:%22newPeopleVaccinatedFirstDoseByPublishDate%22,%22newPeopleVaccinatedSecondDoseByPublishDate%22:%22newPeopleVaccinatedSecondDoseByPublishDate%22,%22cumPeopleVaccinatedFirstDoseByPublishDate%22:%22cumPeopleVaccinatedFirstDoseByPublishDate%22,%22cumPeopleVaccinatedSecondDoseByPublishDate%22:%22cumPeopleVaccinatedSecondDoseByPublishDate%22%7D&format=csv"), header=TRUE, stringsAsFactors=FALSE)
data$date=as.Date(data$date)


#Empty Time
timeframe=zoo(seq(0,0, length=180), order.by=seq(as.Date("2020-12-20"), as.Date("2021-05-31"), by=1))


#Extract the daily new vaccinated people
cumfirst=zoo(x=data$cumPeopleVaccinatedFirstDoseByPublishDate, order.by=data$date)
cumsecond=zoo(x=data$cumPeopleVaccinatedSecondDoseByPublishDate, order.by=data$date)
#Put in the legacy numbers
cumfirst[seq(as.Date("2020-12-20"), as.Date("2021-01-10"),1)]<-c(seq(669674,669674,length=7), seq(996616,996616,length=7), seq(1367847, 1367847, length=7), 2286572)
cumsecond[seq(as.Date("2020-12-20"), as.Date("2021-01-10"),1)]<-c(seq(0,0,length=7), seq(0,0,length=7), seq(21808,21808, length=7), 413750)

#Merge all the timeseries into one zoo
finalzoo=merge.zoo(timeframe, cumfirst, cumsecond, all=T, drop=F)
finalzoo$timeframe<-NULL

finalzoo$threeweektarget=lag(finalzoo$cumfirst, k=-21, na.pad=TRUE)
finalzoo$twelveweektarget=lag(finalzoo$cumfirst, k=-84, na.pad=TRUE)

#Draw the graph
plot.zoo(finalzoo/1000000, plot.type="single",  col=c("blue","green","orange","red"), lwd=2, lty=c(1, 1, 4,4), xlab="Date",ylab="People (millions)", main="Vaccine Completion")

legend(as.numeric(as.Date("2020-12-23")), 9, legend=c("First Doses", "Second Doses", "3-Week Target", "12-Week Target"), col=c(4,3,8,0), lty=c(1,1,5), bg="white")

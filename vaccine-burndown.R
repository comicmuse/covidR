library (curl)
library (zoo)

#Get the data
data <- read.csv(curl("https://coronavirus.data.gov.uk/api/v1/data?filters=areaType=overview&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22newPeopleVaccinatedFirstDoseByPublishDate%22:%22newPeopleVaccinatedFirstDoseByPublishDate%22,%22newPeopleVaccinatedSecondDoseByPublishDate%22:%22newPeopleVaccinatedSecondDoseByPublishDate%22,%22cumPeopleVaccinatedFirstDoseByPublishDate%22:%22cumPeopleVaccinatedFirstDoseByPublishDate%22,%22cumPeopleVaccinatedSecondDoseByPublishDate%22:%22cumPeopleVaccinatedSecondDoseByPublishDate%22%7D&format=csv"), header=TRUE, stringsAsFactors=FALSE)
data$date=as.Date(data$date)

#Legacy data
legacy=zoo(c(seq(14366892,14366892,length=7), seq(14045222,14045222,length=7), seq(13686549, 13686549, length=7), 12713428), order.by=c(seq(as.Date("2020-12-20"), as.Date("2021-01-10"),1)))


#Extract the daily new vaccinated people
newfirst=zoo(x=data$newPeopleVaccinatedFirstDoseByPublishDate, order.by=data$date)
currentrate=mean( tail ( newfirst, 7))

#Extract the cumulative vaccinated and work out the remainder
cumfirst=zoo(x=data$cumPeopleVaccinatedFirstDoseByPublishDate, order.by=data$date)
peopleleft<-15000000-cumfirst

#Original Target
#td=seq(as.Date("2020-12-20"), as.Date("2021-02-15"), "days")
#target=zoo( seq(15000000,0,length=length(td)), order.by=td)

#Target based off Govt Announcement
td=seq(as.Date("2021-01-04"), as.Date("2021-02-15"), "days")
target=zoo( seq(13686549, 0,length=length(td)), order.by=td)

#Calculate burndown
burndown=seq(tail(coredata(peopleleft), 1), 0, by=-currentrate)
burndownzoo=zoo(burndown, order.by=seq(end(peopleleft), by=1, length=length(burndown)))

#Required Rate
reqrate=round(tail(coredata(peopleleft),1) / as.numeric(as.Date("2021-02-15") - end(peopleleft)))

#Merge all the timeseries into one zoo
finalzoo=merge.zoo(target, peopleleft, burndownzoo, legacy, all=T, drop=F)

#Draw the graph
plot.zoo(finalzoo/1000000, plot.type="single", col=c(4,3,8,5), lty=c(1,1,5), lwd=2, xlab="Date",ylab="People (millions)", main="Vaccine Burndown (First Dose)")
grid (NA,NULL, lty = 6, col = "cornsilk2") 
text (as.numeric(as.Date("2020-12-25")), 4, paste("Current Rate:   ",currentrate), adj=0, bg=0)
text (as.numeric(as.Date("2020-12-25")), 3.3, paste("Required Rate:",reqrate), adj=0, bg=0)
text (as.numeric(as.Date("2020-12-25")), 2.6, paste("Intercept:  ",end(burndownzoo)), adj=0, bg=0)
legend(as.numeric(as.Date("2021-02-02")), 14, legend=c("Target", "Remaining", "Forecast"), col=c(4,3,8,5), lty=c(1,1,5), bg="white")
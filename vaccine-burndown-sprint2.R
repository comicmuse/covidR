library (curl)
library (zoo)

#Get the data
data <- read.csv(curl("https://coronavirus.data.gov.uk/api/v1/data?filters=areaType=overview&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22newPeopleVaccinatedFirstDoseByPublishDate%22:%22newPeopleVaccinatedFirstDoseByPublishDate%22,%22newPeopleVaccinatedSecondDoseByPublishDate%22:%22newPeopleVaccinatedSecondDoseByPublishDate%22,%22cumPeopleVaccinatedFirstDoseByPublishDate%22:%22cumPeopleVaccinatedFirstDoseByPublishDate%22,%22cumPeopleVaccinatedSecondDoseByPublishDate%22:%22cumPeopleVaccinatedSecondDoseByPublishDate%22%7D&format=csv"), header=TRUE, stringsAsFactors=FALSE)
data$date=as.Date(data$date)

#Legacy data
legacy=zoo(c(seq(14366892,14366892,length=7), seq(14045222,14045222,length=7), seq(13686549, 13686549, length=7), 12713428), order.by=c(seq(as.Date("2020-12-20"), as.Date("2021-01-10"),1)))

legacy <- legacy+17000000

#Extract the daily new vaccinated people
newfirst=zoo(x=data$newPeopleVaccinatedFirstDoseByPublishDate, order.by=data$date)
currentrate=round(mean( tail ( newfirst, 7)))
yesterdayrate=tail(newfirst, 1)

#Extract the cumulative vaccinated and work out the remainder
cumfirst=zoo(x=data$cumPeopleVaccinatedFirstDoseByPublishDate, order.by=data$date)
peopleleft<-32000000-cumfirst

#Original Target
#td=seq(as.Date("2020-12-20"), as.Date("2021-02-15"), "days")
#target=zoo( seq(32000000,17000000,length=length(td)), order.by=td)

#Target based off Govt Announcement
td=seq(as.Date("2021-01-04"), as.Date("2021-02-15"), "days")
target=zoo( seq(13686549+17000000, 17000000,length=length(td)), order.by=td)
td2=seq(as.Date("2021-02-15"), as.Date("2021-04-30"), "days")

target2=zoo( seq(17000000, 0,length=length(td2)), order.by=td2)


#Calculate burndown
burndown=seq(tail(coredata(peopleleft), 1), 0, by=-currentrate)
burndownzoo=zoo(burndown, order.by=seq(end(peopleleft), by=1, length=length(burndown)))

#Required Rate
reqrate=round(tail(coredata(peopleleft),1) / as.numeric(as.Date("2021-04-30") - end(peopleleft)))

#Merge all the timeseries into one zoo
finalzoo=merge.zoo(target, target2, peopleleft, burndownzoo, legacy, all=T, drop=F)

#Draw the graph
plot.zoo(finalzoo/1000000, plot.type="single", col=c(4,4,3,8,5), lty=c(1,1, 1,5), lwd=2, xlab="Date",ylab="People (millions)", main="Vaccine Burndown (First Dose)")
#grid (NA,NULL, lty = 6, col = "cornsilk2") 
text (as.numeric(as.Date("2020-12-25")), 6.5, paste("Current Rate:   ",currentrate), adj=0, bg=0)
text (as.numeric(as.Date("2020-12-25")), 5, paste("Required Rate:",reqrate), adj=0, bg=0)
text (as.numeric(as.Date("2020-12-25")), 3.5, paste("Yesterday: ",yesterdayrate), adj=0, bg=0)
text (as.numeric(as.Date("2020-12-25")), 2, paste("Intercept:  ",end(burndownzoo)+1), adj=0, bg=0)
legend(as.numeric(as.Date("2021-03-20")), 27, legend=c("Target", "Remaining", "Projected"), col=c(4,3,8,5), lty=c(1,1,5), bg="white")

tail (newfirst, 1)
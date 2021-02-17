


library (curl)
library (zoo)
library(ggplot2)

admissions<- read.csv(curl("https://coronavirus.data.gov.uk/api/v1/data?filters=areaType=overview&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22newAdmissions%22:%22newAdmissions%22,%22cumAdmissions%22:%22cumAdmissions%22%7D&format=csv"), header=TRUE, stringsAsFactors=FALSE)

admissionsbydate<- zoo(admissions$newAdmissions, order.by=as.Date(admissions$date))

aveadmissions<-rollmean(admissionsbydate, 7,align="right")

alladmissions<- aveadmissions[index(aveadmissions) >=as.Date("2021-01-18")]
alladmissions[seq(end(alladmissions)+1, as.Date("2021-05-01"), by=1)]=NA

model<-lm(log(alladmissions) ~ as.numeric(time(alladmissions)))

predictline=zoo(exp(predict(model, list(as.numeric(time(alladmissions))))), order.by=time(alladmissions))

finalmerge=merge(alladmissions, predictline)

ggplot( mapping=aes(x = time(alladmissions), y = coredata(alladmissions)) ) + 
scale_y_log10() + geom_point(col="red",shape=1, fill="white", size=3) + geom_smooth(method="lm", fullrange=T)



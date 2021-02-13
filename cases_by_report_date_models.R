library (curl)
library (zoo)


cases<- read.csv(curl("https://coronavirus.data.gov.uk/api/v1/data?filters=areaName=United%2520Kingdom;areaType=overview&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22newCasesByPublishDate%22:%22newCasesByPublishDate%22,%22cumCasesByPublishDate%22:%22cumCasesByPublishDate%22%7D&format=csv"), header=TRUE, stringsAsFactors=FALSE)

casesbydate<- zoo(cases$newCasesByPublishDate, order.by=as.Date(cases$date))

avecases<-rollmean(casesbydate, 7,align="right")

allsince11<- avecases[index(avecases) >=as.Date("2021-01-18")]

model<-lm(log(allsince11) ~ as.numeric(time(allsince11)))


predictline=zoo(exp(predict(model, list(as.numeric(time(allsince11))))), order.by=time(allsince11))

finalmerge=merge(allsince11, predictline)

plot(finalmerge, plot.type="single", log="y", lty=c(1,1), type=c("b","l"), col=c("blue","red"), main=paste("7-day Ave Reported date - ", end ( allsince11)), xlab="Date", ylab="7-day Ave New Cases")

legend("topright", paste("R-Squared: ", round(summary(model)$r.squared, 4)))
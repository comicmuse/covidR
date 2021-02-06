library (curl)
library (zoo)


cases<- read.csv(curl("https://coronavirus.data.gov.uk/api/v1/data?filters=areaName=United%2520Kingdom;areaType=overview&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22newCasesByPublishDate%22:%22newCasesByPublishDate%22,%22cumCasesByPublishDate%22:%22cumCasesByPublishDate%22%7D&format=csv"), header=TRUE, stringsAsFactors=FALSE)

casesbydate<- zoo(cases$newCasesByPublishDate, order.by=as.Date(cases$date))

avecases<-rollmean(casesbydate, 7,align="right")

elbow=14


allsince11<- avecases[index(avecases) >=as.Date("2021-01-11")]

firsttrend<-head(allsince11, elbow)

recent<-tail(allsince11, length(allsince11)-elbow)


model0<-lm(log(allsince11) ~ as.numeric(time(allsince11)))


model1<-lm(log(firsttrend) ~ as.numeric(time(firsttrend)))

model2<-lm(log(recent) ~ as.numeric(time(recent)))


predictline0=zoo(exp(predict(model0, list(as.numeric(time(allsince11))))), order.by=time(allsince11))

predictline1=zoo(exp(predict(model1, list(as.numeric(time(firsttrend))))), order.by=time(firsttrend))

predictline2=zoo(exp(predict(model2, list(as.numeric(time(recent))))), order.by=time(recent))

finalmerge=merge(firsttrend, recent)
finalmerge=merge(finalmerge, predictline1)
finalmerge=merge(finalmerge, predictline2)
finalmerge=merge(finalmerge, predictline0)



plot(finalmerge, plot.type="single", log="y", lty=c(1,1), type=c("b","b","l","l", "l"), col=c("blue","blue","red", "red", "orange"), main="7-day Ave Reported date", xlab="Date", ylab="7-day Ave New Cases")



tail ( recent, 1)

summary(model0)$r.squared
summary(model1)$r.squared
summary(model2)$r.squared
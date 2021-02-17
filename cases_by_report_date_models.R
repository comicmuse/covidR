library (curl)
library (zoo)
library(ggplot2)

cases<- read.csv(curl("https://coronavirus.data.gov.uk/api/v1/data?filters=areaName=United%2520Kingdom;areaType=overview&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22newCasesByPublishDate%22:%22newCasesByPublishDate%22,%22cumCasesByPublishDate%22:%22cumCasesByPublishDate%22%7D&format=csv"), header=TRUE, stringsAsFactors=FALSE)

casesbydate<- zoo(cases$newCasesByPublishDate, order.by=as.Date(cases$date))

avecases<-rollmean(casesbydate, 7,align="right")

allcases<- avecases[index(avecases) >=as.Date("2021-01-18")]
allcases[seq(end(allcases)+1, as.Date("2021-05-01"), by=1)]=NA

model<-lm(log(allcases) ~ as.numeric(time(allcases)))

predictline=zoo(exp(predict(model, list(as.numeric(time(allcases))))), order.by=time(allcases))

finalmerge=merge(allcases, predictline)

ggplot( mapping=aes(x = time(allcases), y = coredata(allcases)) ) + 
scale_y_log10() + geom_point(col="red",shape=1, fill="white", size=3) + geom_smooth(method="lm", fullrange=T)


#plot(finalmerge, plot.type="single", log="y", lty=c(1,1), type=c("b","l"), col=c("blue","red"), main=paste("7-day Ave Reported date - ", end ( allcases)), xlab="Date", ylab="7-day Ave New Cases")
#grid
#legend("topright", paste("R-Squared: ", round(summary(model)$r.squared, 4)))
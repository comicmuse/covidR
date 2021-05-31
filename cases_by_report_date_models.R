library (curl)
library (zoo)
library(ggplot2)

cases<- read.csv(curl("https://coronavirus.data.gov.uk/api/v1/data?filters=areaName=United%2520Kingdom;areaType=overview&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22newCasesByPublishDate%22:%22newCasesByPublishDate%22,%22cumCasesByPublishDate%22:%22cumCasesByPublishDate%22%7D&format=csv"), header=TRUE, stringsAsFactors=FALSE)

casesbydate<- zoo(cases$newCasesByPublishDate, order.by=as.Date(cases$date))

avecases<-rollmean(casesbydate, 7,align="right")

allcases<- avecases[index(avecases) >=as.Date("2021-02-01")]

trend_start_date=as.Date("2021-05-20")

model<-lm(log(allcases[index(allcases) >= trend_start_date]) ~ as.numeric(time(allcases[index(allcases) >= trend_start_date])))

model_slope=round(100 * (1-exp(predict(model))[1]/exp(predict(model))[2]),2)


ggplot( mapping=aes(x = time(allcases), y = coredata(allcases)) ) + 
scale_y_log10() + geom_point(col="red",shape=1, fill="white", size=3) + 
#geom_line() +
geom_smooth(method="lm", mapping=aes(x = time(allcases[index(allcases) >= trend_start_date]), y = coredata(allcases[index(allcases) >=trend_start_date])) , level=.99)+
labs(x="date", y="cases (log scale)", title="7-day average of New UK Cases")+
annotate("text",label = paste("R-Squared: ",round(summary(model)$r.squared*100, 3), "%"), y=10000, x=as.Date("2021-05-01"))+
annotate("text",label = paste("Trend Fitted from: ",trend_start_date), y=12000, x=as.Date("2021-05-01"))+
annotate("text",label = paste("Daily growth: ",model_slope, "%"), y=11000, x=as.Date("2021-05-01"))



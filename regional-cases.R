library(curl)
library(zoo)
library(tidyr)


cases<-read.csv(curl("https://coronavirus.data.gov.uk/api/v1/data?filters=areaType=region;date>2021-01-01&structure=%7B%22areaType%22:%22areaType%22,%22areaName%22:%22areaName%22,%22areaCode%22:%22areaCode%22,%22date%22:%22date%22,%22newCasesByPublishDate%22:%22newCasesByPublishDate%22,%22cumCasesByPublishDate%22:%22cumCasesByPublishDate%22%7D&format=csv"), header=TRUE, stringsAsFactors=FALSE)


cases %>% pivot_wider()

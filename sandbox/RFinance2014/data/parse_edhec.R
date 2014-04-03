library(xts)

df <- read.table("data/edhec.csv", header=TRUE, as.is=TRUE, sep=";")
head(df)

edhec <- xts(apply(df[,-1], 2, function(x) as.numeric(gsub("%", "", x))) / 100, as.Date(df[,1], format="%d/%m/%Y"))
save(edhec, file="edhec.rda")
# load("edhec.rda")
# head(edhec)
# library(PerformanceAnalytics)
# data(edhec)

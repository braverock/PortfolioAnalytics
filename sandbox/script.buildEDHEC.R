### Construct an xts object of EDHEC composite hedge fund style indexes

# Peter Carl

# Used for updating the edhec data object in PerformanceAnalytics

require(gdata)
require(xts)

# Download the following file to the working directory:
# http://www.edhec-risk.com/indexes/pure_style/data/table/history.csv
### @TODO: Is there a way to download it directly? Maybe not, seems to require a login
x=read.csv(file="history.csv", sep=";", header=TRUE, check.names=FALSE)
x.dates = as.Date(x[,1], format="%d/%m/%Y")
x.data = apply(x[,-1], MARGIN=2, FUN=function(x){as.numeric(sub("%","", x, fixed=TRUE))/100}) # get rid of percentage signs
edhec = xts(x.data, order.by=x.dates)
colnames(edhec)
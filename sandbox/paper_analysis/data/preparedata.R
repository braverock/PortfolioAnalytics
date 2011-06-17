


setwd("c:/Documents and Settings/Administrator/Desktop/risk budget programs")

library(zoo); 

#   Load the data

start = as.Date("1991-01-01") ; end = as.Date("2009-12-31") ;

# Risky assets:

data = as.data.frame(read.csv( file = "data/MSCI_Europe.csv",skip=6,sep=","))
colnames(data) = c("Date","Europe")
#data$Europe[(as.character(data$Europe)=="#NA")]=NA; 
MSCI_Europe = zoo( data$Europe , order.by = as.Date( as.character(data$Date) , format = "%m/%d/%Y" ) )
MSCI_Europe = window( MSCI_Europe , start = start , end = end );

data = as.data.frame(read.csv( file = "data/MSCI_Japan.csv",skip=6,sep=","))
colnames(data) = c("Date","Japan")
#data$Japan[(as.character(data$Japan)=="#NA")]=NA; 
MSCI_Japan = zoo( data$Japan , order.by = as.Date( as.character(data$Date) , format = "%m/%d/%Y" ) )
MSCI_Japan = window( MSCI_Japan , start = start , end = end );

data = as.data.frame(read.csv( file = "data/MSCI_US.csv",skip=6,sep=","))
colnames(data) = c("Date","US")
#data$US[(as.character(data$US)=="#NA")]=NA; 
MSCI_US = zoo( data$US , order.by = as.Date( as.character(data$Date) , format = "%m/%d/%Y" ) )
MSCI_US = window( MSCI_US , start = start , end = end );

#data = as.data.frame(read.csv( file = "data/barclaysglobalbond.csv",skip=6,sep=","))
#data = as.data.frame(read.csv( file = "data/usgovbond.csv",skip=6,sep=","))
data = as.data.frame(read.csv( file = "data/MLDOMMASTER.csv",skip=6,sep=","))
colnames(data) = c("Date","bond")
#data$bond[(as.character(data$bond)=="#NA")] = NA; 
bond = zoo( as.numeric(data$bond) , order.by = as.Date( as.character(data$Date) , format = "%m/%d/%Y" ) )
bond = window( bond , start = start , end = end );

data = as.data.frame(read.csv( file = "data/SPGSCI.csv",skip=6,sep=","))
colnames(data) = c("Date","SPGSCI")
#data$SPGSCI[(as.character(data$SPGSCI)=="#NA")]=NA; 
SPGSCI = zoo( data$SPGSCI , order.by = as.Date( as.character(data$Date) , format = "%m/%d/%Y" ) )
SPGSCI = window( SPGSCI , start = start , end = end );

data = as.data.frame(read.csv( file = "data/SP500.csv",skip=6,sep=","))
colnames(data) = c("Date","SP500")
#data$SP500[(as.character(data$SP500)=="#NA")]=NA; 
SP500 = zoo( data$SP500 , order.by = as.Date( as.character(data$Date) , format = "%m/%d/%Y" ) )
SP500 = window( SP500 , start = start , end = end );

data = as.data.frame(read.csv( file = "data/MSCI_EAFEbis.csv",skip=0,sep=","))
colnames(data) = c("Date","EAFE")
MSCI_EAFEbis = zoo( data$EAFE , order.by = as.Date( as.character(data$Date) , format = "%Y%m%d" ) )
MSCI_EAFEbis = window( MSCI_EAFEbis , start = start , end = end );

# Data Brian

data = as.data.frame(read.csv( file = "data/SPGSCIbis.csv",skip=6,sep=","))
colnames(data) = c("Date","SPGSCI")
SPGSCIbis = zoo( data$SPGSCI , order.by = as.Date( as.character(data$Date) , format = "%Y%m%d" ) )
SPGSCIbis = window( SPGSCIbis , start = start , end = end );

plot(SPGSCIbis)

# Middle interest rate on secondary market 3-month treasury bill:

data = as.data.frame(read.csv( file = "data/TBill.csv",skip=6,sep=","))
colnames(data) = c("Date","TBill")
#data$TBill[(as.character(data$TBill)=="#NA")]=NA; 
# The rates listed on Treasury bills are annualized discount rates, assuming a year has 360 days.
# T-bill discount rate = [face value-bill price]*(360/number of days until maturity)
# A three month T-Bill has a maturity of 91 days
# The bond equivalent yield associated to the T-Bill is
# T-bill yield = [ (face value - bill price)/bill price ]x(365/number of days until maturity)
# The annualized yield is then (take face value of 1000):
# bill price = 100 - (91/360)*rate
# yield = {[(91/360)*rate]/[100 - (91/360)*rate]}*(365/91)
# See e.g. Mizrach, Bruce and Neely, Christopher J.,The Microstructure of the U.S. Treasury Market(April 2008). FRB St. Louis Working Paper No. 2007-052B. Available at SSRN: http://ssrn.com/abstract=1070226

# Bond-equivalent annualized yield 
data$TBill = (91/360)*(365/91)*data$TBill/(100-data$TBill*(91/360) ) ; 
TBill = zoo( data$TBill , order.by = as.Date( as.character(data$Date) , format = "%m/%d/%Y" ) )
TBill = window( TBill , start = start , end = end );

# International equity + bond portfolio

data = merge( bond , MSCI_Europe , MSCI_Japan , MSCI_US , TBill)
colnames(data) = c( "Bond" , "Europe", "Japan" , "US" , "TBill")
data = na.locf(data);
head(data); plot(data);
write.zoo( data , file="data/intequitybonds/data.txt" , row.names=F )
save( data , file = paste(  getwd(),"/data/intequitybonds/intequitybonds.Rdata" , sep="")   )

# Mixed portfolio 

data = merge.zoo( bond , SP500 , MSCI_EAFEbis , SPGSCIbis , TBill)
colnames(data) = c( "Bond" , "SP500", "EAFE" , "SPGSCI" , "TBill")
data = na.locf(data);
head(data); plot(data);
write.zoo( data , file="data/equitybondscommodity/data.txt" , row.names=F )
save( data , file = paste(  getwd(),"/data/equitybondscommodity/equitybondscommodity.Rdata" , sep="")   )






setwd("c:/Documents and Settings/Administrator/Desktop/risk budget programs")

library(zoo); 

# Load the data
start = as.Date("1975-12-31") ; end = as.Date("2010-06-30") ;
dates = seq.Date(as.Date(start) + 1, as.Date(end) + 1, by ="month") - 1


# S&P GSCI

data = as.data.frame(read.csv( file = "data/SPGSCIall.csv",skip=5,sep=","))
colnames(data) = c("Date","SPGSCI")
SPGSCI = zoo( data$SPGSCI , order.by = as.Date( as.character(data$Date) , format = "%m/%d/%Y" ) )
SPGSCI = window( SPGSCI , start = start , end = end );
monthlySPGSCI = aggregate(SPGSCI, as.yearmon , tail , 1 );
monthlyR_SPGSCI = diff(monthlySPGSCI)/as.vector(lag(monthlySPGSCI))
monthlyR_SPGSCI  = zoo( as.vector(monthlyR_SPGSCI)  , order.by = dates[2:length(dates)])

# S&P 500 total returns

data = as.data.frame(read.csv( file = "data/SP500all.csv",skip=5,sep=","))
colnames(data) = c("Date","SP500")
SP500 = zoo( data$SP500 , order.by = as.Date( as.character(data$Date) , format = "%m/%d/%Y" ) )
SP500= window( SP500 , start = start , end = end );
monthlySP500 = aggregate(SP500, as.yearmon , tail , 1 );
monthlyR_SP500 = diff(monthlySP500)/as.vector(lag(monthlySP500))

     # we still need the returns on the total return index for the period Jan 1976-Jan 1988

data = as.data.frame(read.csv( file = "data/SP500returns.csv",skip=5,sep=","))
colnames(data) = c("Date","SP500")
SP500 = zoo( data$SP500 , order.by = as.Date( as.character(data$Date) , format = "%m/%d/%Y" ) )
SP500= window( SP500 , start = start , end = "1988-01-31" );
monthlyRSP500bis = aggregate(SP500, as.yearmon , tail , 1 );
Z = merge( monthlyRSP500bis , monthlyR_SP500    )
monthlyR_SP500 = replace(Z$monthlyRSP500bis, is.na( Z$monthlyRSP500bis) , 0) +  replace( Z$monthlyR_SP500, is.na( Z$monthlyR_SP500) , 0)
monthlyR_SP500  = zoo( as.vector(monthlyR_SP500)  , order.by = dates[2:length(dates)])

# MSCI EAFE 

data = as.data.frame(read.csv( file = "data/EAFEall.csv",skip=5,sep=","))
colnames(data) = c("Date","EAFE")
EAFE = zoo( data$EAFE , order.by = as.Date( as.character(data$Date) , format = "%m/%d/%Y" ) )
EAFE = window( EAFE , start = start , end = end );
monthlyEAFE= aggregate(EAFE, as.yearmon , tail , 1 );
monthlyR_EAFE = diff(monthlyEAFE)/as.vector(lag(monthlyEAFE))
monthlyR_EAFE  = zoo( as.vector(monthlyR_EAFE)  , order.by = dates[2:length(dates)])


# NAREIT 

data = as.data.frame(read.csv( file = "data/nareit.csv",skip=0,sep=","))
colnames(data) = c("Date","NAREIT")
monthlyNAREIT = zoo( as.numeric(data$NAREIT) , order.by = as.Date( as.character(data$Date) , format = "%m/%d/%Y" ) )
monthlyNAREIT = window( monthlyNAREIT , start = start , end = end );
monthlyR_NAREIT = diff(monthlyNAREIT)/as.vector(lag(monthlyNAREIT))
monthlyR_NAREIT  = zoo( as.vector(monthlyR_NAREIT)  , order.by = dates[2:length(dates)])

# Bond

data = as.data.frame(read.csv( file = "data/MLDOMMASTERall.csv",skip=5,sep=","))
colnames(data) = c("Date","bond")
bond = zoo( data$bond , order.by = as.Date( as.character(data$Date) , format = "%m/%d/%Y" ) )
bond = window( bond , start = start , end = end );
monthlybond= aggregate(bond, as.yearmon , tail , 1 );
monthlyR_bond = diff(monthlybond)/as.vector(lag(monthlybond))
monthlyR_bond  = zoo( as.vector(monthlyR_bond)  , order.by = dates[2:length(dates)])

# Inflation

data = as.data.frame(read.csv( file = "data/CPI.csv",skip=5,sep=","))
colnames(data) = c("Date","CPI")
CPI = zoo( data$CPI , order.by = as.Date( as.character(data$Date) , format = "%m/%d/%Y" ) )
CPI = window( CPI , start = start , end = end );
monthlyCPI = aggregate(CPI, as.yearmon , tail , 1 );
monthlyInflation = diff(monthlyCPI)/as.vector(lag(monthlyCPI))
monthlyInflation  = zoo( as.vector(monthlyInflation)  , order.by = dates[2:length(dates)])


# Middle interest rate on secondary market 3-month treasury bill:

data = as.data.frame(read.csv( file = "data/Tbillall.csv",skip=5,sep=","))
colnames(data) = c("Date","TBill")
TBill = zoo( data$TBill , order.by = as.Date( as.character(data$Date) , format = "%m/%d/%Y" ) )
TBill = window( TBill , start = start , end = end );
monthlyTBill = aggregate(TBill, as.yearmon , tail , 1 );
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
monthlyTBill = (91/360)*(365/91)*monthlyTBill/(100-monthlyTBill*(91/360) ) ; 
# Make it monthly
monthlyTBill = monthlyTBill/12; 
monthlyTBill = zoo( as.vector(monthlyTBill)  , order.by = dates[2:length(dates)])

# Mixed portfolio 

data = merge.zoo( monthlyR_bond , monthlyR_SP500 , monthlyR_NAREIT, monthlyR_SPGSCI , monthlyTBill,monthlyInflation , monthlyR_EAFE)
colnames(data) = c( "Bond" , "SP500", "NAREIT" ,  "SPGSCI" ,"TBill","Inflation",  "EAFE" )
data = na.locf(data);
head(data); plot(data); tail(data)
write.zoo( data , file="data/data.txt" , row.names=F )
write.csv( data , file="data/data.csv" , row.names=F )
#save( data , file = paste(  getwd(),"/data/equitybondscommodity/equitybondscommodity.Rdata" , sep="")   )



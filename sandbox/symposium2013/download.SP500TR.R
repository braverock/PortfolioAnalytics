# Script for downloading and parsing a monthly total return series from
# http://www.standardandpoors.com/
#
# Peter Carl

# Load needed packages:
require(xts)
require(gdata)

### Constants
filename = "EDHEC-index-history.csv"
objectname = "SP500TR"
datadir = "./data"
cachedir = "./cache"

# Download the first sheet in the xls workbook directly from the web site:
x = read.xls("http://www.spindices.com/documents/additional-material/monthly.xlsx?force_download=true")

# That gives us something like the following:
# > head(x)
#   STANDARD...POOR.S.INDEX.SERVICES       X    X.1      X.2      X.3      X.4
# 1          S&P 500 MONTHLY RETURNS                                          
# 2                                                                           
# 3                         MONTH OF   PRICE  PRICE  1 MONTH 3 MONTH  6 MONTH 
# 4                                    CLOSE CHANGE % CHANGE % CHANGE % CHANGE
# 5                          10/2009 1036.19 -20.88   -1.98%    4.93%   18.72%
# 6                          09/2009 1057.08  36.45    3.57%   14.98%   32.49%
#        X.5      X.6      X.7      X.8      X.9    X.10     X.11 X.12 X.13
# 1                                                                 NA   NA
# 2                                              1 MONTH 12 MONTH   NA   NA
# 3   1 YEAR   2 YEAR   3 YEAR  5 YEARS 10 YEARS  TOTAL     TOTAL   NA   NA
# 4 % CHANGE % CHANGE % CHANGE % CHANGE % CHANGE  RETURN   RETURN   NA   NA
# 5    6.96%  -33.12%  -24.80%   -8.32%  -23.97%  -1.86%    9.80%   NA   NA
# 6   -9.37%  -30.76%  -20.87%   -5.16%  -17.59%   3.73%   -6.91%   NA   NA
#   X.14 X.15
# 1   NA   NA
# 2   NA   NA
# 3   NA   NA
# 4   NA   NA
# 5   NA   NA
# 6   NA   NA

# So we only really care about column 1 for dates and column 12 (X.10) for 
# total returns.  The first four rows are headers, and can be discarded.
rawdates = x[-1:-4,1]
rawreturns = x[-1:-4,12]
# Data goes back to 12/1988. 

# First we convert the dates to something we can use.  Note that frac=1 sets
# the day to the last day of the month.  That should be close enough for
# monthly data.
ISOdates = as.Date(as.yearmon(rawdates, "%m/%Y"), frac=1)

# Now we convert the rawreturns strings into numbers
tr = as.numeric(as.character((sub("%", "", rawreturns, fixed=TRUE))))/100

# Now construct an xts object with the two columns
SP500TR.R=na.omit(as.xts(tr, order.by=ISOdates))
colnames(SP500TR.R)="SP500TR"

# Clean up
rm(list=c("tr", "ISOdates", "rawdates", "rawreturns"))
### Save data into cache
save(SP500TR.R, file=paste(cachedir, "/", objectname, ".RData", sep=""))
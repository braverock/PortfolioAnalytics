
##### Equity Data for Example 1 and Example 2 #####
load("data/crsp_weekly.rda")

equity.data <- cbind(largecap_weekly[,1:15], 
                     midcap_weekly[,1:15], 
                     smallcap_weekly[,1:5])
market <- largecap_weekly[,21]
Rf <- largecap_weekly[,22]

##### edhec Data for Example 3 and Example 4 #####
# Load the updated edhec dataset
load("data/edhec.rda")

# Prep data for Examples 3 and 4
R <- edhec[,c("Convertible.Arbitrage", "Equity.Market.Neutral", 
              "Fixed.Income.Arbitrage", 
              "CTA.Global", "Emerging.Markets", "Global.Macro")]
# Abreviate column names for convenience and plotting
colnames(R) <- c("CA", "EMN", "FIA", "CTAG", "EM", "GM")


# clean up and remove the data we don't need
rm(largecap_weekly, midcap_weekly, smallcap_weekly, microcap_weekly)
rm(edhec)

# I. Different options
data(attrib)
for (i in c("none", "top.down", "bottom.up")){
    for (j in c("carino", "menchero", "grap", "frongello", "davies.laker")){
        for (k in c(TRUE, FALSE)){
            for (l in c(TRUE, FALSE)){
            Attribution(Rp, wp, Rb, wb, i, j, k, l)
            }
        }
    }
}

Attribution(Rp, wp, Rb, wb, method = "top.down", linking = "carino")
Attribution(Rp, wp, Rb, wb, method = "bottom.up", linking = "menchero")
Attribution(Rp, wp, Rb, wb, method = "none", linking = "grap")
Attribution(Rp, wp, Rb, wb, method = "none", linking = "frongello")
Attribution(Rp, wp, Rb, wb, method = "none", linking = "davies.laker")
Attribution(Rp, wp, Rb, wb, method = "none", geometric = TRUE)

Weight.transform(wp, Rp)
Return.level(Rp, wp, h, level = "Sector")
Weight.level(wp, h, level = "Sector")

Attribution.levels(Rp, wp, Rb, wb, h, c("type", "currency", "Sector"))
Attribution.levels(Rp, wp, Rb, wb, h, c("currency", "Sector"))
Attribution.levels(Rp, wp, Rb, wb, h, c("type", "Sector"))

# II. Comparing with results in books
# Data
allocation <- matrix(c(0, -0.0104, -0.0016, -0.0072, -0.0086, 0.0108, 0.025, 0.0175, -0.0075, -0.003, -0.007, 0), 4, 3, TRUE)
selection <- matrix(c(0.04, -0.003, -0.006, 0.014, -0.002, 0.005, 0.015, 0.015, 0.01, 0.015, -0.01, 0.03), 4, 3, TRUE)
wp <- matrix(c(0.4, 0.3, 0.3, 0.7, 0.2, 0.1, 0.3, 0.5, 0.2, 0.3, 0.5, 0.2), 4, 3, TRUE)
wb <- matrix(c(0.4, 0.2, 0.4, 0.4, 0.3, 0.3, 0.5, 0.4, 0.1, 0.4, 0.4, 0.2), 4, 3, TRUE)
Rp <- matrix(c(0.2, -0.05, 0.06, -0.05, 0.03, -0.05, -0.2, 0.08, -0.15, 0.1, -0.07, 0.25), 4, 3, TRUE)
Rb <- matrix(c(0.1, -0.04, 0.08, -0.07, 0.04, -0.1, -0.25, 0.05, -0.2, 0.05, -0.05, 0.1), 4, 3, TRUE)
rownames(wp) <- c("2012-01-01", "2012-02-01", "2012-03-01", "2012-04-01")
colnames(wp) <- c("UK equities", "Japanese equities", "US equities")
rownames(wb) <- c("2012-01-01", "2012-02-01", "2012-03-01", "2012-04-01")
colnames(wb) <- c("UK equities", "Japanese equities", "US equities")
rownames(Rp) <- c("2012-01-01", "2012-02-01", "2012-03-01", "2012-04-01")
colnames(Rp) <- c("UK equities", "Japanese equities", "US equities")
rownames(Rb) <- c("2012-01-01", "2012-02-01", "2012-03-01", "2012-04-01")
colnames(Rb) <- c("UK equities", "Japanese equities", "US equities")
Rp <- checkData(Rp)
Rb <- checkData(Rb)
wp <- checkData(wp)
wb <- checkData(wb)
rownames(allocation) <- c("2012-01-01", "2012-02-01", "2012-03-01", "2012-04-01")
colnames(allocation) <- c("UK equities", "Japanese equities", "US equities")
rownames(selection) <- c("2012-01-01", "2012-02-01", "2012-03-01", "2012-04-01")
colnames(selection) <- c("UK equities", "Japanese equities", "US equities")
allocation <- cbind(allocation, rowSums(allocation))
selection <- cbind(selection, rowSums(selection))
rp = reclass(rowSums(Rp * wp), Rp)
rb = reclass(rowSums(Rb * wb), Rb)
rp = checkData(rp)
rb = checkData(rb)

# 1. Arithmetic attribution (Brinson, Hood and Beebower model)
RP=Rp[1, ]
RB=Rb[1, ]
WB=as.vector(wb[1, ])
WP=as.vector(wp[1, ])
Attribution(RP, WP, RB, WB, "none", "carino") # The same as in Table 5.2 (Bacon, 2008)

# 2. Carino linking
Carino(rp, rb, allocation, TRUE)   # Attribution effects are the same as in Table 8.2 (Bacon, 2008)
Carino(rp, rb, selection, TRUE)    # Attribution effects are the same as in Table 8.2 (Bacon, 2008)

# 3. Menchero linking
Menchero(rp, rb, allocation, TRUE) # Attribution effects are the same as in Table 8.3 (Bacon, 2008)
Menchero(rp, rb, selection, TRUE)  # Attribution effects are the same as in Table 8.3 (Bacon, 2008)

# 4. GRAP linking
Grap(rp, rb, allocation, TRUE)     # Attribution effects are the same as in Table 8.4 (Bacon, 2008)
Grap(rp, rb, selection, TRUE)      # Attribution effects are the same as in Table 8.4 (Bacon, 2008)

# 5. Frongello linking
Frongello(rp, rb, allocation, TRUE)# Attribution effects are the same as in Table 8.5 (Bacon, 2008)
Frongello(rp, rb, selection, TRUE) # Attribution effects are the same as in Table 8.5 (Bacon, 2008)

# 6. Geometric attribution
# 6.1 Single-period
RP=Rp[1, ]
RB=Rb[1, ]
WB=wb[1, ]
WP=wp[1, ]
Attribution.geometric(RP, WP, RB, WB) # The same as in Table 5.5

# 6.2 Multi-period
index(wp) <- as.Date(c("2011-12-31", "2012-02-01", "2012-03-01", "2012-04-01"))
index(wb) <- as.Date(c("2011-12-31", "2012-02-01", "2012-03-01", "2012-04-01"))
Attribution(Rp, wp, Rb, wb, geometric = TRUE) # Total effects and total excess returns are the 
                                              # same as in Table 8.6 and Exhibit 8.12 (Bacon, 2008)
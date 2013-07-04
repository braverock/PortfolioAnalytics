library(PortfolioAnalytics)

data(edhec)
ret <- edhec[, 1:4]
funds <- colnames(ret)

pspec <- portfolio.spec(assets=funds)

pspec <- add.constraint(portfolio=pspec, type="full_investment", enabled=T)
pspec <- add.constraint(portfolio=pspec, type="box", min=0.05, max=0.65, enabled=T)
pspec <- add.constraint(portfolio=pspec, type="group", groups=c(2, 2), 
                        group_min=c(0.08, 0.05), group_max=c(0.55, 0.85), enabled=T)
pspec <- add.constraint(portfolio=pspec, type="turnover", turnover_target=0.4, enabled=T)
pspec <- add.constraint(portfolio=pspec, type="diversification", div_target=0.6, enabled=T)
pspec <- add.constraint(portfolio=pspec, type="position_limit", max_pos=3, enabled=F)
portfolio <- pspec


# leverage constraints are violated
weights <- c(0.15, 0.25, 0.4, 0.1)
sum(weights)

fn_map(weights, portfolio)

# box constraints are violated
weights <- c(0.05, 0.7, 0.1, 0.15)
sum(weights)

fn_map(weights, portfolio)

# group constraints are violated
weights <- c(0.1, 0.65, 0.1, 0.15)
sum(weights)

fn_map(weights, portfolio)



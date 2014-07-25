library(PortfolioAnalytics)

data(edhec)
ret <- edhec[, 1:4]
funds <- colnames(ret)

pspec <- portfolio.spec(assets=funds)

pspec <- add.constraint(portfolio=pspec, type="full_investment", enabled=T)
pspec <- add.constraint(portfolio=pspec, type="box", min=0.05, max=0.65, enabled=T)
pspec <- add.constraint(portfolio=pspec, type="group", groups=list(1:2,3:4), 
                        group_min=c(0.08, 0.05), group_max=c(0.55, 0.85), enabled=T)
pspec <- add.constraint(portfolio=pspec, type="turnover", turnover_target=0.4, enabled=F)
pspec <- add.constraint(portfolio=pspec, type="diversification", div_target=0.6, enabled=F)
pspec <- add.constraint(portfolio=pspec, type="position_limit", max_pos=3, enabled=T)
portfolio <- pspec


# leverage and position_limit constraints are violated
weights <- c(0.15, 0.25, 0.4, 0.1)
sum(weights)

fn_map(weights, portfolio)

# box constraints are violated but postion_limit is already satisfied
# issue because min vector does not have a zero value and weights[1] = 0
# all constraints are satisfied so there should be no transformation
# Is it reasonable to expect the user to have a min vector with zeros when using position_limit constraints? 
# I try to catch this and modify the tmp_min vector so this does not trigger
# violation of box constraints
weights <- c(0, 0.55, 0.3, 0.15)
sum(weights)

fn_map(weights, portfolio)

# group and position limit constraints are violated
weights <- c(0.1, 0.65, 0.1, 0.15)
sum(weights)

fn_map(weights, portfolio)

# normalize weights from the equal weights seed portfolio
weights <- portfolio$assets
sum(weights)

fn_map(weights, portfolio)

##### relaxing box constraints #####
pspec <- portfolio.spec(assets=funds)

pspec <- add.constraint(portfolio=pspec, type="full_investment", enabled=T)
# make min infeasible and too restrictive
pspec <- add.constraint(portfolio=pspec, type="box", min=0.3, max=0.75, enabled=T)

# weights satisfy leverage constraints but not box constraints
weights <- c(0.15, 0.05, 0.25, 0.55)
sum(weights)

# min constraint needs to be relaxed
# note how min has been changed
fn_map(weights, pspec, TRUE)

##### relaxing group constraints #####
pspec <- portfolio.spec(assets=funds)

pspec <- add.constraint(portfolio=pspec, type="full_investment", enabled=T)
pspec <- add.constraint(portfolio=pspec, type="box", min=0.05, max=0.7, enabled=T)
# Make group constraints too restrictive
pspec <- add.constraint(portfolio=pspec, type="group", groups=list(1:2, 3:4), 
                        group_min=c(0.05, 0.01), group_max=c(0.45, 0.5), enabled=T)

# weights satisfy leverage and box constraints, but not group
weights <- c(0.15, 0.05, 0.10, 0.7)

# group constraints needs to be relaxed
# note how cLO and cUP have been changed
fn_map(weights, pspec, TRUE)

##### relaxing position limits constraints #####
pspec <- portfolio.spec(assets=funds)

pspec <- add.constraint(portfolio=pspec, type="full_investment", enabled=T)
pspec <- add.constraint(portfolio=pspec, type="box", min=0.05, max=0.4, enabled=T)
# Make position limit constraint too restrictive
pspec <- add.constraint(portfolio=pspec, type="position_limit", max_pos=2, enabled=T)

# weights satisfy leverage and box constraints, but not group
weights <- c(0.4, 0.05, 0.15, 0.4)

# position limit constraint needs to be relaxed
# note how max_pos has been increased to 3
fn_map(weights, pspec, TRUE)

##### relaxing leverage exposure constraint #####
pspec <- portfolio.spec(assets=funds)
pspec <- add.constraint(portfolio=pspec, type="weight_sum", 
                        min_sum=0.99, max_sum=1.01)
pspec <- add.constraint(portfolio=pspec, type="box", min=-0.4, max=1)
pspec <- add.constraint(portfolio=pspec, type="leverage_exposure", leverage=1.6)

# weights satisfy leverage and box constraints, but not group
weights <- c(-0.4, 0.75, 0.25, 0.4)
sum(weights)
sum(abs(weights))

# relax leverage exposure constraint
fn_map(weights, pspec, TRUE)

rp_transform(weights, min_sum=0.99, max_sum=1.01, 
             min_box=rep(-0.3, 4), max_box=rep(0.6,4),
             groups=NULL, cLO=NULL, cUP=NULL,
             leverage=1.5, max_permutations=10000)

pspec <- portfolio.spec(assets=funds)
pspec <- add.constraint(portfolio=pspec, type="weight_sum", 
                        min_sum=0.99, max_sum=1.01)
pspec <- add.constraint(portfolio=pspec, type="box", min=-0.4, max=1)
pspec <- add.constraint(portfolio=pspec, type="leverage_exposure", leverage=1.6)
rp <- random_portfolios(pspec, 5000, eliminate=FALSE)
x <- apply(rp, 1, function(x) sum(abs(x)))
plot(x)

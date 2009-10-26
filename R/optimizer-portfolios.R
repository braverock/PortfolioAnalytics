# functions to build portfolios for use by the optimizer

# this code may be made obsolete by the advanced (non-linear, MIP) fPortfolio optimizers, but for now, they are beta code at best

# require(LSPM) # for the un-exported .nPri functions
# generate all feasible portfolios
#LSPM:::.nPri(n=13,r=45,i=n^r,replace=TRUE)
# not likely to actually BE feasible for any portfolio of real size, but I'll write the grid generator anyway that will generate all the permutations, and kick out only the feasible portfolios

# assume a structure that includes column names for each asset
# constraints should be a list by constraint type
# we have min/max constraints on each asset
# constraint <- function (x){
# 
# structure( list(L   = L,
#                   dir = dir,
#                   rhs = rhs,
#                   n_L_constraints = n_L_constraints),
#             class = c("L_constraint", "constraint"))
# }
# ultimately, I'd like to have a real portfolio class, and real constraints classes, but for now we can build Burns-style random portfolios by specifying our box constriants

            

# random portfolios


generatesequence <- function (min=.01, max=1, by=min/max , min_mult=1, max_mult=1, rounding=3 )
{ # @author: Peter Carl, Brian Peterson
  # this creates the sequence of possible weights, not constrained by asset
  ret <- seq(from = round(min*min_mult,rounding), to = round(max*max_mult,rounding), by = by)
  return(ret)
}


randomize_portfolio <- function (seed, weight_seq, min_mult=-Inf,max_mult=Inf, min_sum=.99, max_sum=1.01, max_permutations=100, rounding=3) 
{ # @author: Peter Carl, Brian Peterson (based on an idea by Pat Burns)
  # generate random permutations of a portfolio seed meeting your constraints on the weights of each asset
  # set the portfolio to the seed
  portfolio=as.vector(seed)
  rownames(portfolio)<-NULL
  weight_seq=as.vector(weight_seq)
# initialize our loop
  permutations=1
  #while portfolio is between min/max sum and we have not reached max_permutations
  #while(permutations<=max_permutations){
    # create a temporary portfolio so we don't return a non-feasible portfolio
    tportfolio=portfolio
    # first randomly permute each element of the temporary portfolio
    random_index <- sample(1:length(tportfolio),length(tportfolio))
    for (i in 1:length(tportfolio)) {
       cur_index<-random_index[i]
       cur_val <- tportfolio[cur_index]
       # randomly permute a random portfolio element
       tportfolio[cur_index]<-sample(weight_seq[(weight_seq>=cur_val*min_mult) & (weight_seq<=cur_val*max_mult)],1)
    }
      
    while ((sum(tportfolio)<=min_sum | sum(tportfolio)>=max_sum) & permutations<=max_permutations) {
        permutations=permutations+1
        # check our box constraints on total portfolio weight
        # reduce(increase) total portfolio size till you get a match
        # 1> check to see which bound you've failed on, brobably set this as a pair of while loops
        # 2> randomly select a column and move only in the direction *towards the bound*, maybe call a function inside a function
        # 3> check and repeat
        random_index <- sample(1:length(tportfolio), length(tportfolio))
        i = 1
        while (sum(tportfolio)<=min_sum & i<=length(tportfolio)) {
          # randomly permute and increase a random portfolio element
          cur_val<-tportfolio[random_index[i]]
          cur_index<-i
            if (length(weight_seq[(weight_seq>=cur_val)&(weight_seq<=cur_val*max_mult)])>1)
            {
              # randomly sample one of the larger weights
              tportfolio[cur_index]<-sample(weight_seq[(weight_seq>=cur_val)&(weight_seq<=cur_val*max_mult)],1)
              # print(paste("new val:",tportfolio[cur_index]))
            } else { 
              if (length(weight_seq[(weight_seq>=cur_val)&(weight_seq<=cur_val*max_mult)]) == 1) {
                tportfolio[cur_index]<-weight_seq[(weight_seq>=cur_val)&(weight_seq<=cur_val*max_mult)]
              }
            }
          i=i+1 # increment our counter
        } # end increase loop
        while (sum(tportfolio)>=max_sum & i<=length(tportfolio)) {
          # randomly permute and decrease a random portfolio element
          cur_val<-tportfolio[random_index[i]]
          cur_index<-i
            if (length(weight_seq<=cur_val & weight_seq>=cur_val*min_mult )>1) {
              tportfolio[cur_index]<-sample(weight_seq[which(weight_seq<=cur_val & weight_seq>=cur_val*min_mult )],1)
            } else {
              if (length(weight_seq<=cur_val & weight_seq>=cur_val*min_mult )==1) {
                tportfolio[cur_index]<-weight_seq[(weight_seq<=cur_val) & (weight_seq>=cur_val*min_mult)]
              }
            }
          i=i+1 # increment our counter
        } # end decrease loop
    } # end final walk towards the edges

#     if ((sum(tportfolio)>=min_sum & sum(tportfolio)<=max_sum & permutations<=max_permutations) | permutations <= max_permutations/10) {
      portfolio<-tportfolio
#     }

  colnames(portfolio)<-colnames(seed)
  if (sum(portfolio)<=min_sum | sum(tportfolio)>=max_sum){
        portfolio <- seed
        warning("Infeasible portfolio created, defaulting to seed, perhaps increase max_permutations.")
  }
  if(isTRUE(all.equal(seed,portfolio))) {
    if (sum(seed)>=min_sum & sum(seed)<=max_sum) {
      warning("Unable to generate a feasible portfolio different from seed, perhaps adjust your parameters.")
      return(seed)
    } else {
      warning("Unable to generate a feasible portfolio, perhaps adjust your parameters.")
      return(NULL)
    }
  }
  return(portfolio)
}

# random_walk_portfolios(seed,y,min_sum=.99,max_sum=1.01,permutations=100,max_permutations=5000)
random_walk_portfolios <- function (seed, weight_seq, permutations=100, ...)
{ # @author: Peter Carl, Brian Peterson (based on an idea by Pat Burns)
  # this function generates a series of portfolios that are a "random walk" from the current portfolio
  result <- matrix(nrow=permutations, ncol=length(seed))
  i <- 1
  while (i<=permutations) {
    result[i,] <- as.matrix(randomize_portfolio(seed, weight_seq, ...))
    seed<-result[i,]
    if(i==permutations) {
      result = unique(result)
      i = nrow(result)
      result = rbind(result, matrix(nrow=(permutations-i),ncol=length(seed)))
    }
    i<-i+1
  }
  colnames(result)<-names(seed)
  return(result)
}

# EXAMPLE: start_t<- Sys.time(); x=random_walk_portfolios(rep(1/5,5), generatesequence(min=0.01, max=0.30, by=0.01), max_permutations=500, permutations=5000, min_sum=.99, max_sum=1.01); end_t<-Sys.time(); start_t-end_t;
# > nrow(unique(x))
# [1] 4906
# > which(rowSums(x)<.99 | rowSums(x)>1.01)
# integer(0)

#TODO: write a function that uses the fn's above to generate an arbitrary number of random portfolios
# currently, you can do this pretty easily with a one-line loop
# random_walk_portfolios(seed,seq,permutations=100)
# or 

# start_t <- Sys.time(); s<-foreach(seed=iter(weights, by='row'),.combine=rbind) %dopar% random_walk_portfolios(seed,xseq,permutations=10000); end_t <- Sys.time(); save.image(); start_t-end_t;

random_portfolios <- function(n=1000, nassets=NULL, seed=NULL, nseeds=nportfolios/10, ...)
{ # @author: Peter Carl, Brian Peterson (based on an idea by Pat Burns)
  # generate an arbitrarily large number of random portfolios.  typically, extra constraints would be passed in via ...

  if (is.null(nassets)&is.null(seed)) {
    stop("You must specify either the number of assets or a seed portfolio")
  }

  # generate a seed if we didn't get one.  Use zeroes
  if (is.null(seed) | nrow(seed)<nseeds) {
    #if(is.null(seed)) 
        # generate new seeds

        # start with equal weight

        # 
  }

  for (i in 1:nrow(seed)){
# 	randomize_portfolio(seed[i,],weight_seq, min_mult=-Inf,max_mult=Inf, min_sum=.99, max_sum=1.01, max_permutations=300, rounding=3)
  }
  
}

# TODO: write a function for random trades that only makes n trades and increases/decreases other lelemnts to compensate.
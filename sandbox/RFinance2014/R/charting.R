nvd3WeightsPlot <- function(object, 
                            type=c("stackedAreaChart", "multiBarChart")
                            ){
  type <- match.arg(type)
  
  # extract the weights and turn into a data.frame
  weights <- extractWeights(object)
  weights.df <- reshape2::melt(
    data.frame(date=format(index(weights)), weights),
    id.vars = 1, 
    variable.name = "stock", 
    value.name = "weight"
  )
  weights.df$date <- as.Date(weights.df$date)
  
  # plot
  n1 <- rCharts::nPlot(
    weight ~ date, 
    group = "stock", 
    data = weights.df, 
    type = type
  )
  n1$xAxis(
    tickFormat = "#!function(d){
    return d3.time.format('%b %Y')(new Date(d * 24 * 60 * 60 * 1000))
    }!#"
  )
  n1$yAxis(
    tickFormat = "#!function(d){
    return d3.format('0.2%')(d)
    }!#"
  )
  return(n1)
}

nvd3RiskPlot <- function(object, 
                            type=c("stackedAreaChart", "multiBarChart")
                         ){
  type <- match.arg(type)
  
  # extract the risk budget pct_contrib and turn into a data.frame
  tmp <- extractObjectiveMeasures(object)
  rb <- tmp[,grep("pct_contrib", colnames(tmp))]
  colnames(rb) <- gsub("^.*\\.", "", colnames(rb))
  rb.df <- reshape2::melt(
    data.frame(date=as.Date(format(index(rb))), rb),
    id.vars = 1, 
    variable.name = "fund", 
    value.name = "risk"
  )
  
  # plot
  n1 <- rCharts::nPlot(
    risk ~ date, 
    group = "fund", 
    data = rb.df, 
    type = type
  )
  n1$xAxis(
    tickFormat = "#!function(d){
    return d3.time.format('%b %Y')(new Date(d * 24 * 60 * 60 * 1000))
    }!#"
  )
  n1$yAxis(
    tickFormat = "#!function(d){
    return d3.format('0.2%')(d)
    }!#"
  )
  return(n1)
}

# require(rCharts)
# weights <- extractWeights(opt.minVarSample)
# weights.df <- reshape2::melt(
#   data.frame(
#     date=format(index(weights)),
#     weights
#   ),
#   id.vars = 1, 
#   variable.name = "stock", 
#   value.name = "weight"
# )
# 
# d1 <- dPlot(
#   weight ~ date, 
#   groups = "stock", 
#   data = weights.df, 
#   type = "bubble" #area, bar, or bubble
# )
# d1$xAxis(
#   type = "addTimeAxis", 
#   inputFormat = "%Y-%m-%d", 
#   outputFormat = "%b %Y"
# )
# d1$yAxis(
#   outputFormat = "0.2%", 
#   orderBy = "weight"
# )
# d1
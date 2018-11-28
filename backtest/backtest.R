library(data.table)
library(plyr)
library(xlsx)
library(matrixStats)

####################################################
# load data
####################################################

data <- fread('../data/crsp.msf.csv', header=T)
colnames(data) <- sapply(colnames(data), tolower)

setorder(data, date)
setindex(data, permno)
setindex(data, primexch)

stopifnot(length(which(is.na(data$prc))) == 0)
stopifnot(length(which(is.na(data$ret))) == 0)
stopifnot(length(which(is.na(data$vol))) == 0)
stopifnot(length(which(is.na(data$shrout))) == 0)


# calculate turnover according to paper
data$turnover <- data$vol / data$shrout


# reshape to wide format
data.ret <- dcast(data, date ~ permno, value.var=c('ret'))
data.turnover <- dcast(data, date ~ permno, value.var=c('turnover'))
data.marketcap <- dcast(data, date ~ permno, value.var=c('marketcap'))
data.exch <- dcast(data, date ~ permno, value.var=c('primexch'))

# more efficient representations
dates <- data.ret$date
data.ret <- as.matrix(data.ret[, !'date'])
data.turnover <- as.matrix(data.turnover[, !'date'])
data.marketcap <- as.matrix(data.marketcap[, !'date'])
data.exch <- as.matrix(data.exch[, !'date'])

# excess returns
data.ff <- fread('../data/fama.french.factors.csv', header=T)
data.ff[, 2:ncol(data.ff)] <- data.ff[, !'date'] / 100 # stored in percentage values
data.ret.ex <- data.ret - matrix(rep(data.ff$rf, ncol(data.ret)), ncol=ncol(data.ret))

# sanity check: stock data matches Fama-French factors data (date)
stopifnot(all(dates == data.ff$date))

# free memory
rm(data)





# ####################################################
# # plots of data
# ####################################################
# 
# # plot market cap over time
# plot(rowSums(data.marketcap[, !'date'], na.rm=T), type='l', main="Total market cap (1964 - 2016)", ylab='market capitalization')
# 
# # plot average monthly returns over time
# avg.ret <- rowMeans(data.ret[, !'date'], na.rm=T)
# avg.ret.ts <- cumprod(1 + avg.ret)
# 
# plot(avg.ret, type='l', main='average monthly returns', ylab='return')
# plot(avg.ret.ts, type='l', main='average cumulative returns', ylab='return')
# 
# # histogram of returns
# hist(avg.ret)
# 
# # plot number of stocks over time
# no.constituents <- rowSums(is.na(data.ret[, !'date']))
# plot(no.constituents, type='l', main='Number of stocks over time', xlab='month index', ylab="number of stocks")




####################################################
# backtest
####################################################

source('doublesort.R')
source('movingWindow.R')
source('perf.statistics.R')

# collect previous month's momentum, turnover and market cap.
windowSize <- 1
windowOffset <- 1
measure.momentum <- movingWindow(function(x) x, data.ret, windowSize=windowSize, windowOffset=windowOffset, keepFirstWindowRows=T, returnAsMatrix=T)
measure.turnover <- movingWindow(function(x) x, data.turnover, windowSize=windowSize, windowOffset=windowOffset, keepFirstWindowRows=T, returnAsMatrix=T)
measure.marketCap <- movingWindow(function(x) x, data.marketcap, windowSize=windowSize, windowOffset=windowOffset, keepFirstWindowRows=T, returnAsMatrix=T)
stock.names <- colnames(data.ret)

# portfolio breakpoints (decile) for momentum and turnover are derived using only NYSE stocks
nyse.stocks <- matrix(ifelse(data.exch == 'N', 1, NA), nrow=nrow(data.exch))
colnames(nyse.stocks) <- colnames(data.exch)

nyse.momentum <- nyse.stocks * measure.momentum
nyse.turnover <- nyse.stocks * measure.turnover

breakpoints.momentum <- matrix(NA, nrow=nrow(nyse.momentum), ncol=9)
breakpoints.turnover <- matrix(NA, nrow=nrow(nyse.turnover), ncol=9)
for (i in 1:nrow(breakpoints.momentum)) {
  breakpoints.momentum[i, ] <- quantile(na.omit(nyse.momentum[i, ]), probs=seq(0.1, 0.9, 0.1))
  breakpoints.turnover[i, ] <- quantile(na.omit(nyse.turnover[i, ]), probs=seq(0.1, 0.9, 0.1))
}

# free memory
rm(nyse.momentum)
rm(nyse.turnover)

# sanity checks
stopifnot(nrow(measure.momentum) == nrow(measure.turnover))
stopifnot(nrow(measure.turnover) == nrow(measure.marketCap))
stopifnot(nrow(data.ret) == nrow(measure.momentum))
stopifnot(nrow(data.ret.ex) == nrow(measure.momentum))
stopifnot(nrow(data.turnover) == nrow(measure.momentum))
stopifnot(nrow(data.marketcap) == nrow(measure.momentum))
stopifnot(nrow(breakpoints.momentum) == nrow(data.ret))
stopifnot(nrow(breakpoints.turnover) == nrow(data.ret))

stopifnot(length(which(is.na(breakpoints.momentum[(windowSize+1):nrow(breakpoints.momentum)]))) == 0)
stopifnot(length(which(is.na(breakpoints.turnover[(windowSize+1):nrow(breakpoints.turnover)]))) == 0)

# double-sorted data matrix - avg. returns in cells, momentum in columns, turnover in rows
n.rows <- 10
n.columns <- 10
sort.func.name <- 'doublesort.cond2'
#sort.func.name <- 'doublesort.uncond'
sort.func <- match.fun(sort.func.name)
avg.doublesorted <- matrix(0, nrow=n.rows, ncol=n.columns, dimnames=list(paste0('turnover', 1:n.rows), paste0('ret', 1:n.columns)))
strategy.ret <- c()
market.ret <- c()
range <- (windowSize + 1):(nrow(data.ret) - windowOffset)

#########
# range <- 20:30

doublesort.cond2 <- function(targetValues, rowCriterias, rowBreakpoints, columnCriterias, columnBreakpoints, aggregationFunc, n.rows, n.columns) {
  output <- matrix(NA, nrow=n.rows, ncol=n.columns)
  
  column.ranks <- as.numeric(cut(columnCriterias, c(-Inf, as.vector(columnBreakpoints), Inf)))
  
  for (column in 1:n.columns) {
    column.matches <- which(column.ranks == column)
    column.rowCriterias <- rowCriterias[column.matches]
    column.targetValues <- targetValues[column.matches]
    
    row.ranks <- as.numeric(cut(column.rowCriterias, c(-Inf, as.vector(rowBreakpoints), Inf)))
    
    for (row in 1:n.rows) {
      row.matches <- which(row.ranks == row) # equals cell matches, since conditional sort
      
      # aggregate cell matches to get cell value
      if (length(row.matches) > 0) {
        output[row, column] <- aggregationFunc(column.targetValues[row.matches])
      }
      else {
        output[row, column] <- 0
      }
    }
  }
  
  return(output)
}


for (row in range) {
  rets <- data.ret[row, ]
  momentum <- measure.momentum[row, ]
  turnover <- measure.turnover[row, ]
  marketCap <- measure.marketCap[row, ]
  
  # only consider returns for which we have a turnover and momentum value and no NA values
  idx.intersect <- intersect(which(!is.na(rets)),
                             intersect(which(!is.na(momentum)),
                                       intersect(which(!is.na(turnover)),
                                                 which(!is.na(marketCap)))))
  rets <- rets[idx.intersect]
  momentum <- momentum[idx.intersect]
  turnover <- turnover[idx.intersect]
  # aggregationFunc <- function(rets) {
  #   caps <- marketCap[which(stock.names %in% names(rets))]
  #   return(weightedMean(rets, caps))
  # }
  aggregationFunc <- mean
  
  # double sort
  if (sort.func.name == 'doublesort.cond2') {
    doublesorted.t <- doublesort.cond2(rets, turnover, breakpoints.turnover[row, ], momentum, breakpoints.momentum[row, ], aggregationFunc, n.rows, n.columns)
  }
  else {
    doublesorted.t <- sort.func(rets, turnover, momentum, aggregationFunc, n.rows, n.columns)
  }
  
  # collect returns
  strategy.ret <- c(strategy.ret, doublesorted.t[1, 1] - doublesorted.t[1, n.columns])
  market.ret <- c(market.ret, data.ff$mkt[row])
  
  avg.doublesorted <- avg.doublesorted + doublesorted.t / length(range)
}

print(avg.doublesorted)


perf.strategy <- cumprod(1 + strategy.ret)
perf.market <- cumprod(1 + market.ret)

matplot(log(cbind(perf.market, perf.strategy)), type='l',
        main='High-low strategy performance (log)',
        lty=1, xlab='month', ylab='log performance starting at 1')





# save table
write.xlsx(avg.doublesorted, paste0(sort.func.name, ' ', n.rows, 'x', n.columns, '.xlsx'), sheetName='doublesort', col.names=T, row.names=T)

# save performance
write.xlsx(data.table(ret.strategy=strategy.ret, ret.market=market.ret, perf.strategy=perf.strategy, perf.market=perf.market),
           paste0(sort.func.name, ' ', n.rows, 'x', n.columns, ' strategy.xlsx'),
           sheetName='high - low returns', col.names=T, row.names=F)


cat('Sharpe market: ', sharpe.ratio(market.ret, data.ff$rf[range], 12))
cat('Sharpe strategy: ', sharpe.ratio(strategy.ret, data.ff$rf[range], 12))

cat('Sortino market: ', sortino.ratio(market.ret, data.ff$rf[range], 12))
cat('Sortino strategy: ', sortino.ratio(strategy.ret, data.ff$rf[range], 12))

















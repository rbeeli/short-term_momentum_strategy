library(data.table)
library(plyr)
library(tictoc)
library(RColorBrewer)
library(matrixStats)


####################################################
# load data & pre-process
####################################################

data <- fread('../data/crsp.msf.csv', header=T)
colnames(data) <- sapply(colnames(data), tolower)

setorder(data, date)
setindex(data, date)
setindex(data, permno)


# ensure positive price values:
#  - when there are no trades, CRSP stores a negative value [-1*(bid/ask average)] in the PRC variable
data$prc <- abs(data$prc)

# adjust volume according to paper:
#  - prior to February 2001, we divide Nasdaq volume by 2.0.
#  - from February 2001 to December 2001, we divide by 1.8.
#  - From January 2002 to December 2003, we divide by 1.6.
idxs <- which(data$primexch == 'N' & data$date < 20010201)
data[idxs, 'vol'] <- data[idxs, 'vol'] / 2.0

idxs <- which(data$primexch == 'N' & data$date >= 20010201 & data$date <= 20011231)
data[idxs, 'vol'] <- data[idxs, 'vol'] / 1.8

idxs <- which(data$primexch == 'N' & data$date >= 20020101 & data$date <= 20031231)
data[idxs, 'vol'] <- data[idxs, 'vol'] / 1.6


# calculate turnover according to paper
data$turnover <- (data$vol * 100) / (data$shrout * 1000)

# calculate market cap
data$mcap <- data$prc * data$shrout

# reshape to wide format
data.ret <- dcast(data, date ~ permno, value.var=c('ret'))
data.prc <- dcast(data, date ~ permno, value.var=c('prc'))
data.turnover <- dcast(data, date ~ permno, value.var=c('turnover'))
data.mcap <- dcast(data, date ~ permno, value.var=c('mcap'))

# more efficient representations
dates <- data.ret$date
data.ret <- as.matrix(data.ret[, !'date'])
data.prc <- as.matrix(data.prc[, !'date'])
data.turnover <- as.matrix(data.turnover[, !'date'])
data.mcap <- as.matrix(data.mcap[, !'date'])

# free memory
rm(data)
gc()




# ####################################################
# # plots of data
# ####################################################
# 
# # plot market cap over time
# plot(rowSums(data.mcap[, !'date'], na.rm=T), type='l', main="Total market cap (1964 - 2016)", ylab='market capitalization')
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

# The largest capitalizations in each decile of the NYSE index serve as the breakpoints
# that are applied to various exchange groupings of the universe.

# The returns of the combined portfolios are the value-weighted returns of the relevant deciles.



calc.momentum <- function(returns, volumes, windowSize) {
  mom <- matrix(NA, nrow=nrow(returns), ncol=ncol(returns))
  
  from <- windowSize
  to <- nrow(returns)
  
  for (i in from:to) {
    idx.from <- i - windowSize + 1
    idx.to <- i
    
    wnd.data <- returns[idx.from:idx.to, ]
    wnd.avg.mom <- colMeans(wnd.data, na.rm=T)
    
    mom[i, ] <- wnd.avg.mom
  }
  
  return(mom)
}

calc.turnover <- function(turnover, windowSize) {
  turnover <- matrix(NA, nrow=nrow(turnover), ncol=ncol(turnover))
  
  return(turnover)
}


windowSize.momentum <- 1
windowSize.turnover <- 1

measure.momentum <- calc.momentum(data.ret, data.vol, windowSize.momentum)
measure.turnover <- calc.turnover(data.turnover, windowSize.turnover)
















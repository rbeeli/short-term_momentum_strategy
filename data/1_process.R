library(data.table)
library(plyr)
library(tictoc)
library(RColorBrewer)


# load data from file system
data <- fread('crsp.msf.csv', header=T)
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



head(data.ret[, 1:50])
cat('Number of companies:', length(unique(data$permno)), '\n')



####################################################
# plots
####################################################

# plot market cap over time
plot(totalMarketCap, type='l', main="S&P 500 market capitalization", ylab='market capitalization')

# plot returns of S&P 500 over time
series <- cbind(cumprod(1+sp500.index$ret), cumprod(1+sp500.index.ex$ret), cumprod(1+ff$rf))
cols <- brewer.pal(3, 'Set1')
matplot(series, type='l', main='S&P 500 returns', ylab='return', col=cols)
legend('topleft', legend=c('S&P 500 total returns', 'S&P 500 total excess returns', 'Fama-French risk-free returns'),col=cols, fill=cols)

# plot number of index constituents over time
sp500.constituents <- rep(ncol(stocks.exret) - 2, nrow(stocks.exret)) - rowSums(is.na(stocks.exret))
plot(sp500.constituents, type='l', ylim=c(480, 520), xaxt='n', main='Number of S&P 500 constituents over time', xlab="date", ylab="number of listed stocks")

# plot average daily return over time (equal weighted)
avg.daily.ret.eq <- rowSums(stocks.exret[, -c(1,2)], na.rm=T) / sp500.constituents
avg.daily.ret.eq.mean <- mean(avg.daily.ret.eq)
plot(avg.daily.ret.eq, type='l', main='average daily return (equal weighted)', col='black')
abline(h=avg.daily.ret.eq.mean, col='red')
legend('topright', legend=c('daily average', sprintf('overall average %.4f %%', avg.daily.ret.eq.mean * 100)), col=c('black', 'red'), fill=c('black', 'red'))

# plot average daily return over time (value weighted)
avg.daily.ret.vw <- rowSums(stocks.exret[, -c(1,2)] * sp500.weights[, -c(1)], na.rm=T)
avg.daily.ret.vw.mean <- mean(avg.daily.ret.vw)
plot(avg.daily.ret.vw, type='l', main='average daily return (value weighted)', col='black')
abline(h=avg.daily.ret.vw.mean, col='red')
legend('topright', legend=c('daily average', sprintf('overall average %.4f %%', avg.daily.ret.vw.mean * 100)), col=c('black', 'red'), fill=c('black', 'red'))















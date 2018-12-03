library(data.table)
library(plyr)
library(xlsx)
library(matrixStats)
library(ggplot2)
library(ggthemes)
library(scales)


####################################################
# load data
####################################################

# returns
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

# more efficient matrix representations with less overhead
data.dates <- data.ret$date
data.ret <- as.matrix(data.ret[, !'date'])
data.turnover <- as.matrix(data.turnover[, !'date'])
data.marketcap <- as.matrix(data.marketcap[, !'date'])
data.exch <- as.matrix(data.exch[, !'date'])


# Fama-French factors
data.ff <- fread('../data/fama.french.factors.csv', header=T)

# excess returns
data.ret.ex <- data.ret - matrix(rep(data.ff$rf, ncol(data.ret)), ncol=ncol(data.ret))

# sanity checks
stopifnot(all.equal(nrow(data.ret), nrow(data.ret.ex), nrow(data.turnover)))
stopifnot(all(data.dates == data.ff$date))

# free memory
rm(data)






####################################################
# backtest
####################################################

source('doublesort.R')
source('movingWindow.R')
source('sharpe.R')


# collect previous month's momentum, turnover and market cap
mwnd.dates <- as.Date(as.matrix(data.dates[-1]))
mwnd.rets <- data.ret[-1, ]
mwnd.rets.ex <- data.ret.ex[-1, ]
mwnd.ff <- data.ff[-1, ]

windowSize <- 1
mwnd.momentum <- movingWindow(function(x) x, data.ret.ex, windowSize=windowSize, windowOffset=1, returnAsMatrix=T)
mwnd.turnover <- movingWindow(function(x) x, data.turnover, windowSize=windowSize, windowOffset=1, returnAsMatrix=T)
mwnd.marketCap <- movingWindow(function(x) x, data.marketcap, windowSize=windowSize, windowOffset=1, returnAsMatrix=T)
mwnd.exch <- movingWindow(function(x) x, data.exch, windowSize=windowSize, windowOffset=1, returnAsMatrix=T)

# sanity checks
stopifnot(all.equal(length(mwnd.dates), nrow(mwnd.rets), nrow(mwnd.rets.ex), nrow(mwnd.momentum), nrow(mwnd.turnover),
                    nrow(mwnd.marketCap), nrow(mwnd.exch), nrow(mwnd.ff)))
stopifnot(all(colnames(mwnd.rets.ex) == colnames(data.ret.ex)))



# portfolio breakpoints (deciles) for momentum (last month's return) are derived using only NYSE stocks
nyse.stocks <- matrix(ifelse(mwnd.exch == 'N', 1, NA), nrow=nrow(mwnd.exch))
nyse.momentum <- nyse.stocks * mwnd.momentum

breakpoints.momentum <- matrix(NA, nrow=nrow(nyse.momentum), ncol=9)
for (i in 1:nrow(breakpoints.momentum)) {
  breakpoints.momentum[i, ] <- quantile(na.omit(nyse.momentum[i, ]), probs=seq(0.1, 0.9, 0.1))
}

# free memory
rm(nyse.momentum, nyse.stocks)




# calculate double-sorted deciles tables as 3D matrix (dimensions: turnover (rows), momentum (columns), month)
n.rows <- 10
n.columns <- 10
n.months <- length(mwnd.dates)
stock.names <- colnames(mwnd.rets.ex)

ts.deciles.rets <- array(NA, dim=c(n.rows, n.columns, n.months), dimnames=list(paste0('turnover', 1:n.rows), paste0('ret', 1:n.columns)))
ts.deciles.rets.ex <- array(NA, dim=c(n.rows, n.columns, n.months), dimnames=list(paste0('turnover', 1:n.rows), paste0('ret', 1:n.columns)))
ts.deciles.marketCaps <- array(NA, dim=c(n.rows, n.columns, n.months), dimnames=list(paste0('turnover', 1:n.rows), paste0('ret', 1:n.columns)))
ts.deciles.counts <- array(NA, dim=c(n.rows, n.columns, n.months), dimnames=list(paste0('turnover', 1:n.rows), paste0('ret', 1:n.columns)))

for (month in 1:n.months) {
  rets <- mwnd.rets[month, ]
  rets.ex <- mwnd.rets.ex[month, ]
  momentum <- mwnd.momentum[month, ]
  turnover <- mwnd.turnover[month, ]
  marketCap <- mwnd.marketCap[month, ]
  breakpoints.mom <- breakpoints.momentum[month, ]
  
  # only consider stocks which have a return, momentum, turnover and market cap. value
  idx.intersect <- Reduce(intersect, list(which(!is.na(rets)), which(!is.na(momentum)), which(!is.na(turnover)), which(!is.na(marketCap))))
  rets <- rets[idx.intersect]
  rets.ex <- rets.ex[idx.intersect]
  momentum <- momentum[idx.intersect]
  turnover <- turnover[idx.intersect]
  marketCap <- marketCap[idx.intersect]
  
  ####################
  # double sorts
  
  # value-weighted returns in each decile
  ts.deciles.rets[ , , month] <- doublesort.conditional.colbreaks(rets, turnover, momentum, breakpoints.mom, function(rets) {
    caps <- mwnd.marketCap[month, which(stock.names %in% names(rets))]
    return(weightedMean(rets, caps))
  }, n.rows, n.columns)
  
  # value-weighted returns in each decile
  ts.deciles.rets.ex[ , , month] <- doublesort.conditional.colbreaks(rets.ex, turnover, momentum, breakpoints.mom, function(rets) {
    caps <- mwnd.marketCap[month, which(stock.names %in% names(rets))]
    return(weightedMean(rets, caps))
  }, n.rows, n.columns)
  
  # average market cap. in each decile
  ts.deciles.marketCaps[ , , month] <- doublesort.conditional.colbreaks(marketCap, turnover, momentum, breakpoints.mom, mean, n.rows, n.columns)
  
  # number of stocks in each decile
  ts.deciles.counts[ , , month] <- doublesort.conditional.colbreaks(rets, turnover, momentum, breakpoints.mom, length, n.rows, n.columns)
}

# calculate statistics of each deciles
ts.deciles.rets.mean <- apply(ts.deciles.rets, c(1,2), mean)
ts.deciles.rets.ex.mean <- apply(ts.deciles.rets.ex, c(1,2), mean)
ts.deciles.marketCaps.mean <- apply(ts.deciles.marketCaps, c(1,2), mean)
ts.deciles.counts.mean <- apply(ts.deciles.counts, c(1,2), mean)
ts.deciles.betas <- apply(ts.deciles.rets, c(1,2), function(rets) {
  # CAPM regression
  fit <- lm(rets - rf ~ mktrf, data=mwnd.ff)
  return(coef(fit)['mktrf'])
})


# print deciles tables
print(ts.deciles.rets.mean)
print(ts.deciles.rets.ex.mean)
print(ts.deciles.marketCaps.mean)
print(ts.deciles.counts.mean)
print(ts.deciles.betas)

# save deciles tables
write.xlsx(ts.deciles.rets.mean, paste0('doublesort ', n.rows, 'x', n.columns, ' average returns.xlsx'), sheetName='doublesort', col.names=T, row.names=T)
write.xlsx(ts.deciles.rets.ex.mean, paste0('doublesort ', n.rows, 'x', n.columns, ' average excess returns.xlsx'), sheetName='doublesort', col.names=T, row.names=T)
write.xlsx(ts.deciles.marketCaps.mean, paste0('doublesort ', n.rows, 'x', n.columns, ' average market cap.xlsx'), sheetName='doublesort', col.names=T, row.names=T)
write.xlsx(ts.deciles.counts.mean, paste0('doublesort ', n.rows, 'x', n.columns, ' average count.xlsx'), sheetName='doublesort', col.names=T, row.names=T)
write.xlsx(ts.deciles.betas, paste0('doublesort ', n.rows, 'x', n.columns, ' betas.xlsx'), sheetName='doublesort', col.names=T, row.names=T)



# analyze long-short strategies
pf.rets <- t(ts.deciles.rets[ , n.columns, ] - ts.deciles.rets[ , 1, ])
pf.rets.ex <- t(ts.deciles.rets.ex[ , n.columns, ] - ts.deciles.rets.ex[ , 1, ])
pf.rets.combo <- 0.5 * pf.rets[, 10] - 0.5 * pf.rets[, 1]
pf.rets.combo.ex <- 0.5 * pf.rets.ex[, 10] - 0.5 * pf.rets.ex[, 1]

pf.rets.means.ex <- colMeans(pf.rets.ex)
pf.rets.tvalues <- unlist(apply(pf.rets.ex, 2, function(x) t.test(x)[1]))
pf.rets.alpha.ff5 <- apply(pf.rets, 2, function(rets) {
  # Fama-French 5 factor regression analysis
  fit <- lm(rets - rf ~ mktrf + smb + hml + cma + rmw, data=mwnd.ff)
  return(coef(fit)["(Intercept)"])
})
pf.rets.capm.beta <- apply(pf.rets, 2, function(rets) {
  # CAPM regression
  fit <- lm(rets - rf ~ mktrf, data=mwnd.ff)
  return(coef(fit)['mktrf'])
})
pf.rets.sharpe <- apply(pf.rets, 2, function(rets) {
  return(sharpe.ratio(rets, mwnd.ff$rf, scale=12))
})

# save to file
write.xlsx(data.table(ret.avg=pf.rets.means.ex, ret.t=pf.rets.tvalues, ret.alpha.ff5=pf.rets.alpha.ff5, beta.capm=pf.rets.capm.beta, sharpe=pf.rets.sharpe),
           paste0('doublesort ', n.rows, 'x', n.columns, ' statistics.xlsx'),
           sheetName='statistics', col.names=T, row.names=F)





perf.pf.ret <- colCumprods(1 + pf.rets.ex)

par(mfrow=c(1,1))
matplot(log(perf.pf.ret[, c(1,10)]), type='l', lty=1, main=expression('High-low ' ~ r['1,0'] ~ ' long-short portfolios'), ylab='performance (log)')

perf.pf.highturnover <- cumsum(pf.rets.ex[, 10])
perf.pf.lowturnover <- cumsum(pf.rets.ex[, 1])
perf.pf.combo <- cumsum(pf.rets.combo.ex)
perf.pf.market <- cumsum(mwnd.ff$mktrf)


# plot whole date range
plot.data.wide <- data.table(date=mwnd.dates, low.turnover=perf.pf.lowturnover, high.turnover=perf.pf.highturnover, combo=perf.pf.combo, market=perf.pf.market)
plot.data <- melt(plot.data.wide, id.vars=c('date'))

ggplot(data=plot.data, aes(x=date, group=variable)) +
  geom_line(aes(y=value, colour=variable)) +
  labs(y="cumulative excess returns", x="") +
  scale_x_date(date_breaks='48 months', labels=date_format("%m.%Y")) +
  scale_y_continuous(labels=function(x) paste0(x*100, '%'), breaks=seq(-100, 100, by=2)) +
  scale_colour_manual('', values=c('low.turnover'='red', 'high.turnover'='black', 'combo'='green', 'market'='blue'),
                      labels=c('Short-term reversal', 'Short-term momentum', '50:50 combo', 'FF market')) +
  theme_hc() +
  theme(axis.text.x=element_text(angle=45, hjust=1), legend.position='bottom')


# plot newer date range
plot.data.wide <- data.table(date=mwnd.dates, low.turnover=perf.pf.lowturnover, high.turnover=perf.pf.highturnover, combo=perf.pf.combo, market=perf.pf.market)
plot.data.wide <- plot.data.wide[(nrow(plot.data.wide) - 20*12):nrow(plot.data.wide), ]
plot.reset <- matrix(rep(as.matrix(plot.data.wide[1, 2:ncol(plot.data.wide)]), nrow(plot.data.wide)), nrow=nrow(plot.data.wide), byrow=T)
plot.data.wide[, 2:ncol(plot.data.wide)] <- plot.data.wide[, 2:ncol(plot.data.wide)] - plot.reset
plot.data <- melt(plot.data.wide, id.vars=c('date'))

ggplot(data=plot.data, aes(x=date, group=variable)) +
  geom_line(aes(y=value, colour=variable)) +
  labs(y="cumulative excess returns", x="") +
  scale_x_date(date_breaks='48 months', labels=date_format("%m.%Y")) +
  scale_y_continuous(labels=function(x) paste0(x*100, '%'), breaks=seq(-100, 100, by=2)) +
  scale_colour_manual('', values=c('low.turnover'='red', 'high.turnover'='black', 'combo'='green', 'market'='blue'),
                      labels=c('Short-term reversal', 'Short-term momentum', '50:50 combo', 'FF market')) +
  theme_hc() +
  theme(axis.text.x=element_text(angle=45, hjust=1), legend.position='bottom')



cat('Sharpe market: ', sharpe.ratio(mwnd.ff$mkt, mwnd.ff$rf, 12))

cat('Sharpe combo: ', sharpe.ratio(pf.rets.combo, mwnd.ff$rf, 12))

cat('Beta combo: ', (function(rets) {
  # CAPM regression
  fit <- lm(rets - rf ~ mktrf, data=mwnd.ff)
  return(coef(fit)['mktrf'])
})(pf.rets.combo))

cat('t-stat combo: ', unlist(t.test(pf.rets.combo.ex)[1]))

cat('FF alpha: ', (function(rets) {
  # Fama-French 5 factor regression analysis
  fit <- lm(rets - rf ~ mktrf + smb + hml + cma + rmw, data=mwnd.ff)
  return(coef(fit)["(Intercept)"])
})(pf.rets.combo))



















library(RPostgres)
library(data.table)
library(readr)
library(bsts)


# connect to WRDS database server
wrds <- dbConnect(Postgres(), host='wrds-pgdata.wharton.upenn.edu', port=9737, dbname='wrds', sslmode='require', user='rbeeli', password=read_file('WRDS_pwd.txt'))


#######################################
# query data from WRDS server
#######################################
res <- dbSendQuery(wrds, paste0(
    "select m.permno, m.permco, m.cusip, m.date, m.ret, m.prc, m.shrout, m.vol, b.shrcd, b.primexch
       from crsp.msf as m
  left join crsp.msenames as b on b.permno=m.permno
      where (m.date >= '1983-01-01' and m.date <= '2017-12-31')
        and b.primexch IN('N','A','Q')
        and b.shrcd IN(10, 11)
        and b.namedt <= m.date
        and m.date <= b.nameendt"))
data <- data.table(dbFetch(res, n=-1))
dbClearResult(res)



#######################################
# adjust for delisting returns
#######################################
# https://wrds-www.wharton.upenn.edu/pages/support/research-wrds/wrds-research-applications/size-portfolios-common-stocks-using-nyse-breakpoints-python/
res <- dbSendQuery(wrds, paste0("select permno, dlret, dlstdt from crsp.msedelist"))
dldata <- data.table(dbFetch(res, n=-1))
dbClearResult(res)


# close WRDS database server connection
dbDisconnect(wrds)
rm(res)
rm(wrds)



# set delisting date to last day of delisting month
dldata$dlstdt.eom <- LastDayInMonth(dldata$dlstdt)

# merge delisting data with stock data
data <- merge(data, dldata, by.x=c('permno', 'date'), by.y=c('permno', 'dlstdt.eom'), all.x=T)



#######################################
# data pre-processing
#######################################

# ensure positive price values
data$prc <- abs(data$prc)

# calculate unscaled volume
data$vol <- data$vol * 100

# calculate unscaled shares outstanding
data$shrout <- data$shrout * 1000

# calculate market cap
data$marketcap <- data$prc * data$shrout

# calculate total returns by incorporating delisting returns
data$ret <- ifelse(is.na(data$dlret), data$ret, ifelse(is.na(data$ret), data$dlret, (1 + data$ret) * (1 + data$dlret) - 1))
  
# remove records without a price (PRC)
data <- data[-which(is.na(data$prc)), ]

# remove record of companies without a return (RET)
#  - first entry of time series of company
#  - change of exchange
#  - company was excluded from exchange for a period of time (sometimes prices change significantly)
data <- data[-which(is.na(data$ret)), ]

# remove companies with no volume data
permnos.zero.vol <- c()
for (i.permno in unique(data$permno[which(is.na(data$vol))])) {
  recs <- data[permno == i.permno, 'vol']$vol
  
  if (length(recs) == length(which(is.na(recs)))) {
    permnos.zero.vol <- c(permnos.zero.vol, i.permno)
  }
}
data <- data[-which(data$permno %in% permnos.zero.vol), ]

# set missing volume data to 0
data$vol[which(is.na(data$vol))] <- 0

# adjust volume according to paper:
#  - prior to February 2001, we divide Nasdaq volume by 2.0
#  - from February 2001 to December 2001, we divide by 1.8
#  - from January 2002 to December 2003, we divide by 1.6
idxs <- which(data$primexch == 'N' & data$date < 20010201)
data[idxs, 'vol'] <- data[idxs, 'vol'] / 2.0

idxs <- which(data$primexch == 'N' & data$date >= 20010201 & data$date <= 20011231)
data[idxs, 'vol'] <- data[idxs, 'vol'] / 1.8

idxs <- which(data$primexch == 'N' & data$date >= 20020101 & data$date <= 20031231)
data[idxs, 'vol'] <- data[idxs, 'vol'] / 1.6


# write CRSP data to CSV files
fwrite(data, 'crsp.msf.csv', sep=';')






#######################################
# Fama-French factors
#######################################

ffData <- fread('F-F_Research_Data_5_Factors_2x3.CSV', header=T, sep=',')
ffData$mkt <- ffData$mktrf + ffData$rf
ffData$rf <- abs(ffData$rf)
ffData$date <- as.character(ffData$date)

# fliter Fama-French factors to match returns data
dates <- sort(unique(data$date))
datesFormatted <- format(sort(unique(data$date)), '%Y%m')
ffDates.idxs <- which(ffData$date %in% datesFormatted)

stopifnot(length(ffDates.idxs) == length(dates))

ffData <- cbind(dates, ffData[ffDates.idxs, !'date'])
colnames(ffData)[1] <- 'date'

# write Fama-French factors data to CSV files
fwrite(ffData, 'fama.french.factors.csv', sep=';')






# https://wrds-www.wharton.upenn.edu/pages/support/wrds-cloud/r-wrds-cloud/accessing-wrds-data-r/
#
# res <- dbSendQuery(wrds, "select distinct table_schema
#                    from information_schema.tables
#                    where table_type ='VIEW'
#                    or table_type ='FOREIGN TABLE'
#                    order by table_schema")
# data <- dbFetch(res, n=-1)
# dbClearResult(res)
# data
# 
# 
# res <- dbSendQuery(wrds, "select distinct table_name
#                    from information_schema.columns
#                    where table_schema like 'crsp%'
#                    order by table_name")
# data <- dbFetch(res, n=-1)
# dbClearResult(res)
# data
# 
# res <- dbSendQuery(wrds, "select column_name, data_type
#                    from information_schema.columns
#                    where table_schema like 'crsp' and table_name='msenames'")
# data <- dbFetch(res, n=-1)
# dbClearResult(res)
# data


















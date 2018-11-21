library(RPostgres)
library(data.table)
library(readr)
library(bsts)

# connect to WRDS server
wrds <- dbConnect(Postgres(), host='wrds-pgdata.wharton.upenn.edu', port=9737, dbname='wrds', sslmode='require', user='rbeeli', password=read_file('CRSP_pw.txt'))


# perform query
res <- dbSendQuery(wrds, paste0(
    "select m.permno, m.permco, m.cusip, m.date, m.ret, m.prc, m.shrout, m.vol, b.shrcd, b.primexch
       from crsp.msf as m
  left join crsp.msenames as b on b.permno=m.permno
      where (m.date >= '1963-07-01' and m.date <= '2016-12-31')
        and primexch IN('N','A','Q')
        and shrcd IN(10, 11)"))
data <- data.frame(dbFetch(res, n=-1))
dbClearResult(res)

# adjust for delisting returns
# https://wrds-www.wharton.upenn.edu/pages/support/research-wrds/wrds-research-applications/size-portfolios-common-stocks-using-nyse-breakpoints-python/
res <- dbSendQuery(wrds, paste0("select permno, dlret, dlstdt from crsp.msedelist"))
dldata <- data.frame(dbFetch(res, n=-1))
dbClearResult(res)

# set delisting date to last day of month of delisting date
dldata$dlstdt.eom <- LastDayInMonth(dldata$dlstdt)

# merge delisting data with stock data
data <- merge(data, dldata, by.x=c('permno', 'date'), by.y=c('permno', 'dlstdt.eom'), all.x=T)
data <- data[, !'dlstdt']

# calculate total return
data$totret <- ifelse(is.na(data$dlret), data$ret, (1 + data$ret) * (1 + data$dlret) - 1)



# close connection
dbDisconnect(wrds)
rm(res)
rm(wrds)


# write to CSV files
fwrite(data, 'crspa.msf.csv', sep=';')








# https://wrds-www.wharton.upenn.edu/pages/support/wrds-cloud/r-wrds-cloud/accessing-wrds-data-r/

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

res <- dbSendQuery(wrds, "select column_name, data_type
                   from information_schema.columns
                   where table_schema like 'crsp' and table_name='msenames'")
data <- dbFetch(res, n=-1)
dbClearResult(res)
data



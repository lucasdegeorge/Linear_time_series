require(zoo) 
require(tseries) 

path <- "C:/Users/lucas/Documents/GitHub/Linear_time_series_electricity"
setwd(path) 
getwd() 
list.files()

datafile <- "valeurs_mensuelles.csv" 
data <- read.csv(datafile,sep=";")

dates_char <- as.character(data$dates)
dates_char[1] #
tail(dates_char,1) #
dates <- as.yearmon(seq(from=1990, to=2023, by=1/12))


prod <- zoo(data[[2]])
T <- length(prod.source)
plot(prod)


datafile_test <- "data_tp5.csv" 
data_test <- read.csv(datafile_test,sep=";")

dates_char <- as.character(data_test$dates)
dates_char[1] #
tail(dates_char,1) #
dates <- as.yearmon(seq(from=1990, to=2023, by=1/12)) #
spread <- zoo(data_test$spread, order.by=dates)
dspread <- diff(spread,1)
plot(cbind(spread,dspread))





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
dates <- as.yearmon(seq(from=2023, to=1990, by=-1/12))

prod <- zoo(data$values, order.by=dates)

plot(prod)

diff_prod = diff(prod,1)

plot(diff_prod)







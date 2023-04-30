require(zoo) 
require(tseries) 

library(readr)
library(tidyverse)
library(plyr)
library(questionr)
library(corrplot)
library(Hmisc)
library(lmtest)
library(margins)
library(psych)

path <- "C:/Users/lucas/Downloads"
# path <- "C:/Users/lucas/Documents/GitHub/Linear_time_series_electricity"
setwd(path) 
getwd() 

# Loading of data

datafile <- "valeurs_mensuelles.csv"
data <- as.data.frame(read.csv(datafile,sep=";"))

data <- rename.variable(data, "Indice.CVS.CJO.de.la.production.industrielle..base.100.en.2015...Construction.navale..NAF.rév..2.niveau.groupe..poste.30.1", "values" )
data <- rename.variable(data, "Libellé", "dates")
data <- data[,-3]
data <- data[-1:-2,]
donnees <- apply ( data , 2 , rev )


dates_char <- as.character(data$dates)
dates_char[1] #
tail(dates_char,1) #
dates <- as.yearmon(seq(from=2023, to=1990, by=-1/12))

prod <- zoo(data$values, order.by=dates)

plot(prod)

## Part I ##

# Question 2

diff_prod = diff(prod,1)

plot(diff_prod)

# calculate autocorrelation
acf(diff_prod, pl=TRUE)

# Question 3

# Representation before and after 
plot(cbind(prod,diff_prod)) 

## Part II ## 

# Question 4 

# calculate autocorrelation
acf(as.numeric(diff_prod), pl=TRUE)
# Interpretation : q_max = 2
q_max <- 2

# calculate partial autocorrelation
pacf(as.numeric(diff_prod), pl=TRUE)
# Interpretation : p_max = 3
p_max <- 3

# Matrix of AICs and BICs
mat <- matrix (NA, nrow=p_max+1, ncol=q_max+1) # empty matrix 
rownames(mat) <- paste0("p=",0:p_max) 
colnames(mat) <- paste0("q=",0:q_max) 
AICs <- mat # AIC matrix
BICs <- mat # BIC matrix
pqs <- expand.grid(0:p_max, 0:q_max)
for (row in 1:dim(pqs)[1]){
  p <- pqs[row, 1] 
  q <- pqs[row, 2] 
  estim <- try(arima(diff_prod, c(p, 0, q), include.mean = F)) # try to estimate the ARIMA
  AICs[p+1,q+1] <- if (class(estim)=="try-error") NA else estim$aic
  BICs[p+1,q+1] <- if (class(estim)=="try-error") NA else BIC(estim) 
}

# display AICs
AICs
AICs==min(AICs)
# display BICs
BICs 
BICs==min(BICs)

# Interpretation: we choose ARMA(0,2)

arma02 <- arima(diff_prod, c(0, 0, 2), include.mean=F)
arma02 # Execute to get the estimated coef ma1 and ma2

# Function Q_tests for testing th autocorrelation of residuals
Qtests <- function(series, k, fitdf=0) {
  aux <- function(l){
    pval <- if (l<=fitdf) NA else Box.test(series, lag=l, type="Ljung", fitdf=fitdf)$p.value
    return (c("lag"=l, "pval"=pval))
  } 
  pvals <- apply(matrix(1:k), 1, FUN=aux)
  return (t(pvals))
}

Qtests(arma02$residuals, 24, fitdf=5)
# Les resultats sont bizarres l?? 

# Function adj_r2 for computing the adjusted R square 
adj_r2 <- function(model){
  ss_res <- sum(model$residuals^2) # sum of squared residuals
  p <- model$arma[1] 
  q <- model$arma[2]
  ss_tot <- sum(diff_prod[-c(1:max(p, q))]^2) 
  n <- model$nobs-max(p, q) 
  adj_r2 <- 1-(ss_res/(n-p-q-1)) / (ss_tot/(n-1)) #ajusted R square
  return (adj_r2)
}
adj_r2(arma02)

# Question 5
arima012 <- arima(prod, c(0, 1, 2), include.mean=F)
arima012

## Part III ##

# Question 7 

tsdiag(arma02)
qqnorm(arma02$residuals)
plot(density(arma02$residuals, lwd=0.5), xlim=c(-10,10), main="Density of residuals")
mu <- mean(arma02$residuals)
sigma <- sd(arma02$residuals)
x <- seq(-10,10)
y <- dnorm(x,mu,sigma)
lines(x, y, lwd=0.5, col="blue")

# Question 8 

arma02$coef
phi_1 <- as.numeric(arma02$coef[1])
phi_2 <- as.numeric(arma02$coef[2])
sigma2 <- as.numeric(arma02$sigma )
phi_1
phi_2
sigma2

# Prediction 

XT1 = predict(arma02, n.ahead=2)$pred[1]
XT2 = predict(arma02, n.ahead=2)$pred[2]
XT1
XT2

# Prediction for the serie prod
library(forecast)
arima012 <- arima(diff_prod, c(0, 1, 2), include.mean=F)
arima012
fore = forecast(arima012, h=5, level=95)
par(mfrow=c(1,1))
plot(fore, xlim=c(2020, 2024), col=1, fcol=2, shaded=TRUE, xlab="Time" , ylab="Value", main="Forecast for the serie prod")


require(ellipse)
require(ellipsis)
require(car)
library(ellipse)

arma = arima0(diff_prod, order=c(0, 1, 2))
Sigma <- matrix(c(sigma2, phi_1*sigma2, phi_1*sigma2, (phi_1)^2 * sigma2 + sigma2), ncol=2)
inv_Sigma <- solve(Sigma)
plot(XT1, XT2, xlim=c(-10,10), ylim=c(-10,10), xlab="Forecast for X_{T+1}", ylab =" Forecast on X_{T+2}", main ="95% bivariate confidence region")
lines(ellipse(Sigma, centre=c(XT1,XT2)), type="l" , col="red", xlab="Xt+1", ylab="Xt+2",main="Confidence ellipse for (Xt+1,Xt+2)")
abline(h=XT1,v=XT2)




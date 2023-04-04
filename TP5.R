path <- "C:/Users/csaurel/Desktop/THESE/Cours_Ensae/STL"
setwd(path)

# Q1 ####
datafile <- "data_tp5.csv"
data <- read.csv(datafile, sep=";")

# Q2 ####
require(zoo)
dates_char <- as.character(data$dates)
dates_char[1] #
tail(dates_char,1) #
dates <- as.yearmon(seq(from=1986+2/12, to=2007+3/12, by=1/12)) #
spread <- zoo(data$spread, order.by=dates)
dspread <- diff(spread,1)
plot(cbind(spread,dspread))
#
#

# Q3 ####
summary(lm(spread ~ dates))
#
install.packages("fUnitRoots")#tests de racine unitaire plus modulables
library(fUnitRoots)
# require(fUnitRoots) 
adf <- adfTest(spread, lag=0, type="ct") #
adf
#
Qtests <- function(series, k, fitdf=0) {
  pvals <- apply(matrix(1:k), 1, FUN=function(l) {
    pval <- if (l<=fitdf) NA else Box.test(series, lag=l, type="Ljung-Box", fitdf=fitdf)$p.value
    return(c("lag"=l,"pval"=pval))
  })
  return(t(pvals))
}
?adfTest
str(adf)
Qtests(adf@test$lm$residuals, 24, fitdf = length(adf@test$lm$coefficients))
#
#
series <- spread; kmax <- 24; adftype="ct"
adfTest_valid <- function(series, kmax, adftype){
  k <- 0
  noautocorr <- 0
  while (noautocorr==0){
    cat(paste0("ADF with ",k," lags: residuals OK? "))
    adf <- adfTest(series, lags=k, type=adftype)
    pvals <- Qtests(adf@test$lm$residuals, 24, fitdf = length(adf@test$lm$coefficients))[,2]
    if (sum(pvals<0.05,na.rm=T)==0) {
      noautocorr <- 1; cat("OK \n")
    } else cat("nope \n")
    k <- k+1
  }
  return(adf)
}
adf <- adfTest_valid(spread,24,adftype="ct")
#
Qtests(adf@test$lm$residuals, 24, fitdf = length(adf@test$lm$coefficients))
#
adf
#
summary(lm(dspread ~ dates[-1]))
#
adf <- adfTest_valid(dspread,24,"nc")
#
Qtests(adf@test$lm$residuals, 24, fitdf = length(adf@test$lm$coefficients))
adf
#

# Q4 ####
par(mfrow=c(1,2))
pacf(dspread,24);acf(dspread,24) #on regarde jusqu'? deux ans de retard
#
# 
pmax=3;qmax=3

# Q5 ####
pqs <- expand.grid(0:pmax,0:qmax) #combinaisons possibles de p<=p* et q<=q*
mat <- matrix(NA, nrow=pmax+1, ncol=pmax+1)
rownames(mat) <- paste0("p=",0:pmax) #renomme les lignes
colnames(mat) <- paste0("q=",0:pmax) #renomme les colonnes
AICs <- mat #matrice ou assigner les AIC
BICs <- mat #matrice ou assigner les BIC
for (row in 1:dim(pqs)[1]){
  p <- pqs[row,1]
  q <- pqs[row,2]
  estim <- try(arima(dspread,c(p,0,q), include.mean=F)) #tente d'estimer l'ARIMA
  AICs[p+1,q+1] <- if (class(estim)=="try-error") NA else estim$aic
  BICs[p+1,q+1] <- if (class(estim)=="try-error") NA else BIC(estim)
}
AICs
BICs
AICs==min(AICs)
#
BICs==min(BICs)
#
arima310 <- arima(spread,c(3,1,0),include.mean=F)
arima011 <- arima(spread,c(0,1,1),include.mean=F)

# Q6 ####
arima310
# 
arima011
# 
# Q7 ####
Qtests(arima310$residuals, 24, fitdf=3)
#
Qtests(arima011$residuals, 24, fitdf=1)
#
#

# Q8 ####
# 


# Q9 ####
adj_r2 <- function(model){
  ss_res <- sum(model$residuals^2)
  ss_tot <- sum(dspread[-c(1:max(p,q))]^2)
  p <- model$arma[1]
  q <- model$arma[2]
  n <- model$nobs-max(p,q)
  adj_r2 <- 1-(ss_res/(n-p-q-1))/(ss_tot/(n-1))
  return(adj_r2)
}
adj_r2(arima310)
adj_r2(arima011)
# 

dev.off()
plot(arima310$residuals)
axis(side=1,2001+11/12)
# 

# Q10 ####
#.
# 
breakpoint <- c(1:length(dates))[dates==1995]
ap1995 <- c(rep(0,breakpoint-1),rep(1,length(dates)-breakpoint+1))
arima(spread,c(3,1,0),xreg=ap1995,include.mean=F)
plot(dspread)
#
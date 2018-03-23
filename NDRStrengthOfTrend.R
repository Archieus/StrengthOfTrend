library(quantmod)
library(DSTrading)
Sys.setenv(TZ = "EST5EDT")

y <- read.csv('http://www.alphavantage.co/query?function=TIME_SERIES_DAILY_ADJUSTED&symbol=SPY&outputsize=full&apikey=Y474&datatype=csv')

y.new <- cbind(y[,c(2:5,7,6)])
rownames(y.new) <-y[,1]
SPY <- as.xts(y.new)

### STRENGTH OF TREND CONCEPT ###

###1. ADX(42)###
ADX42 <- na.omit(ADX(SPY[,2:4], 42))$ADX
ADX.Chg <- na.omit(runSum(ADX42 - lag(ADX42,1)))

#Peaks <- findPeaks(ADX42)
#Valleys <- findValleys(ADX42)

ADX.Signal <- matrix(0, nrow = nrow(ADX.Chg), ncol = 1)

for(i in 1:nrow(ADX.Chg)){
  if(ADX.Chg[i] >= .5){
    ADX.Signal[i] <- 1
  }else {
    if(ADX.Chg[i] <= -1){
  ADX.Signal[i] <- -1
    }
  }
}

ADXSig.df <- as.data.frame(ADX.Signal)
row.names(ADXSig.df) <- index(ADX.Chg)

ADXSig.x <- as.xts(ADXSig.df)

###2. Efficiency Ratio###
ER <- na.omit(abs(ROC(SPY$close,63)))/na.omit(runSum(abs(ROC(SPY$close,1))),53)
ER.SMA <- na.omit(SMA(ER,200))

ER.Data <- na.omit(cbind(ER, ER.SMA))

ER.Signal <- matrix(0, nrow = nrow(ER.Data), ncol = 1)

for(i in 1:nrow(ER.Data)){
  if(ER.Data$close[i] >= ER.Data$SMA[i]*1.25){
    ER.Signal[i] <- 1
  }else {
    if(ER.Data$close[i] <= ER.Data$SMA[i]*.5){
    ER.Signal[i] <- -1  
    }
  }
}

ERSig.df <- as.data.frame(ER.Signal)
rownames(ERSig.df) <- index(ER.Data)
ERSig.x <- as.xts(ERSig.df)

###3. R-Squared Average Correlation Coefficients###
SPY.d <- cbind(SPY, "Days" = 1:nrow(SPY))

R2.21 <- rollSFM(log(SPY.d[,4]), SPY.d$Days, 21)$r.squared
R2.63 <- rollSFM(log(SPY.d[,4]), SPY.d$Days, 63)$r.squared
R2.252 <- rollSFM(log(SPY.d[,4]), SPY.d$Days, 252)$r.squared
R2.All <- na.omit(cbind(R2.21,R2.63,R2.252))
R2.Mean <- as.data.frame(rowMeans(R2.All))
row.names(R2.Mean) <- index(R2.All)

R2.Signal <- ifelse(R2.Mean > .6, 1, -1)
R2Sig.x <- as.xts(R2.Signal)

###4. Bandpass Filter (Empirical Mode Decomposition)###

###Bandpass Empirical Mode Decomposition using DSTrading###
#Dark Blue = pctB
#Light Blue = Momentum (Indicates Trend Direction)
#Black = Trend (Uptrend Trend > Peak, Down Trend < Valley, Rangebound < Peak, > Valley)
#Red = Peak
#Green = Valley

###NDR Uses a 42-day lookback and a 504-SMA for the BandPass Indicator###
###It appears this version of EMD is calculated slightly differently than Ehlers EMD###
SPY.BP <- KEMD(SPY[,4],.7,30 ,.25, "SMA")
BP.SMA <- SMA(SPY.BP$trend,360)
BPData <- na.omit(cbind(SPY.BP$trend,BP.SMA))

BP.Signal <- matrix(0, nrow = nrow(BPData), ncol = 1)

for(i in 1:nrow(BPData)){
  if(BPData$trend[i] > BPData$SMA[i]){
    BP.Signal[i] <- 1
  }else {
    if(BPData$trend[i] < BPData$SMA[i]){
      BP.Signal[i] <- -1
    }
  }
}

BPSig.df <- as.data.frame(BP.Signal)
rownames(BPSig.df) <- index(BPData)
BPSig.x <- as.xts(BPSig.df)

NDR.Comp <- na.omit(cbind(ADXSig.x,ERSig.x,R2Sig.x,BPSig.x))
names(NDR.Comp) <- c("ADX", "Eff Ratio", "R2-Mean", "BandPass")
NDR.EQWt <- as.data.frame(rowMeans(NDR.Comp))
rownames(NDR.EQWt) <- index(NDR.Comp)
NDRComp.x <- as.xts(NDR.EQWt)

layout(1:3)
chart_Series(SPY[,4]['2017:/'], name = "SPY Daily")
chart_Series(NDRComp.x['2017:/'],name = "NDR Strenght of Trend Composite")
chart_Series((SPY.BP['2017:/'])$trend, name = "Empirical Mode Decomposition", TA = "add_TA(BP.SMA['2017:/'], on = 1)")

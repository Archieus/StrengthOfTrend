library(quantmod)
library(DSTrading)
library(tseries)
Sys.setenv(TZ = "EST5EDT")

y <- read.csv('http://www.alphavantage.co/query?function=TIME_SERIES_DAILY_ADJUSTED&symbol=SPY&outputsize=full&apikey=Y474&datatype=csv')

y.new <- cbind(y[,c(2:5,7,6)])
rownames(y.new) <-y[,1]
SPY <- as.xts(y.new)

PValue <- suppressWarnings(as.data.frame(rollapply(last(as.ts(SPY[,6]),300), 30, function(u) adf.test(u,k=1)$p.value)))

### STRENGTH OF TREND CONCEPT ###

#TREND = 1
#CYCLE (MEAN REVERSION) = -1
#NEUTRAL = 0

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
###bandFraction = .25 will allow you to trade when is cycle mode (Swing Trades)
###and keep you out of the market when in trend mode (Momentum Trading)

SPY.BP <- KEMD(SPY[,4], delta =.7,n = 30 ,bandFraction =.25, maType ="SMA")
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

NDR.SM10 <- EMA(NDRComp.x,10, wilder = TRUE) #EMA = Smoothing

# layout(1:3)
# chart_Series(SPY[,4]['2017:/'], name = "SPY Daily")
# chart_Series(NDRComp.x['2017:/'],name = "NDR Strength of Trend Composite (w/10-day Smoothing)", TA = "add_TA(NDR.SM10, on = 1)")
# chart_Series(SPY.BP[,1]['2017:/'],
#              name = "KEMD, > Peak(Red)= UpTrend, < Valley(Green) = DownTrend, < Red > Green = Range",
#              TA = c("add_TA(SPY.BP$peak, on = 1, col = 2)", 
#                     "add_TA(SPY.BP$valley, on = 1, col = 3)", 
#                     "add_TA(SPY.BP$momentum, col = 4)"))

##Revised to include Momentum for Trend Direction##
layout(1:3)
chart_Series(SPY[,4]['2017:/'], name = "SPY Daily")
chart_Series(NDRComp.x['2017:/'],name = "NDR Strength of Trend Composite (w/10-day Smoothing)", TA = "add_TA(NDR.SM10, on = 1)")
#chart_Series(EMA(NDRComp.x['2017:/'],10, wilder = TRUE),name = "10-Day Smoothing NDR Strength of Trend Composite")
chart_Series((SPY.BP['2017:/'])$trend, name = "Empirical Mode Decomposition",
             TA = c("add_TA(BP.SMA, on = 1)", "add_TA(SPY.BP$momentum, col = 4)"))

View(tail(SPY.BP))

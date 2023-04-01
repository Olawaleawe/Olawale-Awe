install.packages("quantmod")
library(quantmod)
# getSymbols(): to get data
getSymbols("AAPL")
# OHLCVA data
head(AAPL,n=3)
# Get OHLC data
price<-OHLC(AAPL)
head(price, n=3)
#######
install.packages("dygraphs")
library(dygraphs)
##########
getSymbols("AAPL")
getSymbols("SPY")
dygraph(OHLC(AAPL))
graph<-dygraph(Cl(SPY), main = "SPY") 
dyShading(graph, from="2007-08-09", 
          to="2011-05-11", color="#FFE6E6")
graph<-dygraph(OHLC(AAPL), main = "AAPL") 
graph<-dyEvent(graph,"2007-6-29",
               "iphone", labelLoc = "bottom") 
graph<-dyEvent(graph,"2010-5-6", 
               "Flash Crash", labelLoc = "bottom") 
graph<-dyEvent(graph,"2014-6-6", 
               "Split", labelLoc = "bottom") 
dyEvent(graph,"2011-10-5",
        "Jobs", labelLoc = "bottom") 
AAPL <- tail(AAPL, n=30)
graph<-dygraph(OHLC(AAPL))
dyCandlestick(graph)

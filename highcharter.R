library(tidyverse)
library(timetk)
library(kableExtra)
library(highcharter)

symbols <- 
  c("SPY","EFA", "IJS", "EEM","AGG")

prices <- 
  getSymbols(symbols, 
             src = 'yahoo', 
             from = "2013-01-01",
             to = "2017-12-31",
             auto.assign = TRUE, 
             warnings = FALSE) %>% 
  map(~Ad(get(.))) %>%
  reduce(merge) %>% 
  `colnames<-`(symbols)

prices_monthly <- 
  to.monthly(prices, 
             indexAt = "last", 
             OHLC = FALSE)

asset_returns_xts <- 
  na.omit(Return.calculate(prices_monthly, 
                           method = "log"))

asset_returns_xts <- asset_returns_xts * 100


asset_returns_long <-  
  prices %>% 
  to.monthly(indexAt = "last", 
             OHLC = FALSE) %>% 
  tk_tbl(preserve_index = TRUE, 
         rename_index = "date") %>%
  gather(asset, returns, -date) %>% 
  group_by(asset) %>%  
  mutate(returns = 
           (log(returns) - log(lag(returns))) *100
  ) %>% 
  na.omit()

highchart(type = "stock") %>% 
  hc_add_series(asset_returns_xts$SPY, type = "line")

highchart(type = "stock") %>% 
  hc_add_series(asset_returns_xts$SPY, 
                type = "line",
                color = "green")

highchart(type = "stock") %>% 
  hc_add_series(asset_returns_xts$SPY, type = "column")

highchart(type = "stock") %>% 
  hc_add_series(asset_returns_xts$SPY, type = "scatter") %>% 
  hc_add_series(asset_returns_xts$EFA, type = "scatter")
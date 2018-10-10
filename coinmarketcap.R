library(coinmarketcapr)
library(formatR)
library(yaml)
library(googleVis)
library(knitr)
library(lattice)
op <- options(gvis.plot.tag="chart")
top_cc <- get_marketcap_ticker_all()
top_dd <- get_global_marketcap()
plot_top_5_currencies()
kable(head(top_cc))

top_cc <- get_marketcap_ticker_all()
kable(head(top_cc))

for(i in c(4:ncol(top_cc))) {
  top_cc[,i] <- as.double(top_cc[,i])
}

mark <- plot(gvisColumnChart(top_cc[1:15, ], "name", "market_cap_usd", options = list(title = "Market Cap Of Leading Cryptocurrencies", legend = "left")))
plot(mark)

var <- gvisColumnChart(top_cc[1:15, ], "name", c("percent_change_1h", "percent_change_24h", 
                                                 "percent_change_7d"), options = list(title = "% change of prices", legend = "top"))
plot(var)

vol <- gvisColumnChart(top_cc[1:15, ], "name", c("X24h_volume_usd"), options = list(title = "Volume", 
                                                                                    legend = "top"))
plot(vol)

#First import the historical data using crypto library. Crypto library in R retrieves the historical market data provided by ‘Cryptocurrency Market Capitalizations’ https://coinmarketcap.com
library(httr)
library(RCurl)
library(crypto)
library(knitr)
library(doSNOW)



-------------------------------------------------
# Market Cap Trend
# 
# Import the historical data of top 5 currencies and plot the trend of their market ca.
# 
# listCoins function returns the list of all the coins and url to download their history data.
# getcoins function is used to get historical data of the coins.

  
  
  #automate excel listing analysis
  
all_coins <- crypto_history(start_date = '20180301')

 
#some_coins <- crypto_history(coin = "DENT")
#binance_coins <- getCoins(
 #recent listings from binance  
coin = c("LOOM",
         "XRP",                    "BCN",
                                   "REP",
                                   "ZEN",
                                   "SKY",
                                   "GNT",
                                   "CVC",
                                   "EOS",
                                   "THETA",
                                   "NXS",
                                   "ONT",
                                   "ENJ",
                                   "TRX",
                                   "ETC",
                                   "SC",
                                   "ICX",
                                   "NPXS",
                                   "KEY",
                                   "NAS",
                                   "DENT",
                                   "ARDOR",
                                   "NULS",
                                   "HOT",
                                   "POLY")

binance_listings <- all_coins[all_coins$symbol %in% coin,]
binance_listings$month <- months(binance_listings$date) 
#binance_listings <- binance_listings[which(binance_listings$date >= "2018-05-01"),]


g <- with(binance_listings, aggregate(volume, list(name = name, month = month), mean, na.rm = TRUE))
h <- with(binance_listings, aggregate(close, list(name = name, month = month), mean, na.rm = TRUE))

binance_listings <- merge(binance_listings,g, by.x = c("name","month"), by.y = c("name", "month"))
binance_listings <- merge(binance_listings,h, by.x = c("name","month"), by.y = c("name", "month"))
names(binance_listings)[15]<- "average_volume"
names(binance_listings)[16]<- "average_price"

binance_listings <- binance_listings[with(binance_listings, order(name,date)),]
max_price <- max_price[with(max_price, order(name,date)),]

max_price <- merge(binance_listings, listing_vs_price_and_volume_with_averages, by.x =  "symbol", by.y = "list")


names(max_price)[24]<- "launch_date"
names(max_price)[5] <- "reading_date"


max_price$launch_week = format(max_price$launch_date, format = "%Y %W")
max_price$launch_month = months(max_price$launch_date)


# g <- with(max_price, mean(volume[reading_date < launch_date]))
# h <- with(max_price, mean(close[reading_date < launch_date))
# 
# max_price <- merge(max_price,g, by.x = c("name","month"), by.y = c("name", "month"))
# max_price <- merge(max_price,h, by.x = c("name","month"), by.y = c("name", "month"))
# names(max_price)[15]<- "average_volume"
# names(max_price)[16]<- "average_price"

min_price1 <- with(max_price, aggregate(close, list(symbol = symbol, reading_date  < launch_date  & reading_date >= launch_date - 30), mean))
min_volume1 <- with(max_price, aggregate(volume, list(symbol = symbol, reading_date  < launch_date  & reading_date >= launch_date - 30), mean))

# do you want to include the launch date itself?

max_price1 <- with(max_price, aggregate(close, list(symbol = symbol, reading_date  > launch_date  & reading_date <= launch_date + 30), mean))
max_volume1 <- with(max_price, aggregate(volume, list(symbol = symbol, reading_date > launch_date  & reading_date <= launch_date + 30), mean))

min_price1_true <- subset(min_price1, Group.2 == TRUE)
max_price1_true <- subset(max_price1, Group.2 == TRUE)

min_volume1_true <- subset(min_volume1, Group.2 == TRUE)
max_volume1_true <- subset(max_volume1, Group.2 == TRUE)

min_price1_true$price_quotient <- max_price1_true$x / min_price1_true$x
min_price1_true$volme_quotient <- max_volume1_true$x / min_volume1_true$x


min_price1_true <- merge(min_price1_true, listing_vs_price_and_volume_with_averages[,c("list", "date")], by.x = "symbol" ,by.y = "list", all.x = TRUE) 

xyplot(1/price_quotient ~ date, data = min_price1_true, type = c("p", "r"), main = "pre vs post launch date monthly average price", xlab = "launch date", ylab = "ratio")

xyplot(1/volme_quotient ~ date, data = min_price1_true, type = c("p", "r"), main = "pre vs post launch date monthly average volume", xlab = "launch date", ylab = "ratio")

#end of listing 
---------------------------------

# summary <- aggregate(close ~ symbol + month, data = max_price, mean)
# summary$volume <- aggregate(volume ~ symbol + month, data = max_price, mean)
# 
#  
# ave_price <- merge(max_price1, min_price1, by.x = c("symbol", "Group.2"), by.y = c("symbol", "Group.2"))
# ave_price$price_quotient <- ave_price$x.y / ave_price$x.x
# ave_price <- ave_price[ave_price$Group.2 == FALSE,] 
# 
# ave_volume <- merge(max_volume1, min_volume1, by.x = c("symbol", "Group.2"), by.y = c("symbol", "Group.2"))
# ave_volume$volume_quotient <- ave_volume$x.y / ave_volume$x.x
# ave_volume <- ave_volume[ave_volume$Group.2 == FALSE,] 

#  max_price$average_price[max_price$reading_date < max_price$launch_date] / max_price$average_price[max_price$reading_date > max_price$launch_date]  
#   
# max_price$price_quotient <- max_price$x / min_price$x
# max_price$volume_quotient <- max_volume$x / min_volume$x



#binance_listings$price_ratio <- binance_listings$average_price[binance_listings$month == 'April']/binance_listings$average_price[binance_listings$month == 'July'] 
------------------------------------
  
  
 # from webpage https://analyticsprofile.com/algo-trading/cryptocurrency-data-analysis-using-r/
  
 
  
  
  
  
  
  



 
p = gvisAnnotationChart(binance_listings, idvar = "name", "date", "market", options = list(title = "Market Cap Trend",                                                                                 legend = "top"))
plot(p)

p = gvisAnnotationChart(binance_listings, idvar = "name", "date", "close_ratio", options = list(title = "Market Cap Trend",                                                                                 legend = "top"))
plot(p)

f = gvisAnnotationChart(binance_listings, idvar = "name", "date", "spread", options = list(title = "Market Cap Trend",                                                                                 legend = "top"))
plot(f)



ohlc = gvisCandlestickChart(binance_listings2[binance_listings2$name == "XRP" & binance_listings2$date < "2018-08-31" & 
                                    binance_listings2$date > "2017-01-01", ], "date", "low", "open", "close", "high", options = list(title = "OHLC chart of Bitcoin in Jan 2017", 
                                                                                                                         legend = "top", height = 600))
plot(ohlc)





library(quantmod)
coins <- c("LOOM",
           "XRP",
           "BCN",
           "REP",
           "ZEN",
           "SKY",
           "GNT",
           "CVC",
           "EOS",
           "THETA",
           "NXS",
           "ONT",
           "ENJ",
           "TRX",
           "ETC",
           "SC  ",
           "ICX",
           "NPXS",
           "KEY",
           "NAS",
           "DENT",
           "ARDOR",
           "NULS",
           "HOT",
           "POLY")

coins <- unique(binance_listings2$name)
s_date = Sys.Date() - 1
binance_listings2$market_growth <- NA
for (i in coins) {
  binance_listings2[binance_listings2$name == i, ]$market_growth = Delt(binance_listings2[binance_listings2$name == i, ]$volume, 
                                                  type = "arithmetic", k = 90)
}
c_data = binance_listings2[complete.cases(binance_listings2), ]  #complete.cases is used to get rwcords without NA
mg_3 = gvisColumnChart(c_data[c_data$date == s_date, ], "name", "market_growth", 
                       options = list(title = "Market Growth in the last 3 months", scaleType = "allfixed", 
                                      legend = "top"))
plot(mg_3)




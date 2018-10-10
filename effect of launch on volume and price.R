library(coinmarketcapr)
library(formatR)
library(yaml)
library(googleVis)
library(knitr)

#First import the historical data using crypto library. Crypto library in R retrieves the historical market data provided by ‘Cryptocurrency Market Capitalizations’ https://coinmarketcap.com
library(httr)
library(RCurl)
library(crypto)
library(knitr)
library(doSNOW)
library(lattice)

#listCoins function returns the list of all the coins and url to download their history data.
# getcoins function is used to get historical data of the coins.



#automate excel listing analysis

all_coins <- crypto_history(start_date = '20180301')


#some_coins <- crypto_history(coin = "DENT")
#binance_coins <- getCoins(
#recent listings from binance  
coin = c("LOOM",
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

binance_listings_names <-  as.data.frame(unique(binance_listings$name))
binance_listings_names <- merge(binance_listings_names, binance_listings[c], 

binance_listings$month <- months(binance_listings$date) 
#binance_listings <- binance_listings[which(binance_listings$date >= "2018-05-01"),]


g <- with(binance_listings, aggregate(volume, list(name = name, month = month), mean, na.rm = TRUE))
h <- with(binance_listings, aggregate(close, list(name = name, month = month), mean, na.rm = TRUE))

binance_listings <- merge(binance_listings,g, by.x = c("name","month"), by.y = c("name", "month"))
binance_listings <- merge(binance_listings,h, by.x = c("name","month"), by.y = c("name", "month"))
names(binance_listings)[15]<- "average_volume"
names(binance_listings)[16]<- "average_price"

#binance_listings <- binance_listings[with(binance_listings, order(name,date)),]
#max_price <- max_price[with(max_price, order(name,date)),]

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

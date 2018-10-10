
library(coinmarketcapr)
library(formatR)
library(yaml)
library(googleVis)
library(knitr)
library(xts) # typte no when prompted for answer
library(httr)
library(RCurl)
library(knitr)
library(doSNOW)
library(readxl)



-------------------------------------------------
  # Market Cap Trend
  # 
  #  
  # listCoins function returns the list of all the coins and url to download their history data.
  # getcoins function is used to get historical data of the coins.
  #crypto_history is used to get historical data of all coins
  
data1 <- getCoins()  
#data1 <- crypto_history(start_date = '20160301')
all_coins <- data1
#will provide data frame of current prices
#data2 <- crypto_prices()
#all_coins_01032018 <- crypto_history(start_date = '20180301')
#all_coins <- all_coins_01032018
all_coins$month = format(all_coins$date, format = "%Y %b")


# calculate mean volume per coin in latest month
all_coins1 <- subset(all_coins, month == "2018 Jul")

g <- with(all_coins1, aggregate(volume, list(name = name), mean, na.rm = TRUE))
all_coins1 <- merge(all_coins1,g, by.x = c("name"), by.y = c("name"))

names(all_coins1)[15]<- "average_volume"
dim(all_coins1)

#calculate mean volume per day per coin
all_coins1$average_volume <- all_coins1$average_volume/31

#first criteria daily volume below 50,000
all_coins2 <- subset(all_coins1, average_volume < 50000)

dim(all_coins2)
#[1] 42436    15
dim(all_coins1)
#[1] 49119    15

names <- unique(all_coins2$name)

# all coins with average daily volume < 50000 are in names
#only takes coins with small volume
all_coins3 <- all_coins[all_coins$name %in% names,]
max(all_coins3$date)
#[1] "2018-08-13"
all_coins4 <- subset(all_coins3, date == "2018-08-13")


all_coins5 <- subset(all_coins4, market < 10000000)
# all coins with market cap < 10million and small volumne in names2
names2 <- unique(all_coins5$name)

#coinmarketcap_tokendata_volume_raised <- merge(coinmarketcap_tokendata_volume, tokendata, by.x = "name" , by.y = "Name")

#merge to filter by raised
#calculate  % change from ico from all_coins
#calculate all time hight ATH
#calculate % change from all time hight
#launch manually
#sale date (ico offering dat)

#calculate highest price all times 
#data1 <- crypto_history()
g <- with(data1, aggregate(close, list(name = name), max, na.rm = TRUE))
all_coins6 <- merge(all_coins5,g, by.x = c("name"), by.y = c("name"))

names(all_coins6)[15]<- "ATH-price"

all_coins6$athpriceratio <- all_coins6$close / all_coins6$`ATH-price`
final_result <- all_coins6[all_coins6$athpriceratio < 0.20,]


#downloaded token.io from Google drive
#filter by coin which has raised > 20 m
ICO_Analysis_August_sheet1 <- read_excel("~/Desktop/ICO Analysis August-Hagen.xlsx",
sheet = "Sheet1")

ICO_Analysis_August_sheet2 <- read_excel("~/Desktop/ICO Analysis August-Hagen.xlsx",
                                         sheet = "Sheet2")

tokendata <- rbind(ICO_Analysis_August_sheet1, ICO_Analysis_August_sheet2)
tokendata2 <- tokendata[tokendata$`USD Raised` > 20000000,]

##############################

#change names in tokendata somehow to match
##!!!!!!!!!!!!!!!here is the problem ->

final_result$name <- toupper(final_result$name)
final_result$symbol <- toupper(final_result$symbol)
final_result$slug <- toupper(final_result$slug)
tokendata2$Name <- toupper(tokendata2$Name)

tokendata2$Name  <- sub(".*?(\\d+(?:\\.\\d+)?).*", "\\1", tokendata2$Name)
final_result$name <- sub(".*?(\\d+(?:\\.\\d+)?).*", "\\1", final_result$name)
final_result$symbol <- sub(".*?(\\d+(?:\\.\\d+)?).*", "\\1", final_result$symbol)
final_result$slug <- sub(".*?(\\d+(?:\\.\\d+)?).*", "\\1", final_result$slug)



trim.leading <- function (x)  sub("^\\s+", "", x)
trim.trailing <- function (x) sub("\\s+$", "", x)
final_result$name <- trim.leading(final_result$name)
final_result$name <- trim.trailing(final_result$name)
final_result$symbol <- trim.leading(final_result$symbol)
final_result$symbol <- trim.trailing(final_result$symbol)


tokendata2$Name <- trim.leading(tokendata2$Name)
tokendata2$Name  <- trim.trailing(tokendata2$Name)
#####################################


final_result2 <- merge(final_result,tokendata2, by.x = "symbol", by.y = "Symbol")
final_result2 <- unique(final_result2)
final_result4 <- merge(final_result,tokendata2, by.x = "name", by.y = "Name")
final_result4 <- unique(final_result4)
final_result3 <- merge(final_result,tokendata2, by.x = "slug", by.y = "Name")
final_result3 <- unique(final_result3)


#test quality of name column and symbol - do we miss a lot of names because sources are different
#all_coins_names <- subset(all_coins, date == "2018-08-13")
#final_result3 <- merge(all_coins_names,tokendata2, by.x = "symbol", by.y = "Symbol")
#final_result33 <- merge(all_coins_names,tokendata2, by.x = "name", by.y = "Name")
#final_result333 <- merge(final_result3,final_result33, all = TRUE)
#final_result333 <- unique(final_result333)

final_result4 <- final_result4[,c(3,1,2,4:24)]
final_result5 <- merge(final_result2,final_result4, all = TRUE)
final_result5 <- unique(final_result5)
final_result6 <- merge(final_result5,final_result3, all = TRUE)

#final_result5 <- rbind(final_result2,final_result4)
#https://www.coinschedule.com/icos.html
#sapply(tokendata2, function(x) length(unique(x)))
#length(unique(all_coins_names$name))
#sapply(final_result2, function(x) length(unique(x)))
#sapply(data1,function(x) sum(is.na(x)))

final_result6$ICOpriceratio <- final_result6$`Current Price` / final_result6$`Sale Price`
final_result7 <- final_result5[final_result6$ICOpriceratio < 0.20,]
final_result8 <- final_result7[complete.cases(final_result7[,1]),]
final_result8 <- unique(final_result8)

#listing date? criteria for circulating /total supply?

library(crypto)
library(dplyr)

crypto_prices <- crypto_prices()
crypto_prices2 <- crypto_prices

crypto_prices$supplyratio <- crypto_prices$available_supply / crypto_prices$total_supply

crypto_prices$name <- toupper(crypto_prices$name)
crypto_prices$id <- toupper(crypto_prices$id)



final_result9 <- merge(final_result8, crypto_prices, by.x = "symbol" , by.y = "symbol", all.x = TRUE)
final_result9 <- unique(final_result9)

write.csv(final_result9, file = "ICO_analyis_final_result.csv")


final_result9$ICOpriceratio <- final_result9$`Current Price` / final_result9$`Sale Price`
final_result10 <- final_result9[final_result9$ICOpriceratio < 0.20,]
#final_result10 <- merge(final_result10, all_coins2[c(3,15)], by.x = "symbol", by.y = "symbol", all.x = TRUE)

#import from google docs including listings from coinmarketcap 
ICO_Analysis_August_Hagen <- read_excel("~/Downloads/ICO Analysis August-Hagen.xlsx")
View(ICO_Analysis_August_Hagen)
ICO_Analysis_August_Hagen <- ICO_Analysis_August_Hagen[ICO_Analysis_August_Hagen$ico_priceratio < 0.20,]
summary(ICO_Analysis_August_Hagen)
ICO_Analysis_August_29082018 <- merge(ICO_Analysis_August, all_coins2, all.x = TRUE)
write.csv(ICO_Analysis_August_Hagen, file = "ICO_Analysis_29082018.csv")


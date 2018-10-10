library(readxl)    
read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}


library(readxl)
listing_vs_price_and_volume_with_averages <- read_excel("listings/listing vs price and volume with averages.xlsx")
mysheets <- read_excel_allsheets("listings/listing vs price and volume with averages.xlsx")
mysheets$SKY$Date <- as.Date(mysheets$SKY$Date, format = "%b %d ,%Y")

"NAS"   "KEY"   "NPXS"  "SC"    "ICX"   "ETC"   "TRX"   "ONT"   "ENJ"   "NXS"   "THETA" "CVC"  
 "ZEN"   "REP"   "BCN"   "XRP"   "EOS"   "LOOM"  "GNT"   "SKY"

price_means_NAS <- lapply(mysheets$NAS[mysheets$NAS$Date > "2018-06-28" & mysheets$NAS$Date <= "2018-07-28",], mean) 
price_means_NAS <- lapply(with(mysheetsNAS, mean) 
price_means <- mysheets$NAS[mysheets$NAS$Date > "2018-06-28" & mysheets$NAS$Date <= "2018-07-28"], mean

list2env(mysheets,.GlobalEnv)
#sapply(mysheets$NAS[mysheets$NAS$Date > "2018-06-28",], mean)
#data <- mean(mysheets$NAS[mysheets$NAS$Date > "2018-06-28" & mysheets$NAS$Date <= "2018-07-28",])


plot(listing_vs_price_and_volume_with_averages$date, listing_vs_price_and_volume_with_averages$difference, main = "pre vs post launch price difference", type = "l", xlab = "launch date", ylab = "difference [Ethereum]")
abline(h=0, col = 'purple')

plot(difference~date, data = listing_vs_price_and_volume_with_averages)
abline(coef = coef.Model)
abline(model)
lines(listing_vs_price_and_volume_with_averages$difference, fitted(model, col = "blue"))
model <- lm(difference~date, data = listing_vs_price_and_volume_with_averages)
lines(listing_vs_price_and_volume_with_averages$difference, fitted(model), col = "blue")
lines(listing_vs_price_and_volume_with_averages$date, fitted(model), col = "blue")

install.packages("lattice")
library(lattice)
xyplot(difference~date, data = listing_vs_price_and_volume_with_averages, type = c("p", "r"))
xyplot(`volume ratio`~date, data = listing_vs_price_and_volume_with_averages, type = c("p", "r"))
abline(lm(difference ~ date, data  = listing_vs_price_and_volume_with_averages))

listing_vs_price_and_volume_with_averages$date <- as.Date(listing_vs_price_and_volume_with_averages$`listing date`, format("%b %d ,%Y"))


plot(listing_vs_price_and_volume_with_averages$date, listing_vs_price_and_volume_with_averages$`volume difference`, main = "pre vs post launch volume difference", type = "p", xlab = "launch date", ylab = "volume difference [Ethereum]")
xyplot(difference~date, data = listing_vs_price_and_volume_with_averages, type = c("p", "r"))
xyplot(ratio~date, data = listing_vs_price_and_volume_with_averages, type = c("p", "r"),main = "pre vs post launch price ratio", xlab = "launch date", ylab = "ratio")


listing_vs_price_and_volume_with_averages <- read_excel("listings/listing vs price and volume with averages.xlsx",
sheet = "list of differences (2)")

listing_vs_price_and_volume_with_averages$date <- as.Date(listing_vs_price_and_volume_with_averages$`listing date`, format("%b %d ,%Y"))
xyplot(`volume ratio` ~date, data = listing_vs_price_and_volume_with_averages, type = c("p", "r"), main = "pre vs post launch date monthly average volume", xlab = "launch date", ylab = "ratio")
xyplot(`price ratio` ~date, data = listing_vs_price_and_volume_with_averages, type = c("p", "r"),main = "pre vs post launch date monthly average price", xlab = "launch date", ylab = "ratio")

xyplot(`volume difference`/1e9 ~date, data = listing_vs_price_and_volume_with_averages, type = c("p", "r"), main = "pre vs post launch date monthly average volume", xlab = "launch date", ylab = "difference [*10e9]")
xyplot(`price difference` ~date, data = listing_vs_price_and_volume_with_averages, type = c("p", "r"), main = "pre vs post launch date monthly average price", xlab = "launch date", ylab = "difference")

plot(x,y/1e3, ylab="y /10^3")

plot.ts(listing_vs_price_and_volume_with_averages[c(3,5,6,7)])
        
        , listing_vs_price_and_volume_with_averages$`listing date`)

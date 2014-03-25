source_github("https://raw2.github.com/rockclimber112358/practicum/master/functions.R")
#source("/u/st/fl/jbrownin/MATH598_Statistics_Practicum/R_Code/functions.R")

options(digits.secs=6)

###############################################################################
# Raw processing: read in files, restructure
###############################################################################

setwd("/media/storage/Professional Files/Mines/MATH 598- Statistics Practicum/Data/")
raw = read.csv(file="20131104.CLZ3.log")
raw = rbind(raw, read.csv(file="20131105.CLZ3.log") )
raw = rbind(raw, read.csv(file="20131106.CLZ3.log") )
raw = rbind(raw, read.csv(file="20131107.CLZ3.log") )
raw = rbind(raw, read.csv(file="20131108.CLZ3.log") )

#raw = read.csv(file="20131104.CLZ3.log")
#raw = rbind(raw, read.csv(file="20131105.CLZ3.log") )
#raw = rbind(raw, read.csv(file="20131106.CLZ3.log") )
#raw = rbind(raw, read.csv(file="20131107.CLZ3.log") )
#raw = rbind(raw, read.csv(file="20131108.CLZ3.log") )

raw$Time = as.POSIXct(strptime(x=raw$Time, format="%Y%m%d %H:%M:%OS"))
#Convert time to a numeric vector (i.e. seconds after 2013-11-04 00:00:00):
raw$Time = as.numeric( raw$Time - as.POSIXct("2013-11-04") )

#Data frame with prices:
price = raw[raw$RestingSide=="",]
price = price[,c(1:31,35:37)]

#Data frame with transactions:
orders = raw[raw$RestingSide!="",]
orders = orders[,c(1,32:34)]
orders = orders[orders$RestingSide!="null",]
write.csv( orders, file="orders.csv", row.names=F )

rm(raw); gc()

price$PriceDiff1SecAhead = (price$MicroPrice1SecAhead - price$MicroPrice)*1000
#price$PriceRatio1SecAhead = price$MicroPrice1SecAhead / price$MicroPrice - 1
price$PriceDiff60SecAhead = (price$MicroPrice60SecAhead - price$MicroPrice)*1000
#price$PriceRatio60SecAhead = price$MicroPrice60SecAhead / price$MicroPrice - 1
price$day = floor( price$Time/(24*60*60) ) + 1
price$Outcry = as.numeric( price$Time %% (24*60*60) > 6.75*60*60 & price$Time %% (24*60*60) < 13.5*60*60 )
price$Diff = c(0,as.numeric(price$BidQuantity1[2:nrow(price)-1] !=price$BidQuantity1[2:nrow(price)]|
                            price$BidPrice1[2:nrow(price)-1] !=price$BidPrice1[2:nrow(price)]|
                            price$OfferQuantity1[2:nrow(price)-1] !=price$OfferQuantity1[2:nrow(price)]|
                            price$OfferPrice1[2:nrow(price)-1] !=price$OfferPrice1[2:nrow(price)]))
price$Diff[is.na(price$Diff)] = 0
price$Weight = 0 #Placeholder, assigned later

###############################################################################
# Define adjusted microprice(s)
###############################################################################

#Define quantities for bid, bid-.01, bid-.02, bid-.03, bid-.04 and offer, offer+.01, offer+.02, offer+.03, offer+.04
price$BidHigh1Cnt = price$BidQuantity1
price$BidHigh2Cnt = apply(price[,1:5*3-1]*(abs(price[,1:5*3+1]-price$BidPrice1+.01)<.005),1,sum, na.rm=T)
price$BidHigh3Cnt = apply(price[,1:5*3-1]*(abs(price[,1:5*3+1]-price$BidPrice1+.02)<.005),1,sum, na.rm=T)
price$BidHigh4Cnt = apply(price[,1:5*3-1]*(abs(price[,1:5*3+1]-price$BidPrice1+.03)<.005),1,sum, na.rm=T)
price$BidHigh5Cnt = apply(price[,1:5*3-1]*(abs(price[,1:5*3+1]-price$BidPrice1+.04)<.005),1,sum, na.rm=T)
price$OfferLow1Cnt = price$OfferQuantity1
price$OfferLow2Cnt = apply(price[,6:10*3-1]*(abs(price[,6:10*3+1]-price$OfferPrice1-.01)<.005),1,sum, na.rm=T)
price$OfferLow3Cnt = apply(price[,6:10*3-1]*(abs(price[,6:10*3+1]-price$OfferPrice1-.02)<.005),1,sum, na.rm=T)
price$OfferLow4Cnt = apply(price[,6:10*3-1]*(abs(price[,6:10*3+1]-price$OfferPrice1-.03)<.005),1,sum, na.rm=T)
price$OfferLow5Cnt = apply(price[,6:10*3-1]*(abs(price[,6:10*3+1]-price$OfferPrice1-.04)<.005),1,sum, na.rm=T)

form = paste("PriceDiff1SecAhead ~"
  ,paste0("BidHigh",1:5,"Cnt", collapse=" + ")
  ,"+",paste0("OfferLow",1:5,"Cnt", collapse=" + "))

#cvGroup = sample(1:10, size=nrow(price), replace=T)
#fit = cvModel( d=price, cvGroup, indCol=35, model=paste("glm(", form, ")" ) )
#coeffs = data.frame( do.call("rbind", fit$models) )
#coeffs = melt(coeffs, measure.vars=1:ncol(coeffs) )
#ggplot( coeffs, aes(x=variable, y=value) ) + geom_point() +
# geom_boxplot()
#Coefficients are very consistent across CV groups (except for intercept).

#Create an adjusted MicroPrice using a linear regression on the 2 days of training data:
fit = glm( as.formula(form), data=price[price$Time<=2*24*60*60,])
price$MicroPriceAdj = predict(fit, newdata=price)
price$MicroPriceAdj = price$MicroPriceAdj + price$MicroPrice
#Note: could also do this for 60 seconds ahead, but you get almost identical coefficients

price$LogBookImb = log( (price$BidQuantity1 + price$BidQuantity2 + price$BidQuantity3 + price$BidQuantity4 + price$BidQuantity5)
  / (price$OfferQuantity1 + price$OfferQuantity2 + price$OfferQuantity3 + price$OfferQuantity4 + price$OfferQuantity5) )
price$LogBookImbInside = log( price$BidQuantity1 / price$OfferQuantity1 )
#The MicroPriceAdjExp didn't seem as useful...
#price$BidQuantityAdj = as.numeric( cbind(price$BidHigh1Cnt, price$BidHigh2Cnt, price$BidHigh3Cnt, price$BidHigh4Cnt, price$BidHigh5Cnt) %*% 2^(0:-4) )
#price$OfferQuantityAdj = as.numeric( cbind(price$OfferLow1Cnt, price$OfferLow2Cnt, price$OfferLow3Cnt, price$OfferLow4Cnt, price$OfferLow5Cnt) %*% 2^(0:-4) )
#price$MicroPriceAdjExp = (price$BidPrice1*(price$OfferQuantityAdj)+price$OfferPrice1*(price$BidQuantityAdj) ) /
# (price$OfferQuantityAdj + price$BidQuantityAdj)

ggplot(price[sample(1:nrow(price),size=100000),], aes(x=LogBookImb, y=PriceDiff1SecAhead) ) +
  geom_point(alpha=.1) + geom_smooth()
ggplot(price[sample(1:nrow(price),size=100000),], aes(x=LogBookImbInside, y=PriceDiff1SecAhead) ) +
  geom_point(alpha=.1) + geom_smooth()
ggplot(price[sample(1:nrow(price),size=100000),], aes(x=MicroPriceAdjExp, y=PriceDiff1SecAhead) ) +
  geom_point(alpha=.1) + geom_smooth()
ggplot(price[sample(1:nrow(price),size=100000),], aes(x=MicroPriceAdjExp, y=MicroPrice) ) +
  geom_point(alpha=.1) + geom_smooth()

###############################################################################
# Add volume columns
###############################################################################

#price$TotalBidQuantity = price$BidQuantity1 + price$BidQuantity2 + price$BidQuantity3 + price$BidQuantity4 + price$BidQuantity5
#price$TotalOfferQuantity = price$OfferQuantity1 + price$OfferQuantity2 + price$OfferQuantity3 + price$OfferQuantity4 + price$OfferQuantity5
#colnames(price)[colnames(price)=="BidQuantity1"] = "InsideBidQuantity"
#colnames(price)[colnames(price)=="OfferQuantity1"] = "InsideOfferQuantity"
#price$Spread = price$OfferPrice1 - price$BidPrice1

#Remove unneccesary columns:
rmCols = c(paste0("BidQuantity",1:5), paste0("BidNumberOfOrders",1:5), paste0("BidPrice",1:5)
          ,paste0("OfferQuantity",1:5), paste0("OfferNumberOfOrders",1:5), paste0("OfferPrice",1:5)
          ,paste0("BidHigh",1:5,"Cnt"), paste0("OfferLow",1:5,"Cnt"), "BidQuantityAdj", "OfferQuantityAdj")
for(i in rmCols) price[,i] = NULL
write.csv(price, "price_base_cols.csv", row.names=F)

###############################################################################
# Load data into a big matrix object
###############################################################################

#(60+54+54+59)*3: 4 different lag groups for main vars
#(60+59)*2: 2 lag groups for trade vars
#13: main columns (current variables, timestamp, etc.)
#300: Extra columns, since you can't append more after creating (AFAIK)
d = big.matrix( nrow=nrow(price), ncol=(60+54+54+59)*3+(60+59)*2+13+300, backingfile="model_matrix" )
index = 1 #Specifies column of d that's being loaded
cnames = c()
for( i in 1:ncol(price) ){
  d[,index] = price[,i]
  cnames[index] = colnames(price)[i]
  index = index + 1
}

# Lagged MicroPrice adjusted using linear model
for( lag in c(1:60*.01,7:60*.1,7:60,2:60*60 ) ){
  d[,index] = load_lag_price(price[,c("Time","MicroPriceAdj")], lags=lag)
  cnames[index] = paste0("Lag_",lag,"_MicroPriceAdj")
  index = index + 1
}

# Lagged Log( Order Book Imbalance ) for provided bids/offers
for( lag in c(1:60*.01,7:60*.1,7:60,2:60*60 ) ){
  d[,index] = load_lag_price(price[,c("Time","LogBookImb")], lags=lag)
  cnames[index] = paste0("Lag_",lag,"_LogBookImb")
  index = index + 1
}

# Lagged Log( Order Book Imbalance ) for just the inside bid and offer
for( lag in c(1:60*.01,7:60*.1,7:60,2:60*60 ) ){
  d[,index] = load_lag_price(price[,c("Time","LogBookImbInside")], lags=lag)
  cnames[index] = paste0("Lag_",lag,"_LogBookImbInside")
  index = index + 1
}

# Number of Units traded and Number of Units traded on SELL side
for( lag in c(1:60,2:60*60) ){
  trade_hist = load_lag_trades( price, orders, lag=lag )[,3:4]
  d[,index] = trade_hist[,1]
  d[,index+1] = trade_hist[,2]
  cnames[index:(index+1)] = paste0("Lag_", lag, c("_Units","_UnitsSELL") )
  index = index + 2
}

write.csv(cnames, file="cnames.csv", row.names=F)

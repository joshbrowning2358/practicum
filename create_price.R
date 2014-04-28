#Read in functions
source_github("https://raw2.github.com/rockclimber112358/practicum/master/functions.R")
source_github("https://raw2.github.com/rockclimber112358/practicum/master/old_functions.R")
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

#Convert time to a numeric vector (i.e. seconds after 2013-11-04 00:00:00):
raw$Time = as.POSIXct(strptime(x=raw$Time, format="%Y%m%d %H:%M:%OS"))
raw$Time = as.numeric( raw$Time - as.POSIXct("2013-11-04") )

#Data frame with prices:
price = raw[raw$RestingSide=="",]
price = price[,c(1:31,35:37)]

#Data frame with transactions:
orders = raw[raw$RestingSide!="",]
orders = orders[,c(1,32:34)]
orders = orders[orders$RestingSide!="null",]
write.csv( orders, file="orders.csv", row.names=F )
#orders = read.csv(file="orders.csv")

rm(raw); gc()

#Use 1000x the price difference so RMSE's will not need to be adjusted.
price$PriceDiff1SecAhead = (price$MicroPrice1SecAhead - price$MicroPrice)*1000
price$PriceDiff60SecAhead = (price$MicroPrice60SecAhead - price$MicroPrice)*1000
price$day = floor( price$Time/(24*60*60) ) + 1
#Define outcry period from 6:45 to 1:30
price$Outcry = as.numeric( price$Time %% (24*60*60) > 6.75*60*60 & price$Time %% (24*60*60) < 13.5*60*60 )
#Identify which rows we need to make forecasts for
price$Diff = c(0,as.numeric(price$BidQuantity1[2:nrow(price)-1] !=price$BidQuantity1[2:nrow(price)]|
                            price$BidPrice1[2:nrow(price)-1] !=price$BidPrice1[2:nrow(price)]|
                            price$OfferQuantity1[2:nrow(price)-1] !=price$OfferQuantity1[2:nrow(price)]|
                            price$OfferPrice1[2:nrow(price)-1] !=price$OfferPrice1[2:nrow(price)]))
price$Diff[is.na(price$Diff)] = 0
price$Weight = 0 #Placeholder, assigned later
price$BidQRatio = price$BidQuantity1/(price$BidQuantity1+price$OfferQuantity1)
price$Width = price$OfferPrice1-price$BidPrice1

###############################################################################
# Define adjusted microprice(s)
###############################################################################

#Define quantities for bid, bid-.01, bid-.02, bid-.03, bid-.04 and offer, offer+.01, offer+.02, offer+.03, offer+.04
#Note: these are slightly different from BidQuantity2, BidQuantity3, etc!  The rationale is
#that I wanted to use the quantity on the highest bid and then all quantites on the bids 
#$0.01 to $0.04 below it.
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

#Estimate change in price using the variables above
form = paste("PriceDiff1SecAhead ~"
  ,paste0("BidHigh",1:5,"Cnt", collapse=" + ")
  ,"+",paste0("OfferLow",1:5,"Cnt", collapse=" + "))

#Create an adjusted MicroPrice using a linear regression on the 2 days of training data:
fit = glm( as.formula(form), data=price[price$Time<=2*24*60*60,])
price$MicroPriceAdj = predict(fit, newdata=price)
price$MicroPriceAdj = price$MicroPriceAdj + price$MicroPrice
#Note: could also do this for 60 seconds ahead, but you get almost identical coefficients

price$LogBookImb = log( (price$BidQuantity1 + price$BidQuantity2 + price$BidQuantity3 + price$BidQuantity4 + price$BidQuantity5)
  / (price$OfferQuantity1 + price$OfferQuantity2 + price$OfferQuantity3 + price$OfferQuantity4 + price$OfferQuantity5) )
price$LogBookImbInside = log( price$BidQuantity1 / price$OfferQuantity1 )

price$MicroPriceAdjClip = ifelse(price$MicroPriceAdj<price$BidPrice1,price$BidPrice1
                         ,ifelse(price$MicroPriceAdj>price$OfferPrice1,price$OfferPrice1
                         ,price$MicroPriceAdj))

#The MicroPriceAdjExp didn't seem as useful...
#price$BidQuantityAdj = as.numeric( cbind(price$BidHigh1Cnt, price$BidHigh2Cnt, price$BidHigh3Cnt, price$BidHigh4Cnt, price$BidHigh5Cnt) %*% 2^(0:-4) )
#price$OfferQuantityAdj = as.numeric( cbind(price$OfferLow1Cnt, price$OfferLow2Cnt, price$OfferLow3Cnt, price$OfferLow4Cnt, price$OfferLow5Cnt) %*% 2^(0:-4) )
#price$MicroPriceAdjExp = (price$BidPrice1*(price$OfferQuantityAdj)+price$OfferPrice1*(price$BidQuantityAdj) ) /
# (price$OfferQuantityAdj + price$BidQuantityAdj)

###############################################################################
# Add volume columns
###############################################################################

#Remove unneccesary columns:
rmCols = c(paste0("BidQuantity",2:5), paste0("BidNumberOfOrders",1:5), paste0("BidPrice",1:5)
          ,paste0("OfferQuantity",2:5), paste0("OfferNumberOfOrders",1:5), paste0("OfferPrice",1:5),
          paste0("BidHigh",1:5,"Cnt"), paste0("OfferLow",1:5,"Cnt"), "BidQuantityAdj", "OfferQuantityAdj")
for(i in rmCols) price[,i] = NULL
write.csv(price, "price_base_cols.csv", row.names=F)
#price = read.csv(file="price_base_cols.csv")

###############################################################################
# Load data into a big matrix object
###############################################################################

#5s of lags for MicroPrice, MicroPriceAdj, MicroPriceAdjClip, LogBookImb, LogBookImbInside,
# BidQRatio, Width, MicroPriceGeo, Units, UnitsSELL
#ncol(price): main columns (current variables, timestamp, etc.)
#20: Extra columns, since you can't append more after creating (AFAIK)
d = big.matrix( nrow=nrow(price), ncol=10*5+ncol(price)+20, backingfile="lag5_matrix" )

#Specifies column of d that's being loaded
index = 1 
#cnames will contain the colnames of d, since bigmatrix objects can't have column names
cnames = c()

#Add the columns of price to d (and cnames)
for( i in 1:ncol(price) ){
  d[,index] = price[,i]
  cnames[index] = colnames(price)[i]
  index = index + 1
}

# Lagged MicroPrice
for( lag in c(1:5) ){
  d[,index] = load_lag_price(price[,c("Time","MicroPrice")], lags=lag)
  cnames[index] = paste0("Lag_",lag,"_MicroPrice")
  index = index + 1
}

# Lagged MicroPrice adjusted using linear model
for( lag in c(1:5) ){
  d[,index] = load_lag_price(price[,c("Time","MicroPriceAdj")], lags=lag)
  cnames[index] = paste0("Lag_",lag,"_MicroPriceAdj")
  index = index + 1
}

# Lagged MicroPrice adjusted using linear model and then clipped
for( lag in c(1:5) ){
  d[,index] = load_lag_price(price[,c("Time","MicroPriceAdjClip")], lags=lag)
  cnames[index] = paste0("Lag_",lag,"_MicroPriceAdjClip")
  index = index + 1
}

# Lagged Log( Order Book Imbalance ) for provided bids/offers
for( lag in c(1:5) ){
  d[,index] = load_lag_price(price[,c("Time","LogBookImb")], lags=lag)
  cnames[index] = paste0("Lag_",lag,"_LogBookImb")
  index = index + 1
}

# Lagged Log( Order Book Imbalance ) for just the inside bid and offer
for( lag in c(1:5) ){
  d[,index] = load_lag_price(price[,c("Time","LogBookImbInside")], lags=lag)
  cnames[index] = paste0("Lag_",lag,"_LogBookImbInside")
  index = index + 1
}

# Lagged BidQRatio
for( lag in c(1:5) ){
  d[,index] = load_lag_price(price[,c("Time","BidQRatio")], lags=lag)
  cnames[index] = paste0("Lag_",lag,"_BidQRatio")
  index = index + 1
}

# Lagged Width
for( lag in c(1:5) ){
  d[,index] = load_lag_price(price[,c("Time","Width")], lags=lag)
  cnames[index] = paste0("Lag_",lag,"_Width")
  index = index + 1
}

#Create temp with MicroPriceGeo columns (didn't get a chance to try this)
#MPcols = (1:length(cnames))[sapply(cnames, grepl, pattern="MicroPrice$")]
#temp = data.frame(d[,c(which(cnames %in% c("MicroPrice1SecAhead", "day", "Diff")), MPcols)])
#colnames(temp) = c("MicroPrice1SecAhead", "day", "Diff", cnames[sapply(cnames, grepl, pattern="MicroPrice$")])
#form = paste("MicroPrice1SecAhead ~ MicroPrice +"
#  ,paste0(sapply(1:5, function(i){paste0("A^",i,"*Lag_",i,"_MicroPrice")}), collapse=" + "))
#fit = nls(form, data=temp[temp$day<=2,], start=list(A=.001), control=nls.control(tol=.00001, minFactor=10^-16, maxiter=10000))
for( lag in c(1:5) ){
#  d[,index] = ...
  cnames = c(cnames, paste0("Lag_",lag,"_MicroPriceGeo"))
  index = index + 1
}

# Number of Units traded and Number of Units traded on SELL side
for( lag in c(1:5) ){
  trade_hist = load_lag_trades( price, orders, lag=lag )[,3:4]
  d[,index] = trade_hist[,1]
  d[,index+1] = trade_hist[,2]
  cnames[index:(index+1)] = paste0("Lag_", lag, c("_Units","_UnitsSELL") )
  index = index + 2
  print(lag)
}

d[,index] = load_lag_price(price[,c("Time","LogBookImb")], lags=60)
cnames[index] = paste0("Lag_",60,"_LogBookImb")

#Save the column names for any time in the future when you load d:
write.csv(cnames, file="cnames.csv", row.names=F)

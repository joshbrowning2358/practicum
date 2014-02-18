source_github("https://raw2.github.com/rockclimber112358/practicum/master/functions.R")
source("/u/st/fl/jbrownin/MATH598_Statistics_Practicum/R_Code/functions.R")

options(digits.secs=6)

###############################################################################
# Raw processing: read in files, restructure
###############################################################################

raw = read.csv(file="/home/josh/Documents/Professional Files/Mines/MATH 598- Statistics Practicum/Data/20131104.CLZ3.log")
raw = rbind(raw, read.csv(file="/home/josh/Documents/Professional Files/Mines/MATH 598- Statistics Practicum/Data/20131105.CLZ3.log") )
raw = rbind(raw, read.csv(file="/home/josh/Documents/Professional Files/Mines/MATH 598- Statistics Practicum/Data/20131107.CLZ3.log") )
raw = rbind(raw, read.csv(file="/home/josh/Documents/Professional Files/Mines/MATH 598- Statistics Practicum/Data/20131108.CLZ3.log") )

raw = read.csv(file="20131104.CLZ3.log")
raw = rbind(raw, read.csv(file="20131105.CLZ3.log") )
raw = rbind(raw, read.csv(file="20131107.CLZ3.log") )
raw = rbind(raw, read.csv(file="20131108.CLZ3.log") )

#raw = read.csv(file="C:/Users/jbrowning/Desktop/To Home/Personal/Mines Files/MATH 598- Statistics Practicum/Data/20131104.CLZ3.log")
#raw = rbind(raw, read.csv(file="C:/Users/jbrowning/Desktop/To Home/Personal/Mines Files/MATH 598- Statistics Practicum/Data/20131105.CLZ3.log") )
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
write.csv( file="C:/Users/jbrowning/Desktop/To Home/Personal/Mines Files/MATH 598- Statistics Practicum/Data/orders.csv", orders )
write.csv( orders, file="/home/josh/Documents/Professional Files/Mines/MATH 598- Statistics Practicum/orders.csv", row.names=F )

rm(raw); gc()

price$PriceDiff1SecAhead = (price$MicroPrice1SecAhead - price$MicroPrice)*100
#price$PriceRatio1SecAhead = price$MicroPrice1SecAhead / price$MicroPrice - 1
#price$PriceDiff60SecAhead = price$MicroPrice60SecAhead - price$MicroPrice
#price$PriceRatio60SecAhead = price$MicroPrice60SecAhead / price$MicroPrice - 1
set.seed(123)
price$cvGroup = c(sample(c(rep(1:5,each=148125),rep(6:10,each=148124))),rep(-1,1237233))
price$Model.No = as.numeric( price$Time %% (24*60*60) > 6.75*60*60 & price$Time %% (24*60*60) < 13.5*60*60 )

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

form = paste("PriceDiff1SecAhead ~", paste0("BidHigh",1:5,"Cnt", collapse=" + "),"+",paste0("OfferLow",1:5,"Cnt", collapse=" + "))

#cvGroup = sample(1:10, size=nrow(price), replace=T)
#fit = cvModel( d=price, cvGroup, indCol=35, model=paste("glm(", form, ")" ) )
#coeffs = data.frame( do.call("rbind", fit$models) )
#coeffs = melt(coeffs, measure.vars=1:ncol(coeffs) )
#ggplot( coeffs, aes(x=variable, y=value) ) + geom_point() +
#  geom_boxplot()
#Coefficients are very consistent across CV groups (except for intercept).  Thus, it should be safe to use the model without worrying about it skewing cross-validation results.

fit = glm( as.formula(form), data=price[price$Time<=2*24*60*60,])
price$MicroPriceAdj = predict(fit, newdata=price)
price$MicroPriceAdj = price$MicroPriceAdj + price$MicroPrice
#Note: could also do this for 60 seconds ahead, but you get almost identical coefficients

price$LogBookImb = log( (price$BidQuantity1 + price$BidQuantity2 + price$BidQuantity3 + price$BidQuantity4 + price$BidQuantity5)
  / (price$OfferQuantity1 + price$OfferQuantity2 + price$OfferQuantity3 + price$OfferQuantity4 + price$OfferQuantity5) )
price$LogBookImbInside = log( price$BidQuantity1 / price$OfferQuantity1 )
price$BidQuantityAdj = as.numeric( cbind(price$BidHigh1Cnt, price$BidHigh2Cnt, price$BidHigh3Cnt, price$BidHigh4Cnt, price$BidHigh5Cnt) %*% 2^(0:-4) )
price$OfferQuantityAdj = as.numeric( cbind(price$OfferLow1Cnt, price$OfferLow2Cnt, price$OfferLow3Cnt, price$OfferLow4Cnt, price$OfferLow5Cnt) %*% 2^(0:-4) )
price$MicroPriceAdjExp = (price$BidPrice1*(price$OfferQuantityAdj)+price$OfferPrice1*(price$BidQuantityAdj) ) /
  (price$OfferQuantityAdj + price$BidQuantityAdj)

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
write.csv(price,"price_base_cols.csv",row.names=F)

###############################################################################
# Add Lagged Time Variables and estimated derivatives
###############################################################################

lag = load_lag_price(price[,c("Time","MicroPriceAdj")], lags=1:60*.01)
write.csv(lag,"price_lag_hund_cols.csv",row.names=F)

lag = load_lag_price(price[,c("Time","MicroPriceAdj")], lags=7:60*.1)
write.csv(lag,"price_lag_tenth_cols.csv",row.names=F)

lag = load_lag_price(price[,c("Time","MicroPriceAdj")], lags=7:60)
write.csv(lag,"price_lag_seconds_cols.csv",row.names=F)

lag = load_lag_price(price[,c("Time","MicroPriceAdj")], lags=2:60*60)
write.csv(lag,"price_lag_minutes_cols.csv",row.names=F)

########### MicroPriceAdjExp

lag = load_lag_price(price[,c("Time","MicroPriceAdjExp")], lags=1:60*.01)
write.csv(lag,"price_exp_lag_hund_cols.csv",row.names=F)

lag = load_lag_price(price[,c("Time","MicroPriceAdjExp")], lags=7:60*.1)
write.csv(lag,"price_exp_lag_tenth_cols.csv",row.names=F)

lag = load_lag_price(price[,c("Time","MicroPriceAdjExp")], lags=7:60)
write.csv(lag,"price_exp_lag_seconds_cols.csv",row.names=F)

lag = load_lag_price(price[,c("Time","MicroPriceAdjExp")], lags=2:60*60)
write.csv(lag,"price_exp_lag_minutes_cols.csv",row.names=F)

########### LogBookImb

lag = load_lag_price(price[,c("Time","LogBookImb")], lags=1:60*.01)
write.csv(lag,"log_book_imb_lag_hund_cols.csv",row.names=F)

lag = load_lag_price(price[,c("Time","LogBookImb")], lags=7:60*.1)
write.csv(lag,"log_book_imb_lag_tenth_cols.csv",row.names=F)

lag = load_lag_price(price[,c("Time","LogBookImb")], lags=7:60)
write.csv(lag,"log_book_imb_lag_seconds_cols.csv",row.names=F)

lag = load_lag_price(price[,c("Time","LogBookImb")], lags=2:60*60)
write.csv(lag,"log_book_imb_lag_minutes_cols.csv",row.names=F)

########### LogBookImbInside

lag = load_lag_price(price[,c("Time","LogBookImbInside")], lags=1:60*.01)
write.csv(lag,"log_book_imb_inside_lag_hund_cols.csv",row.names=F)

lag = load_lag_price(price[,c("Time","LogBookImbInside")], lags=7:60*.1)
write.csv(lag,"log_book_imb_inside_lag_tenth_cols.csv",row.names=F)

lag = load_lag_price(price[,c("Time","LogBookImbInside")], lags=7:60)
write.csv(lag,"log_book_imb_inside_lag_seconds_cols.csv",row.names=F)

lag = load_lag_price(price[,c("Time","LogBookImbInside")], lags=2:60*60)
write.csv(lag,"log_book_imb_inside_minutes_cols.csv",row.names=F)



#lag = load_lag_price(price[,c("Time","MicroPrice")], lags=c(1:30,45,60,120,300,600))
#price = cbind(price, lag)
rm(lag)

#####Skipping derivatives because they'll create a linearly dependent matrix:#####
#See http://en.wikipedia.org/wiki/Finite_difference_coefficients
#deriv_mat = matrix(0,nrow=6,ncol=4)
#deriv_mat[,1] = c(-137/60, 5, -5, 10/3, -5/4, 1/5)
#deriv_mat[,2] = c(15/4, -77/6, 107/6, -13, 61/12, -5/6)
#deriv_mat[,3] = c(-17/4, 71/4, -59/2, 49/2, -41/4, 7/4)
#deriv_mat[,4] = c(3, -14, 26, -24, 11, -2)
#derivs = as.matrix(price[,c("MicroPriceAdj",paste0("MicroPriceAdj_Lag_",1:5,"s"))]) %*% deriv_mat
#colnames(derivs) = paste0("DerivAdj",1:4)
#price = cbind(price, derivs)
#rm(derivs)

#derivs = as.matrix(price[,c("MicroPrice",paste0("MicroPrice_Lag_",1:5,"s"))]) %*% deriv_mat
#colnames(derivs) = paste0("Deriv",1:4)
#price = cbind(price, derivs)
#rm(derivs)

###############################################################################
# Add Time Variables: Seem pretty useless, so skipping these too.
###############################################################################

#price$SecSinceOpen = price$Time %% (24*3600)
#price$SecSinceHour = price$Time %% 3600
#price$SecSinceMin = price$Time %% 60

###############################################################################
# Trade History
###############################################################################

trades = load_lag_trades(price, orders, lag=1)[,3:4]
for( lag in c(3:30) ){
  trade_hist = load_lag_trades( price, orders, lag=lag )[,3:4]
  trades = cbind( trades, trade_hist )
}
write.csv(trades,"price_trade_sec_1_30.csv",row.names=F)

trades = load_lag_trades(price, orders, lag=31)[,3:4]
for( lag in c(32:60) ){
  trade_hist = load_lag_trades( price, orders, lag=lag )[,3:4]
  trades = cbind( trades, trade_hist )
}
write.csv(trades,"price_trade_sec_31_60.csv",row.names=F)

trades = load_lag_trades(price, orders, lag=2*60)[,3:4]
for( lag in c(3:30*60) ){
  trade_hist = load_lag_trades( price, orders, lag=lag )[,3:4]
  trades = cbind( trades, trade_hist )
}
write.csv(trades,"price_trade_min_2_30.csv",row.names=F)

trades = load_lag_trades(price, orders, lag=31*60)[,3:4]
for( lag in c(32:60*60) ){
  trade_hist = load_lag_trades( price, orders, lag=lag )[,3:4]
  trades = cbind( trades, trade_hist )
}
write.csv(trades,"price_trade_min_31_60.csv",row.names=F)

write.csv( price, file="/home/josh/Documents/Professional Files/Mines/MATH 598- Statistics Practicum/price.csv", row.names=F )
write.csv( file="C:/Users/jbrowning/Desktop/To Home/Personal/Mines Files/MATH 598- Statistics Practicum/Data/price.csv", price, row.names=F )

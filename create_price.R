source("C:/Users/jbrowning/Ubuntu One/Statistics Practicum/functions.R")
source("/home/josh/Ubuntu One/Statistics Practicum/functions.R")

options(digits.secs=6)

###############################################################################
# Raw processing: read in files, restructure
###############################################################################

raw = read.csv(file="/home/josh/Documents/Professional Files/Mines/MATH 598- Statistics Practicum/Data/20131104.CLZ3.log")
raw = rbind(raw, read.csv(file="/home/josh/Documents/Professional Files/Mines/MATH 598- Statistics Practicum/Data/20131105.CLZ3.log") )
raw = read.csv(file="C:/Users/jbrowning/Desktop/To Home/Personal/Mines Files/MATH 598- Statistics Practicum/Data/20131104.CLZ3.log")
raw = rbind(raw, read.csv(file="C:/Users/jbrowning/Desktop/To Home/Personal/Mines Files/MATH 598- Statistics Practicum/Data/20131105.CLZ3.log") )
raw$Time = as.POSIXct(strptime(x=raw$Time, format="%Y%m%d %H:%M:%OS"))
#Convert time to a numeric vector (i.e. seconds after 2013-11-04 00:00:00):
raw$Time = as.numeric( raw$Time - as.POSIXct("2013-11-04") )

#Data frame with prices:
price = raw[raw$RestingSide=="",]
price = price[,c(1:31,35:37)]

#Data frame with transactions:
orders = raw[raw$RestingSide!="",]
orders = orders[,c(1,32:34)]
write.csv( file="C:/Users/jbrowning/Desktop/To Home/Personal/Mines Files/MATH 598- Statistics Practicum/Data/orders.csv", orders )

rm(raw); gc()

price$PriceDiff1SecAhead = (price$MicroPrice1SecAhead - price$MicroPrice)*100
#price$PriceRatio1SecAhead = price$MicroPrice1SecAhead / price$MicroPrice - 1
#price$PriceDiff60SecAhead = price$MicroPrice60SecAhead - price$MicroPrice
#price$PriceRatio60SecAhead = price$MicroPrice60SecAhead / price$MicroPrice - 1

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
fit = glm( as.formula(form), data=price)
price$MicroPriceAdj1Sec = predict(fit)
price$MicroPriceAdj1Sec = price$MicroPriceAdj1Sec + price$MicroPrice
#form = paste("PriceDiff60SecAhead ~", paste0("BidHigh",1:5,"Cnt", collapse=" + "),"+",paste0("OfferLow",1:5,"Cnt", collapse=" + "))
#fit = glm( as.formula(form), data=price)
#price$MicroPriceAdj60Sec = predict(fit)

###############################################################################
# If desired, check cross-validated performance:
###############################################################################

#form = paste("PriceDiff1SecAhead ~", paste0("BidHigh",1:5,"Cnt", collapse=" + "),"+",paste0("OfferLow",1:5,"Cnt", collapse=" + "))
#cvGroup = sample(c(-1,1:10),size=nrow(price),replace=T)
#fit.bench = cvModel(price, cvGroup, model="glm(MicroPrice1SecAhead ~ MicroPrice)", indCol=33 )
#fit.new = cvModel(price, cvGroup, model=paste("glm(",form,")"), indCol=33 )
##Convert fit.new to appropriate scale:
#fit.new = fit.new + price$MicroPrice
##Wahoo!  This idea beats the persistence model and a regression model against current microprice!
#sum( (price$MicroPrice - price$MicroPrice1SecAhead)[cvGroup==-1]^2 )
#sum( (fit.bench - price$MicroPrice1SecAhead)[cvGroup==-1,]^2 )
#sum( (fit.new - price$MicroPrice1SecAhead)[cvGroup==-1,]^2, na.rm=T )

#form = paste("PriceDiff60SecAhead ~", paste0("BidHigh",1:5,"Cnt", collapse=" + "),"+",paste0("OfferLow",1:5,"Cnt", collapse=" + "))
#cvGroup = sample(c(-1,1:10),size=nrow(price),replace=T)
#fit.bench = cvModel(price, cvGroup, model="glm(MicroPrice60SecAhead ~ MicroPrice)", indCol=33 )
#fit.new = cvModel(price, cvGroup, model=paste("glm(",form,")"), indCol=33 )
##Convert fit.new to appropriate scale:
#fit.new = fit.new + price$MicroPrice
##These models do terrible...  Weird...
#sum( (price$MicroPrice - price$MicroPrice1SecAhead)[cvGroup==-1]^2 )
#sum( (fit.bench - price$MicroPrice60SecAhead)[cvGroup==-1,]^2 )
#sum( (fit.new - price$MicroPrice60SecAhead)[cvGroup==-1,]^2, na.rm=T )

###############################################################################
# Add volume columns
###############################################################################

price$TotalBidQuantity = price$BidQuantity1 + price$BidQuantity2 + price$BidQuantity3 + price$BidQuantity4 + price$BidQuantity5
price$TotalOfferQuantity = price$OfferQuantity1 + price$OfferQuantity2 + price$OfferQuantity3 + price$OfferQuantity4 + price$OfferQuantity5
colnames(price)[colnames(price)=="BidQuantity1"] = "InsideBidQuantity"
colnames(price)[colnames(price)=="OfferQuantity1"] = "InsideOfferQuantity"
price$Spread = price$OfferPrice1 - price$BidPrice1

#Remove unneccesary columns:
rmCols = c(paste0("BidQuantity",1:5), paste0("BidNumberOfOrders",1:5), paste0("BidPrice",1:5)
          ,paste0("OfferQuantity",1:5), paste0("OfferNumberOfOrders",1:5), paste0("OfferPrice",1:5)
          ,paste0("BidHigh",1:5,"Cnt"), paste0("OfferLow",1:5,"Cnt"))
for(i in rmCols) price[,i] = NULL

#write.csv(price, file="price.csv")
#price = read.csv(file="price.csv")
#price$Time = as.POSIXct(strptime(x=price$Time, format="%Y%m%d %H:%M:%OS"))

###############################################################################
# Add Lagged Time Variables and estimated derivatives
###############################################################################

lag = load_lag_times(as.matrix(price[,c("Time","MicroPriceAdj1Sec")]))
colnames(lag) = paste0("MicroPriceAdj1Sec_Lag_",1:20,"s")
price = cbind(price, lag)
rm(lag)

lag = load_lag_times(as.matrix(price[,c("Time","MicroPrice")]))
colnames(lag) = paste0("MicroPrice_Lag_",1:20,"s")
price = cbind(price, lag)
rm(lag)

#See http://en.wikipedia.org/wiki/Finite_difference_coefficients
deriv_mat = matrix(0,nrow=6,ncol=4)
deriv_mat[,1] = c(-137/60, 5, -5, 10/3, -5/4, 1/5)
deriv_mat[,2] = c(15/4, -77/6, 107/6, -13, 61/12, -5/6)
deriv_mat[,3] = c(-17/4, 71/4, -59/2, 49/2, -41/4, 7/4)
deriv_mat[,4] = c(3, -14, 26, -24, 11, -2)
derivs = as.matrix(price[,c("MicroPriceAdj1Sec",paste0("MicroPriceAdj1Sec_Lag_",1:5,"s"))]) %*% deriv_mat
colnames(derivs) = paste0("DerivAdj",1:4)
price = cbind(price, derivs)
rm(derivs)

derivs = as.matrix(price[,c("MicroPrice",paste0("MicroPrice_Lag_",1:5,"s"))]) %*% deriv_mat
colnames(derivs) = paste0("Deriv",1:4)
price = cbind(price, derivs)
rm(derivs)

###############################################################################
# Add Time Variables
###############################################################################

price$SecSinceOpen = price$Time %% (24*3600)
price$SecSinceHour = price$Time %% 3600
price$SecSinceMin = price$Time %% 60

write.csv( file="C:/Users/jbrowning/Desktop/To Home/Personal/Mines Files/MATH 598- Statistics Practicum/Data/price.csv", price )

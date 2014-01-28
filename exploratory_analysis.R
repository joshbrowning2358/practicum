source_github("https://raw2.github.com/rockclimber112358/practicum/master/functions.R")

price = read.csv( file="C:/Users/jbrowning/Desktop/To Home/Personal/Mines Files/MATH 598- Statistics Practicum/Data/price.csv")
price = read.csv( file="C:/Users/jbrowning/Desktop/To Home/Personal/Mines Files/MATH 598- Statistics Practicum/Data/price_sampled.csv")
price$X = NULL
#Not sure why this is happening, but fix this issue:
#price[,39] = as.numeric( as.character( price[,39] ) )

#Price doesn't change much in one second:
filter = sample( 1:nrow(price), size=100000 )
ggplot(price[filter,], aes(x=MicroPrice, y=MicroPrice1SecAhead) ) + geom_point(alpha=.1) + geom_smooth()
ggplot(price[filter,], aes(x=MicroPrice, y=MicroPrice60SecAhead) ) + geom_point(alpha=.1) + geom_smooth()

table( round(price$MicroPrice-price$MicroPrice1SecAhead, 2) )
qplot( price$MicroPrice-price$MicroPrice1SecAhead, binwidth=.01, origin=-.155 )
table( round(price$MicroPrice-price$MicroPrice60SecAhead, 2) )
qplot( price$MicroPrice-price$MicroPrice60SecAhead, binwidth=.01, origin=-.405 )

#60 sec variability based on time of day
ggplot( price[filter,], aes(x=Time, y=MicroPrice60SecAhead-MicroPrice) ) + geom_point()
ggplot( price[filter,], aes(x=Time, y=MicroPrice1SecAhead-MicroPrice) ) + geom_point()
#Prices 1 second ahead are roughly constant if InsideQuantity is low.  For 60s, "Total" Quantity is also important, and trends aren't as clear:
#Also, current spread greatly affects 1s price changes but has a much smaller effect on 60s price changes (negative?!)
ggplot( price[filter,], aes(x=InsideBidQuantity + InsideOfferQuantity, y=abs(MicroPrice1SecAhead-MicroPrice)) ) + #geom_point(alpha=.1) +
  geom_smooth()
ggplot( price[filter,], aes(x=TotalBidQuantity + TotalOfferQuantity, y=abs(MicroPrice1SecAhead-MicroPrice)) ) + #geom_point(alpha=.1) +
  geom_smooth()
ggplot( price[filter,], aes(x=Spread, y=abs(MicroPrice1SecAhead-MicroPrice)) ) + #geom_point(alpha=.1) +
  geom_smooth(method="glm")
ggplot( price[filter,], aes(x=InsideBidQuantity + InsideOfferQuantity, y=abs(MicroPrice60SecAhead-MicroPrice)) ) + #geom_point(alpha=.1) +
  geom_smooth()
ggplot( price[filter,], aes(x=TotalBidQuantity + TotalOfferQuantity, y=abs(MicroPrice60SecAhead-MicroPrice)) ) + #geom_point(alpha=.1) +
  geom_smooth()
ggplot( price[filter,], aes(x=Spread, y=abs(MicroPrice60SecAhead-MicroPrice)) ) + #geom_point(alpha=.1) +
  geom_smooth(method="glm")

corr.coeff = function(d, indCol, depCols){
  d2 = d[sample(1:nrow(d), replace=T, size=nrow(d)),]
  return(data.frame(cor(d2[,indCol], d2[,depCols], use="pairwise.complete.obs")))
}
boots = t(sapply( 1:100, function(n)corr.coeff(d=price, indCol=4, depCols=32:51 ) ) )
boots = apply( boots, 2, as.numeric )
boots = apply( boots, 2, function(x){data.frame(mean=mean(x), hi=quantile(x,.975), lo=quantile(x,.025) )} )
boots = do.call("rbind", boots )
boots$Lag = 1:nrow(boots)
ggplot( boots, aes(x=Lag, y=mean, ymax=hi, ymin=lo) ) + geom_line() + geom_ribbon(alpha=.15) +
  scale_y_continuous(limits=c(0,1))

form = as.formula( paste("MicroPrice ~", paste0("MicroPrice_Lag_",1:20,"s",collapse="+") ) )
fit = glm( form, data=price )
summary(fit)
library(car)
vif(fit)
#So, most lags are significant but variance is very inflated.  But, even though significant, all terms but 1s lag are tiny.  20s lag is interesting though...
library(glmnet)
fit = fit.glmnet( form, data=price )

form = as.formula( paste("MicroPrice1SecAhead ~ MicroPrice + ", paste0("MicroPrice_Lag_",1:20,"s",collapse="+") ) )
fit = glm( form, data=price )
summary(fit)
vif(fit)
#Lags which are significant seem very strange, but we have use VIFs
fit = fit.glmnet( form, data=price )
plot(fit)
#Makes much more sense, only one variable is really useful

form = as.formula( paste("PriceDiff1SecAhead ~ ", paste0("MicroPrice_Lag_",1:20,"s",collapse="+") ) )
fit = glm( form, data=price )
summary(fit)
vif(fit)
#So, most lags are significant but variance is very inflated.  But, even though significant, all terms but 1s lag are tiny.  20s lag is interesting though...
fit = fit.glmnet( form, data=price )
plot(fit)
coeffs = data.frame( t( as.matrix(fit$beta) ) )
coeffs$lambda = fit$lambda
coeffs = melt( coeffs, id.vars="lambda" )
coeffs$variable = as.numeric(as.character(gsub("(MicroPrice_Lag_|s)","",coeffs$variable)))
ggplot( coeffs, aes(x=lambda, y=value, color=variable, group=variable) ) + geom_line() +
  scale_x_log10(breaks=10^(-4:0))

ggplot( coeffs[coeffs$variable<=4,], aes(x=lambda, y=value, color=as.factor(variable), group=variable) ) + geom_line() +
  scale_x_log10(breaks=10^(-4:0))


#############################################################
# Volatility Plots
#############################################################

price$SecondRounded = floor( price$Time )
price.agg = ddply(price, "SecondRounded", function(df){
  data.frame( Variance=var(price$MicroPrice)
    ,Spread=max(price$MicroPrice)-min(price$MicroPrice)
    ,Trades=0
    ,MaxQuantity=max(price$TotalBidQuantity + price$TotalOfferQuantity) )
} )





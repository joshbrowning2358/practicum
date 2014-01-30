source_github("https://raw2.github.com/rockclimber112358/practicum/master/functions.R")
setwd("/home/josh/Documents/Professional Files/Mines/MATH 598- Statistics Practicum")

price = read.csv( file="C:/Users/jbrowning/Desktop/To Home/Personal/Mines Files/MATH 598- Statistics Practicum/Data/price.csv")
price = read.csv( file="C:/Users/jbrowning/Desktop/To Home/Personal/Mines Files/MATH 598- Statistics Practicum/Data/price_sampled.csv")
price = read.csv( file="/home/josh/Documents/Professional Files/Mines/MATH 598- Statistics Practicum/price.csv" )
price$X = NULL
#Not sure why this is happening, but fix this issue:
#price[,39] = as.numeric( as.character( price[,39] ) )

#Price doesn't change much in one second:
filter = sample( 1:nrow(price), size=10000 )
ggsave("MicroPrice_MicroPrice1SecAhead_Correlated.png",
  ggplot(price[filter,], aes(x=MicroPrice, y=MicroPrice1SecAhead) ) + geom_point(alpha=.1) + geom_smooth()
  ,width=8, height=8, dpi=400 )
ggsave("MicroPrice_MicroPrice60SecAhead_Correlated.png",
  ggplot(price[filter,], aes(x=MicroPrice, y=MicroPrice60SecAhead) ) + geom_point(alpha=.1) + geom_smooth()
  ,width=8, height=8, dpi=400 )

table( round(price$MicroPrice-price$MicroPrice1SecAhead, 2) )
ggsave("Price_Difference_1sec_Histogram.png",
  qplot( price$MicroPrice-price$MicroPrice1SecAhead, binwidth=.01, origin=-.155 ) +
    labs(x="Price Difference (1 sec ahead)", y="Count") + scale_y_continuous(label=comma)
  ,width=8, height=8, dpi=400 )
table( round(price$MicroPrice-price$MicroPrice60SecAhead, 2) )
ggsave("Price_Difference_60sec_Histogram.png",
  qplot( price$MicroPrice-price$MicroPrice60SecAhead, binwidth=.01, origin=-.405 ) +
    labs(x="Price Difference (60 sec ahead)", y="Count") + scale_y_continuous(label=comma)
  ,width=8, height=8, dpi=400 )

#Variability based on time of day
ggsave("Price_Difference_60sec_Variability_Over_time.png",
  ggplot( price[filter,], aes(x=as.POSIXct(Time, origin=as.Date("2013-11-04","%Y-%m-%d"), tz="GMT"), y=MicroPrice60SecAhead-MicroPrice) ) +
    geom_point(alpha=.2) + labs(x="", y="Price Difference (1 sec ahead)")
  ,width=8, height=8, dpi=400 )
ggsave("Price_Difference_60sec_Variability_Over_time.png",
  ggplot( price[filter,], aes(x=as.POSIXct(Time, origin=as.Date("2013-11-04","%Y-%m-%d"), tz="GMT"), y=MicroPrice1SecAhead-MicroPrice) ) +
    geom_point(alpha=.2) + labs(x="", y="Price Difference (1 sec ahead)")
  ,width=8, height=8, dpi=400 )

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
#So, most lags are significant but variance is very inflated. But, even though significant, all terms but 1s lag are tiny. 20s lag is interesting though...
library(glmnet)
fit = fit.glmnet( form, data=price )
plot(fit)

form = as.formula( paste("MicroPrice1SecAhead ~ MicroPrice + ", paste0("MicroPrice_Lag_",1:20,"s",collapse="+") ) )
fit = glm( form, data=price )
summary(fit)
vif(fit)
#Lags which are significant seem very strange, but we have use VIFs
fit = fit.glmnet( form, data=price )
#Makes much more sense, only one variable is really useful
coeffs = data.frame( t( as.matrix(fit$beta) ) )
coeffs$lambda = fit$lambda
coeffs = melt( coeffs, id.vars="lambda" )
coeffs$variable = as.numeric(as.character(gsub("(MicroPrice_Lag_|s)","",coeffs$variable)))
ggsave("glmnet_coefficients_future_vs_lags.png",
  ggplot( coeffs[coeffs$variable<=5,], aes(x=lambda, y=value, color=as.factor(variable), group=variable) ) + geom_line() +
    scale_x_log10(breaks=10^(-4:0)) + labs(color="Lag Number")
  ,width=8, height=8, dpi=400 )

form = as.formula( paste("PriceDiff1SecAhead ~ ", paste0("MicroPrice_Lag_",1:20,"s",collapse="+") ) )
fit = glm( form, data=price )
summary(fit)
vif(fit)
#So, most lags are significant but variance is very inflated. But, even though significant, all terms but 1s lag are tiny. 20s lag is interesting though...
fit = fit.glmnet( form, data=price )
coeffs = data.frame( t( as.matrix(fit$beta) ) )
coeffs$lambda = fit$lambda
coeffs = melt( coeffs, id.vars="lambda" )
coeffs$variable = as.numeric(as.character(gsub("(MicroPrice_Lag_|s)","",coeffs$variable)))
ggplot( coeffs, aes(x=lambda, y=value, color=variable, group=variable) ) + geom_line() +
  scale_x_log10(breaks=10^(-4:0))

ggsave("glmnet_coefficients_diff_vs_lags.png",
  ggplot( coeffs[coeffs$variable<=5,], aes(x=lambda, y=value, color=as.factor(variable), group=variable) ) + geom_line() +
    scale_x_log10(breaks=10^(-4:0)) + labs(color="Lag Number")
  ,width=8, height=8, dpi=400 )

#############################################################
# Volatility Plots
#############################################################

price$SecondRounded = floor( price$Time )
price$MinuteRounded = floor( price$Time/60 )*60
price.agg = ddply(price, "MinuteRounded", function(df){
  data.frame( Variance=var(df$MicroPrice)
    ,Spread=max(df$MicroPrice)-min(df$MicroPrice)
    ,Trades=df$Trades_Lag_60s[nrow(df)]
    ,MaxQuantity=max(df$TotalBidQuantity + df$TotalOfferQuantity, na.rm=T) )
} )
cor( price.agg )

fit = kmeans( price.agg[,-1], centers=3 )
price.agg$clust = fit$cluster
ggplot( price.agg, aes(x=MinuteRounded, y=Spread)) + geom_point(aes(color=as.factor(clust)), alpha=.5)
ggsave("Cluster_Time_3Groups.png",
  ggplot( price.agg, aes(x=MinuteRounded, fill=as.factor(clust))) + geom_bar(position="fill", binwidth=60*10) +
    scale_x_continuous(breaks=0:10*15000) + theme(axis.text.x=element_text(angle=90, vjust=0.5)) +
    labs(x="Seconds after start of data", y="Proportion", fill="Cluster Number") +
    scale_y_continuous(label=percent)
  ,width=8, height=8, dpi=400 )
#3 centers doesn't seem to make much sense...

fit = kmeans( price.agg[,-1], centers=2 )
price.agg$clust = fit$cluster
ggplot( price.agg, aes(x=MinuteRounded, y=Spread)) + geom_point(aes(color=as.factor(clust)), alpha=.5)
ggsave("Cluster_Time_3Groups.png",
  ggplot( price.agg, aes(x=MinuteRounded, fill=as.factor(clust))) + geom_bar(position="fill", binwidth=60*10) +
    scale_x_continuous(breaks=0:10*15000) + theme(axis.text.x=element_text(angle=90, vjust=0.5)) +
    labs(x="Seconds after start of data", y="Proportion", fill="Cluster Number") +
    scale_y_continuous(label=percent)
  ,width=8, height=8, dpi=400 )
27000/(60*60); 50000/(60*60)
#Try some different centers, see how they work. Ideally, we should get different chunks of time, not "randomness".


#############################################################
# Order Volume
#############################################################

orders = orders[orders$RestingSide!="null",]
ggsave("Time_Between_Orders_Histogram.png", 
  qplot( orders$Time[2:nrow(orders)]-orders$Time[1:(nrow(orders)-1)] ) + scale_x_continuous(limit=c(0,50)) +
    labs(x="Time between trades") + scale_y_continuous("Count", label=comma)
  ,width=8, height=8, dpi=400 )
orders$DeltaT = c(NA,orders$Time[2:nrow(orders)]-orders$Time[1:(nrow(orders)-1)])
ggsave("Time_Between_Orders_RestingSide_Histogram.png", 
  ggplot( orders, aes(x=DeltaT, fill=RestingSide) ) + geom_bar() +
    scale_x_continuous(limit=c(0,50)) + labs(x="Time between trades") +
    scale_y_continuous("Count", label=comma)
  ,width=8, height=8, dpi=400 )
ggsave("Time_Between_Orders_RestingSide_Proportion.png", 
  ggplot( orders, aes(x=DeltaT, fill=RestingSide) ) + geom_bar(position="fill") +
    scale_x_continuous(limit=c(0,50)) + labs(x="Time between trades") +
    scale_y_continuous("Percent of Total", label=percent)
  ,width=8, height=8, dpi=400 )

#############################################################
# Examining Predictiveness of Variables
#############################################################

perf.filter = price$PriceDiff1SecAhead!=0
perf = sum( (price$PriceDiff1SecAhead/100)[perf.filter]^2/
  sum(perf.filter))
mods = data.frame( Type="Mean", Indep="Price", Residuals=perf )
mods$Type = as.character( mods$Type )
mods$Indep = as.character( mods$Indep )

add.row = function(fit, mod.name, mod.type="Price"){
  perf = sum( (fit$residuals)[!is.na(fit$residuals) & perf.filter]^2/
    sum(perf.filter & !is.na(fit$residuals)))
  if( mod.type=="Differenced" ) perf=perf/100^2
  mods <<- rbind( mods, c("MicroPrice Only", mod.type, perf) )
}

fit = glm( MicroPrice1SecAhead ~ MicroPrice, data=price )
add.row( fit, "MicroPrice Only" )

fit = glm( PriceDiff1SecAhead ~ MicroPrice, data=price )
add.row( fit, "MicroPrice Only", "Differenced" )

fit = glm( MicroPrice1SecAhead ~ MicroPriceAdj1Sec, data=price )
add.row( fit, "Adj. MicroPrice")

fit = glm( PriceDiff1SecAhead ~ MicroPriceAdj1Sec, data=price )
add.row( fit, "Adj. MicroPrice", "Differenced")

fit = glm( MicroPrice1SecAhead ~ MicroPrice + MicroPrice_Lag_1s + MicroPrice_Lag_2s + MicroPrice_Lag_3s , data=price )
add.row( fit, "MicroPrice + 3 Lags" )

fit = glm( PriceDiff1SecAhead ~ MicroPrice + MicroPrice_Lag_1s + MicroPrice_Lag_2s + MicroPrice_Lag_3s , data=price )
add.row( fit, "MicroPrice + 3 Lags", "Differenced" )

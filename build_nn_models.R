library(plyr)
library(rpart)
price$Time5Min = floor( price$Time/(5*60) )*5*60
fluct = ddply( price, "Time5Min", function(df){
  data.frame( updates=nrow(df), price_changes=sum(df$PriceDiff1SecAhead!=0) )
} )
ggplot( fluct, aes(x=Time5Min) ) + geom_line(aes(y=updates, color="updates") ) +
  geom_line(aes(y=price_changes, color="price changes") )

price$Time1Sec = floor( price$Time )
SecVolume = ddply( price, "Time1Sec", function(df){
  nrow(df) } )
#Move Time forward 1 second so that for each row you have volume in last second:
SecVolume$Time1Sec = SecVolume$Time1Sec+1
temp = merge(price, SecVolume, by="Time1Sec", all.x=T)

form = as.formula( paste( "PriceDiff1SecAhead ~ ",paste0("MicroPriceAdj1Sec_Lag_", 1:3, "s", collapse="+" ) ) )
ensem = cvModel( price, cvGroup=sample(1:10,size=100,replace=T), indCol=7
    ,model="fit.nn( form, hidden=10 )" )
sum( (ensem$ensem - price$PriceDiff1SecAhead)^2/sum(!is.na(ensem$ensem)), na.rm=T )
#price[1:1000,]: 0.103758
#price[1:2000,]: 0.08157515
#price[1:5000,]: 0.8791287
#price[1:10000,]: 0.1335054
#price[1:30000,]: 0.6253379
#price[1:100000,]: 0.4024115
#price[1:300000,]: 0.6889676
#price: 0.4706029

cvGroup=sample(c(rep(1:5,each=148125),rep(6:10,each=148124)))
write.csv(file="cvGroup.csv", cvGroup)

for( lag in c(3,5,8,12,16,20) ){
  form = as.formula( paste( "PriceDiff1SecAhead ~ ",paste0("MicroPriceAdj1Sec_Lag_", 1:lag, "s", collapse="+" ) ) )
  ensem = cvModel( price, cvGroup, indCol=7
      ,model="fit.nn( form, hidden=10 )" )
  perf = sum( (ensem$ensem - price$PriceDiff1SecAhead)^2/sum(!is.na(ensem$ensem)), na.rm=T )
  models = ensem$models
  save(list="models", file=paste0("lag",lag,"_hidden10_",round(perf,4),".RData") )
}

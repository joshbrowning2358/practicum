price = read.csv( file="C:/Users/jbrowning/Ubuntu One/Statistics Practicum/price.csv")
price$X = NULL

#Price doesn't change much in one second:
filter = sample( 1:nrow(price), size=100000 )
ggplot(price[filter,], aes(x=MicroPrice, y=MicroPrice1SecAhead) ) + geom_point() + geom_smooth()
ggplot(price[filter,], aes(x=MicroPrice, y=MicroPrice60SecAhead) ) + geom_point() + geom_smooth()

table( round(price$MicroPrice-price$MicroPrice1SecAhead, 2) )
qplot( round(price$MicroPrice-price$MicroPrice1SecAhead, 2), binwidth=.01 )
table( round(price$MicroPrice-price$MicroPrice60SecAhead, 2) )
qplot( round(price$MicroPrice-price$MicroPrice60SecAhead, 2), binwidth=.01 )

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

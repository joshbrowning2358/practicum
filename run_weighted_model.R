source_github("https://raw2.github.com/rockclimber112358/practicum/master/functions.R")
setwd("/media/storage/Professional Files/Mines/MATH 598- Statistics Practicum/Data/")
d = attach.big.matrix("model_matrix.desc")

Start = Sys.time()
out = weighted_model(d, ind_vars=c("LogBookImbInside","MicroPriceAdj"), step.size=15*60 )
Sys.time()-Start
#Time difference of 19.71234 mins

filter = d[,which(cnames=="Time")]>24*60*60*2
eval_preds( out[filter], price_diff=d[filter,5], price=d[filter,2], time=d[filter,1] )
eval_preds( 0, price_diff=d[filter,5], price=d[filter,2], time=d[filter,1] )



#Plot results over time and current price level
temp = eval_preds( out[filter], price_diff=d[filter,5], price=d[filter,2], time=d[filter,1], full=T )
ggplot(temp[[2]], aes(x=time, y=Model.RMSE/sqrt(Base.MSE))) + geom_point()
ggplot(temp[[3]], aes(x=price, y=Model.RMSE/sqrt(Base.MSE))) + geom_point()

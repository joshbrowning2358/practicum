library(nnet)
library(neuralnet)
library(TeachNet)
library(RSNNS)
library(AMORE)

d = data.frame( x = runif(2000, max=4*pi, min=0) )
d$y = sin(d$x) + rnorm(2000)
qplot( d$x, d$y ) + geom_smooth()
d$train = rep(c(T,F), each=1000)

####################################################################
#Now fit models to the data using various neural network packages
####################################################################

#nnet
fit1 = nnet( d$x[d$train], d$y[d$train], size=6, linout=T, maxit=100 )
d$preds = predict( fit1, newdata=d[,"x",drop=F] )
ggplot(d) + geom_point(aes(x=x,y=y)) +
  geom_line(aes(x=x, y=preds), color=2, size=1.5)

#neuralnet: Takes a long time to fit
#fit2 = neuralnet( y ~ x, data=d[d$train,], hidden=6, threshold=.1 )
#is( compute( fit2, covariate=d[,"x",drop=F] ) )
#d$preds = compute( fit2, covariate=d[,"x",drop=F] )
#ggplot(d) + geom_point(aes(x=x,y=y)) +
#  geom_line(aes(x=x, y=preds), color=2, size=1.5)

#TeachNet- doesn't work with continuous output
#fit3 = neuralnet( y ~ x, data=d[d$train,], hidden=6 )
#d$preds = predict( fit3, newdata=d[,"x",drop=F] )
#ggplot(d) + geom_point(aes(x=x,y=y)) +
#  geom_line(aes(x=x, y=preds), color=2, size=1.5)

#RSNNS
fit4 = mlp( cbind(1,d$x[d$train]), d$y[d$train], size=c(6), linOut=T )
d$preds = predict( fit4, newdata=cbind(1,d[,"x",drop=F]) )
ggplot(d) + geom_point(aes(x=x,y=y)) +
  geom_line(aes(x=x, y=preds), color=2, size=1.5)

fit5 = rbf( cbind(1,d$x[d$train]), d$y[d$train], size=6 )
d$preds = predict( fit5, newdata=cbind(1,d[,"x",drop=F]) )
ggplot(d[!d$train,]) + geom_point(aes(x=x,y=y)) +
  geom_line(aes(x=x, y=preds), color=2, size=1.5)

fit6 = elman( cbind(1,d$x[d$train]), d$y[d$train], size=6 )
d$preds = predict( fit6, newdata=cbind(1,d[,"x",drop=F]) )
ggplot(d[!d$train,]) + geom_point(aes(x=x,y=y)) +
  geom_line(aes(x=x, y=preds), color=2, size=1.5)

fit7 = jordan( cbind(1,d$x[d$train]), d$y[d$train], size=6 )
d$preds = predict( fit7, newdata=cbind(1,d[,"x",drop=F]) )
ggplot(d[!d$train,]) + geom_point(aes(x=x,y=y)) +
  geom_line(aes(x=x, y=preds), color=2, size=1.5)

fit7 = jordan( d$x[d$train], d$y[d$train], size=6 )
d$preds = predict( fit7, newdata=d[,"x",drop=F])
ggplot(d[!d$train,]) + geom_point(aes(x=x,y=y)) +
  geom_line(aes(x=x, y=preds), color=2, size=1.5)


#AMORE
detach(package:RSNNS)
net = newff( c(2,6,1), learning.rate.global=1e-2, momentum.global=.5
  ,error.criterium="LMS", Stao=NA, hidden.layer="tansig"
  ,output.layer="purelin", method="ADAPTgdwm" )
fit8 = train( net, as.matrix(cbind(1,d[d$train,"x",drop=F])), as.matrix(d[d$train,"y",drop=F])
  ,error.criterium="LMS", show.step=200, n.shows=10 )
predict.AMORE = function(net, newdata){
  nodes = net$net$layers
  if( ncol(newdata)!=length(nodes[[1]]) ) stop("Incorrect number of columns")
  curr.neur = 1
  curr.node = as.matrix(newdata)
  for( i in 2:length(nodes) ){
    new.node = matrix(0,nrow=nrow(newdata),ncol=0)
    for( j in nodes[[i]]){
      new.node = cbind( new.node,
        net$net$neurons[[curr.neur]]$f0( curr.node%*%net$net$neurons[[curr.neur]]$weights )
      )
      curr.neur = curr.neur + 1
    }
    curr.node = new.node
  }
  return( curr.node )
}
d$preds = predict.AMORE( fit8, newdata=cbind(1,d[,"x",drop=F]) )
ggplot(d[!d$train,]) + geom_point(aes(x=x,y=y)) +
  geom_line(aes(x=x, y=preds), color=2, size=1.5)

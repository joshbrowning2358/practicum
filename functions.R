library(randomForest)
library(rpart)
library(gbm)
library(AMORE)
library(glmnet)
library(pls)
library(car)
library(Rcpp)
library(inline)
library(RcppArmadillo)
library(reshape)
library(plyr)
library(ggplot2)
library(scales)
library(neuralnet)
library(biglm)
library(sqldf)
library(bigmemory)

eval_preds = function( preds, act, time=NULL ){
  SS = sum( (preds-act)[!is.na(preds) & act!=0]^2 )
  cnt = sum( !is.na(preds) & act!=0 )
  print(paste("MSE of target is:", round(sum(act^2)/sum(act!=0),6)))
  print(paste("MSE of predictions is:", round(SS/cnt,6)))

  #If time column is provided, generate plot of performance by 15 minute intervals
  if( !is.null(time) ){
    d = data.frame( preds, act, time = floor(time/(15*60))*15*60 )
    d.agg = ddply( d, "time", function(df){
      SS = sum( (df$preds-df$act)[!is.na(df$preds) & df$act!=0]^2 )
      cnt = sum( !is.na(df$preds) & df$act!=0 )
      data.frame(Base.MSE=sum(df$act^2)/sum(df$act!=0)
        ,Model.MSE = SS/cnt )
    } )
    d.agg$Improvement = d.agg$Model.MSE / d.agg$Base.MSE
    toPlot = melt(d.agg, id.vars="time", measure.vars=c("Improvement","Base.MSE"))
    baseline = data.frame(time=c(0,max(d.agg$time)), value=1, variable="Improvement")
    print( ggplot(toPlot, aes(x=time, y=value) ) + geom_line() + facet_wrap(~variable, scales="free") +
      geom_line(data=baseline, color="red", linetype=2) )
  }
  
  return(SS/cnt)
}

#form: specify the model formula, i.e. Y ~ X1 + X2 + X3.  Note that "." notation is supported.
#data: a dataframe containing the data for which the model is desired.
#hidden: the number of hidden neurons in the network.  The package only supports one hidden layer.
#steps: how many iterations should be ran?  Note this may need adjustment based on convergence.
#print: how many times should the function print the current fit number and LMS?
#...: other parameters to be passed into newff or train in the AMORE package.
fit.nn = function( form, data, hidden, steps=1000, print=10, learning.rate.global=1e-

library(nnet)
library(neuralnet)
library(TeachNet)
library(RSNNS)
library(AMORE)
setwd("/media/storage/Professional Files/Mines/MATH 598- Statistics Practicum/Neural Networks HW")

params = data.frame(replication=1:10)
params = merge( params, data.frame(type=c("half-quadratic", "quadratic", "sin", "linear")) )
params = merge(params, data.frame(n=round(10^seq(1,5,length.out=9))) )

#Simulation #1: Try out a bunch of different packages and compare fitting times/MSE:
results = matrix(0,nrow=0,ncol=6)
for( j in 1:nrow(params) ){
  #results = rbind(results, do.call("rbind",
  #  lapply((j*10-9):(j*10-5), function(i){sim_neural_net(type=params$type[i], n=params$n[i])})) )
  results = rbind(results, sim_neural_net(type=params$type[j], n=params$n[j]) )
  write.csv(file="neural_net_output.csv", results, row.names=F)
  print(j)
}

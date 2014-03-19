library(nnet)
library(neuralnet)
library(TeachNet)
library(RSNNS)
library(AMORE)
library(gridExtra)
source_github("https://raw2.github.com/rockclimber112358/practicum/master/functions.R")
setwd("/media/storage/Professional Files/Mines/MATH 598- Statistics Practicum/Neural Networks HW")

##########################################################################################
#Simulation #1: Try out a bunch of different packages and compare fitting times/MSE:
#Conclusion: Use nnet to train the initial network.  AMORE may be useful for updating.
##########################################################################################

params = data.frame(replication=1:10)
params = merge( params, data.frame(type=c("half-quadratic", "quadratic", "sin", "linear")) )
params = merge(params, data.frame(n=round(10^seq(1,5,length.out=9))) )

results = matrix(0,nrow=0,ncol=6)
for( j in 1:nrow(params) ){
  #results = rbind(results, do.call("rbind",
  #  lapply((j*10-9):(j*10-5), function(i){sim_neural_net(type=params$type[i], n=params$n[i])})) )
  results = rbind(results, sim_neural_net(type=params$type[j], n=params$n[j]) )
  write.csv(file="neural_net_output.csv", results, row.names=F)
  print(j)
}

results = data.frame( results )
for( i in c(1:3,6)) results[,i] = as.numeric( as.character( results[,i] ) )
#results = read.csv(file="neural_net_output.csv")

#Generate some plots:
#Time:
ggsave("Neural_Package_Times.png",
  ggplot(results, aes(x=n, y=t, color=mod) ) + geom_point() +
    geom_smooth(method="lm", formula=as.formula(y ~ exp(x) + 0), se=T) +
    scale_x_log10() + facet_wrap( ~ type ) +
    labs(x="Sample Size", y="Time (Minutes)", color="Package")
  ,width=10, height=10, dpi=400 )
results_new = results[results$mod!="neuralnet",]

#MSE.Test
ggsave("Neural_Package_Test_MSE.png",
  ggplot(results, aes(x=n, y=MSE.Test, color=mod) ) + geom_point() +
    #geom_smooth(method="lm", formula=as.formula(y ~ exp(x) + 0), se=T) +
    scale_x_log10() + facet_wrap( ~ type ) + scale_y_log10()
    labs(x="Sample Size", y="MSE on test data", color="Package")
  ,width=10, height=10, dpi=400 )
ggsave("Neural_Package_Test_MSE_Smoothed.png",
  ggplot(results, aes(x=n, y=MSE.Test, color=mod) ) +
    geom_smooth(se=F) +
    scale_x_log10() + facet_wrap( ~ type ) + scale_y_log10() +
    labs(x="Sample Size", y="MSE on test data", color="Package")
  ,width=10, height=10, dpi=400 )
results_new = results[results$mod!="RSNNS: rbf",]

#MSE.Test ~ MSE.Train
ggsave("Neural_Packages_MSE_test_vs_train.png",
  ggplot( results_new[results_new$n == round(10^4.5) & results_new$mod!="AMORE" & !is.na(results_new$MSE.Test),], aes(x=MSE.Train, y=MSE.Test, color=mod, group=mod) ) +
    geom_point() + facet_wrap( ~ type, scale="free" ) + geom_density2d() +
    labs(x="MSE on training data", y="MSE on test data", color="Package")
  ,width=10, height=10, dpi=400 )
ggplot( results_new[results_new$n == round(10^4.5) & !is.na(results_new$MSE.Test),], aes(x=MSE.Train, y=MSE.Test, color=mod, group=mod) ) +
  geom_point() + facet_wrap( ~ type, scale="free" ) + geom_density2d()


##########################################################################################
#Simulation #2: Sensitivity analysis for nnet.  Vary the number of hidden neurons and
# input neurons and again examine timing as well as MSE.  More focus should be on timing
# because the distribution may not be realistic (in 1d, it's easier to approximate)
#Conclusion: 
##########################################################################################

#params = data.frame(replication=1:10)
#params = merge( params, data.frame(type=c("half-quadratic", "quadratic", "linear")) )
params = data.frame(type=c("half-quadratic", "quadratic", "linear"))
params = merge(params, data.frame(hidden=1:10))
params = merge(params, data.frame(input=1:10))
params = merge(params, data.frame(n=round(10^seq(1,7,length.out=7))) )

results2 = matrix(0,nrow=0,ncol=8)
for( j in 1:(nrow(params)/100) ){
  temp = t(mapply(sim_nnet, type=params[1:100+(j-1)*100,]$type, n=params[1:100+(j-1)*100,]$n
    , hidden=params[1:100+(j-1)*100,]$hidden, input=params[1:100+(j-1)*100,]$input))
  results2 = rbind(results2, temp)
  write.csv(file="nnet_output.csv", results2, row.names=F)
  print(j)
}

results2 = read.csv(file="nnet_output.csv")

#Clean up results2 if not read from csv:
#temp = data.frame(results2)
#for(i in 1:ncol(temp) ) temp[,i] = do.call("c", temp[,i])
#temp$type = ifelse(temp$type==1,"half-quadratic",ifelse(temp$type==2,"quadratic","linear"))
#results2 = temp

#Time:
results2$input = paste("Input:", results2$input)
ggsave("nnet_time_results.png",
  ggplot(results2, aes(x=n, y=t, color=hidden) ) + geom_point() +
    scale_x_log10() + facet_wrap( ~ input ) + scale_y_log10() +
    labs(x="Sample Size", y="Time", color="Hidden Nodes") +
    theme( axis.text.x = element_text(angle=90, vjust=.5) )
  ,width=10, height=10, dpi=400 )
results2$input = as.numeric(gsub("Input: ","",results2$input))

#MSE:
results2$input = paste("Input:", results2$input)
ggsave("nnet_MSE_test_results.png",
  ggplot(results2, aes(x=n, y=MSE.Test, color=hidden) ) + geom_point() +
    scale_x_log10() + facet_wrap( ~ input ) + scale_y_log10() + geom_smooth(se=F) +
    theme(axis.text.x=element_text(angle=90, vjust=.5)) +
    labs(x="Sample Size", y="MSE on Test Set", color="Hidden Nodes")
  ,width=10, height=10, dpi=400 )
results2$input = as.numeric(gsub("Input: ","",results2$input))

ggplot(results2, aes(x=n, y=MSE.Test, color=input) ) + geom_point() +
  scale_color_continuous() + facet_wrap( ~ hidden ) + scale_y_log10() + scale_x_log10()
  geom_smooth(se=F)

#Test vs Train
ggsave("MSE_Test_vs_Train_All.png",
  ggplot(results2, aes(x=MSE.Train, y=MSE.Test, color=n) ) +
    geom_abline(intercept=0,slope=1, color="red", linetype=2) +
    geom_point() + scale_x_log10() + scale_y_log10() + scale_color_continuous(trans="log10")
  ,width=10, height=10, dpi=400 )
ggplot(results2[results2$n>=100000,], aes(x=MSE.Train, y=MSE.Test, color=n) ) +
  geom_abline(intercept=0,slope=1, color="red", linetype=2) +
  geom_point() + scale_color_continuous(trans="log10")

#Test vs. Train with histograms on axes:
ggplot(results2[results2$n>=100000,], aes(x=MSE.Train, y=MSE.Test, color=n) ) +
  geom_abline(intercept=0,slope=1, color="red", linetype=2) +
  geom_point() + scale_color_continuous(trans="log10") + geom_rug()
ymax = max(results2[results2$n>=100000,]$MSE.Test)
ymin = 0 #min(results2[results2$n>=100000,]$MSE.Test)
xmax = max(results2[results2$n>=100000,]$MSE.Train)
xmin = 0 #min(results2[results2$n>=100000,]$MSE.Train)
main = ggplot(results2[results2$n>=100000,], aes(x=MSE.Train, y=MSE.Test, color=n) ) +
  geom_abline(intercept=0,slope=1, color="red", linetype=2) +
  geom_point() + coord_cartesian(c(xmin,xmax),c(ymin,ymax)) +
  guides(color=F) + scale_color_continuous(trans="log10") +
  labs(x="MSE on training set", y="MSE on test set")
empty <- ggplot()+geom_point(aes(1,1), colour="white")+
         opts(axis.ticks=theme_blank(), 
              panel.background=theme_blank(), 
              axis.text.x=theme_blank(), axis.text.y=theme_blank(),           
              axis.title.x=theme_blank(), axis.title.y=theme_blank())
hist_top = ggplot(results2[results2$n>=100000,], aes(x=MSE.Train)) + geom_bar() +
  coord_cartesian(c(xmin,xmax)) + labs(x=NULL,y="Count\n") + scale_y_continuous(breaks=c())
hist_right = ggplot(results2[results2$n>=100000,], aes(x=MSE.Test)) + geom_bar() +
  coord_flip(c(ymin,ymax)) + labs(x=NULL,y="\nCount") + scale_y_continuous(breaks=c())
grid.arrange(hist_top, empty, main, hist_right, ncol=2, nrow=2, widths=c(4, 1), heights=c(1, 4))
#Export manually since it's not ggplot2

##########################################################################################
#Part 3: Fit very simple HFT models and see if nnet beats glm
#Conclusion: 
##########################################################################################

d = attach.big.matrix("/media/storage/Professional Files/Mines/MATH 598- Statistics Practicum/model_matrix.desc")
cnames = read.csv(file="/media/storage/Professional Files/Mines/MATH 598- Statistics Practicum/cnames.csv", stringsAsFactors=F)[,1]
temp = d[,cnames %in% c("Time","PriceDiff60SecAhead","PriceDiff1SecAhead","cvGroup"
                       ,"MicroPriceAdj","LogBookImbInside","Lag_600_Units","Lag_600_UnitsSELL")]
temp = data.frame(temp)
colnames(temp) = c("Time","PriceDiff60SecAhead","PriceDiff1SecAhead","cvGroup"
                       ,"MicroPriceAdj","LogBookImbInside","Lag_600_Units","Lag_600_UnitsSELL")
fit.glm = glm( PriceDiff1SecAhead ~ LogBookImbInside + MicroPriceAdj + Lag_600_Units + Lag_600_UnitsSELL
        ,data=temp[temp$cvGroup > 0,] )
fit.glm.pred = predict(fit.glm, newdata=temp)
fit.glm60 = glm( PriceDiff60SecAhead ~ LogBookImbInside + MicroPriceAdj + Lag_600_Units + Lag_600_UnitsSELL
        ,data=temp[temp$cvGroup > 0,] )
fit.glm60.pred = predict(fit.glm60, newdata=temp)

fit.nn.pred = list()
fit.nn60.pred = list()
fit.nn.pred.full = list()
fit.nn60.pred.full = list()
for(size in c(2,3,4,6,8,10,15,20) ){
  fit.nn = nnet( temp[temp$cvGroup > 0,5:8], temp[temp$cvGroup > 0,3], size=size, linout=TRUE )
  fit.nn.pred[[length(fit.nn.pred)+1]] =
    predict(fit.nn, newdata=temp[apply( temp, 1, function(x)sum(is.na(x)) )==0,5:8])
  fit.nn60 = nnet( temp[temp$cvGroup>0,5:8], temp[temp$cvGroup>0,2], size=size, linout=TRUE )
  fit.nn60.pred[[length(fit.nn60.pred)+1]] =
    predict(fit.nn60, newdata=temp[apply( temp, 1, function(x)sum(is.na(x)) )==0,5:8])
  fit.nn = nnet( temp[temp$cvGroup > 0,5:8], temp[temp$cvGroup > 0,3], size=size, linout=TRUE, maxit=100000 )
  fit.nn.pred.full[[length(fit.nn.pred.full)+1]] =
    predict(fit.nn, newdata=temp[apply( temp, 1, function(x)sum(is.na(x)) )==0,5:8])
  fit.nn60 = nnet( temp[temp$cvGroup>0,5:8], temp[temp$cvGroup>0,2], size=size, maxit=100000, linout=TRUE )
  fit.nn60.pred.full[[length(fit.nn60.pred.full)+1]] =
    predict(fit.nn60, newdata=temp[apply( temp, 1, function(x)sum(is.na(x)) )==0,5:8])
}
fit.nn.pred = do.call( "cbind", fit.nn.pred )
fit.nn60.pred = do.call( "cbind", fit.nn60.pred )
fit.nn.pred.full = do.call( "cbind", fit.nn.pred.full )
fit.nn60.pred.full = do.call( "cbind", fit.nn60.pred.full )
write.csv(file="fit.nn.pred.csv", fit.nn.pred, row.names=F)
write.csv(file="fit.nn60.pred.csv", fit.nn60.pred, row.names=F)
write.csv(file="fit.nn.pred.full.csv", fit.nn.pred.full, row.names=F)
write.csv(file="fit.nn60.pred.full.csv", fit.nn60.pred.full, row.names=F)

fit.nn.pred = read.csv(file="fit.nn.pred.csv")
fit.nn60.pred = read.csv(file="fit.nn60.pred.csv")
fit.nn.pred.full = read.csv(file="fit.nn.pred.csv")
fit.nn60.pred.full = read.csv(file="fit.nn60.pred.csv")

perf = data.frame(Model="GLM",Hidden=NA,Perf=eval_preds( fit.glm.pred )[[1]])
perf$Model = as.character(perf$Model)
perf = rbind(perf,c("Persistence",NA,eval_preds(0)[[1]]) )
for(i in 1:ncol(fit.nn.pred) )
  perf = rbind(perf,c("nnet",c(2,3,4,6,8,10,15,20)[i],eval_preds( c(fit.nn.pred[,i],NA) )[[1]]))
for(i in 1:ncol(fit.nn.pred.full) )
  perf = rbind(perf,c("nnet conv",c(2,3,4,6,8,10,15,20)[i],eval_preds( c(fit.nn.pred.full[,i],NA) )[[1]]))
perf60 = data.frame(Model="GLM",Hidden=NA,Perf=eval_preds( fit.glm60.pred, price_diff=d[,4] )[[1]])
perf60$Model = as.character(perf60$Model)
perf60 = rbind(perf60,c("Persistence",NA,eval_preds(0, price_diff=d[,4])[[1]]) )
for(i in 1:ncol(fit.nn.pred) )
  perf60 = rbind(perf60,c("nnet",c(2,3,4,6,8,10,15,20)[i],eval_preds( c(fit.nn60.pred[,i],NA), price_diff=d[,4] )[[1]]))
for(i in 1:ncol(fit.nn.pred.full) )
  perf60 = rbind(perf60,c("nnet conv",c(2,3,4,6,8,10,15,20)[i],eval_preds( c(fit.nn60.pred.full[,i],NA), price_diff=d[,4] )[[1]]))
write.csv(perf, "perf.csv")
write.csv(perf, "perf60.csv")

pers = data.frame(Model="Persistence", Hidden=c(0,20), Perf=perf$Perf[perf$Model=="Persistence"])
glmPlot = data.frame(Model="GLM", Hidden=c(0,20), Perf=perf$Perf[perf$Model=="GLM"])
ggsave("performance_of_1sec_nnet.png",
  ggplot( perf[perf$Model %in% c("nnet", "nnet conv"),], aes(x=Hidden, y=Perf, color=Model) ) +
    geom_point() + geom_line(data=glmPlot) + geom_line(data=pers) +
    labs(x="Hidden Nodes", y="MSE on test set")
  ,width=10, height=10, dpi=400 )

pers = data.frame(Model="Persistence", Hidden=c(0,20), Perf=perf60$Perf[perf60$Model=="Persistence"])
glmPlot = data.frame(Model="GLM", Hidden=c(0,20), Perf=perf60$Perf[perf60$Model=="GLM"])
ggsave("performance_of_60sec_nnet.png",
  ggplot( perf60[perf60$Model %in% c("nnet", "nnet conv"),], aes(x=Hidden, y=Perf, color=Model) ) +
    geom_point() + geom_line(data=glmPlot) + geom_line(data=pers) +
    labs(x="Hidden Nodes", y="MSE on test set")
  ,width=7, height=7, dpi=400 )

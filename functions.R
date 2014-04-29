library(car)
library(Rcpp)
library(inline)
library(RcppArmadillo)
library(reshape)
library(plyr)
library(ggplot2)
library(scales)
require(bigmemory)
library(nnet)
library(mgcv)
require(biglm)
require(sqldf)
require(glmnet)
require(randomForest)
require(rpart)
require(gbm)
#library(AMORE)
#library(pls)
#library(neuralnet)

#preds should be a vector that has a length=nrow(d).  It contains prediction values for the days/times of interest.
#d should be the big.matrix object.
#cnames is the colnames of d
#type should be 1 or 60 depending on the type of model: 1-second or 60-second forecasting
#full: Returns a list of performance by time and performance by price level if T.
eval_preds = function( preds, d, cnames, type, full=F ){
  #Check for NAs.  If NAs exist, overwrite them with 0's (i.e. predict persistence, or a change in price of 0).
  if( any(is.na(preds)) ){
    warning(paste0("No NAs allowed in predictions!  ",sum(is.na(preds))," NA's replaced with MicroPrice at that time."))
    preds[is.na(preds)] = d[is.na(preds),which(cnames=="MicroPrice")]
  }
  #Identify which rows need to be analyzed, pull in true values to model against
  eval.rows = d[,which(cnames=="Diff")]==1
  if(type==1) price_diff = d[,which(cnames=="PriceDiff1SecAhead")]
  if(type==60) price_diff = d[,which(cnames=="PriceDiff60SecAhead")]
  if(type!=1 & type!=60) stop("Invalid type supplied")
  time = d[,which(cnames=="Time")]
  price = d[,which(cnames=="MicroPrice")]
  day = d[,which(cnames=="day")]
  #Create a dataframe with all the relevant variables
  d.eval = data.frame( err=preds-price_diff, time, price, price_diff, day )
  d.eval = d.eval[eval.rows,]
  
  #Aggregate the Wed/Thurs results
  d.agg.tot = sqrt(mean(d.eval$err[d.eval$day %in% c(3,4)]^2, na.rm=T))
  #Aggregate the results by day
  d.agg.day = ddply(d.eval, "day", function(df){sqrt(mean(df$err^2, na.rm=T))})
  
  if(full){
    #Aggregate performance over time:
    d.eval$time = floor( d.eval$time/15/60 )*15*60
    d.agg.t = ddply( d.eval, "time", function(df){
      data.frame(Base.RMSE=sqrt(mean(df$price_diff^2,na.rm=T))
                 ,Model.RMSE = sqrt(mean(df$err^2,na.rm=T)) )
    } )
    
    #Aggregate performance over actual MicroPrice:
    d.eval$price = floor( d.eval$price*100 )/100
    d.agg.p = ddply( d.eval, "price", function(df){
      data.frame(Base.RMSE=sqrt(mean(df$price_diff^2,na.rm=T))
                 ,Model.RMSE = sqrt(mean(df$err^2,na.rm=T)) )
    } )
    
    #Aggregate performance over predicted value:
    d.eval$preds = floor( d.eval$err + d.eval$price_diff )
    d.agg.r = ddply( d.eval, "preds", function(df){
      data.frame(Base.RMSE=sqrt(mean(df$price_diff^2,na.rm=T))
                 ,Model.RMSE = sqrt(mean(df$err^2,na.rm=T)) )
    } )
    
    #return performance aggregated by time, current price, and predicted value
    return( list( d.agg.tot, d.agg.day, d.agg.t, d.agg.p, d.agg.r ) )
  }
  
  return(list(d.agg.tot,d.agg.day))
}

#NOTE: cnames should be in memory before running this function with a bigmatrix object!!!
#d: big.matrix object or data.frame (although data.frame arguments haven't been tested as extensively).
#ind_vars: names of variables to be used in the model (as a character string).  If multiple models are to be fit, this should be a list where each element is a character string.
#dep_var: Either "PriceDiff1SecAhead" or "PriceDiff60SecAhead"
#price_decay: How the case weights should decrease as a function of price level.  If the current price is $94, then observations at $95 and $93 will have weight 1*price.decay.
#day.decay: How should the case weights decrease as a function of day?  For k days in the past, case weights are decayed by (day.decay)^k
#time.decay: How should the case weights decrease as a function of time?  For k seconds in the past, case weights are decayed by (time.decay)^k
#outcry.decay: How should the case weights decrease as a function of outcry period?  If the observation is the same as the current outcry, don't decay, otherwise multiply by this factor.
#hour.decay: How should the case weights decrease as a function of time of day?  
#step.size: How frequently should models be built (in seconds)?  Smaller values should be more accurate but take longer to fit.
#chunk.rows: Controls how many rows are read at one time for the bigglm algorithm.
#type: Should be either "GLM", "nnet", "gam", or "glmnet".  Either a linear regression, neural network, GAM, or penalized glm model is then used.  If fitting multiple models, this can be a list where each element is the string corresponding to the type for the ith model.
#size: How many hidden nodes to fit with nnet?  Ignored for type!="nnet".  If multiple models are to be fit, this should be a list where each element is a numeric.
#min.wt: What obs should be ignored?  Ignored if their weight is less than max(Weight)*min.wt
#repl: How many neural networks (with randomized weights) should be fit?  The network with best fit on training data is kept.  Ignored for type!="nnet"
#combine.method: How should the multiple outputs be combined?  If "glm" or "nnet" (with quotes!) then a linear regression or neural network is built on the output.  If a function, that function is applied to each row of the predictions.
#plot.fl: If set to true, two plots are output that show the performance of the model (relative to persistence) over time and over price level.
weighted_model = function(d, ind_vars, dep_var="PriceDiff1SecAhead"
                          ,price.decay=1, day.decay=1, time.decay=1, outcry.decay=0.5, hour.decay=0.8, step.size=2.25*60*60
                          ,chunk.rows=25000, type="GLM", size=10, min.wt=0, repl=5, combine.method=mean
                          ,plot.fl=FALSE){
  #Check for results.csv (for output), if it doesn't exist then create it:
  if( !any( list.files()=="results.csv") ){
    pred.1 = eval_preds(0,d,cnames,type=1)
    pred.60 = eval_preds(0,d,cnames,type=60)
    out = data.frame(id=1,forecast=1,type="pers",ind_vars=NA,step.size=NA,size=NA,repl=NA,params=NA,t=NA,RMSE=pred.1[[1]], RMSE.W=pred.1[[2]][3,2], RMSE.R=pred.1[[2]][4,2], RMSE.F=pred.1[[2]][5,2])
    out = rbind(out,data.frame(id=1,forecast=60,type="pers",ind_vars=NA,step.size=NA,size=NA,repl=NA,params=NA,t=NA,RMSE=pred.60[[1]], RMSE.W=pred.60[[2]][3,2], RMSE.R=pred.60[[2]][4,2], RMSE.F=pred.60[[2]][5,2]))
    write.csv(file="results.csv", out, row.names=F)
  }

  #Check that the basic columns are present:
  if(is.data.frame(d)) cnames = colnames(d)
  needed.cols = c("Time", "MicroPrice", "Diff", "Weight", "day", "Outcry")
  if(!all(needed.cols %in% cnames))
    stop(paste0("Missing required column(s): ", paste(needed.cols[!(needed.cols %in% cnames)], collapse=", ") ))
  
  #Maintain compatability: convert arguments of length 1 into lists, if applicable
  #If ind_vars is a vector, make it a list of length 1:
  if(is.character(ind_vars)){ ind_vars = list(ind_vars) }
  #If type is a character, make it a list  
  if(length(type)==1){ type=as.list(rep(type,length(ind_vars)))}
  #If size is a numeric, make
  if(length(size)==1){ size=as.list(rep(size,length(ind_vars)))}
  #No combine.method required if only running one model:
  if(length(ind_vars)==1) combine.method=""
  
  #Set up Start and results for output purposes:
  Start = Sys.time()
  results = read.csv(file="results.csv", stringsAsFactors=F)
  ID = max(results$id)+1
  
  #If not all variables are in cnames, then quit now instead of running and then erroring out.
  if( !all(do.call("c",ind_vars) %in% cnames) ){
    stop("Not all variables in ind_vars are in cnames")
  }
  #glmnet needs at least two independent variables, so if there's only one revert to GLM:
  for(i in 1:length(ind_vars) ){
    if(type[[i]]=="GLMnet" & length(ind_vars[[i]])==1){
      warning("GLMnet cannot run with one predictor, reverting to GLM")
      type[[i]] = "GLM"
    }
  }
  
  #Note: outcry starts at 6:45 and ends at 1:30. If step.size is such that 6:45 and 1:30 are not
  #divisible by it, then you may have weird estimates on those boundaries (since outcry is assumed
  #to be on or off over the whole step.size period).
  if(any(round(c(6.75,13.5)*60*60/step.size)!=round(c(6.75,13.5)*60*60/step.size)) & outcry.decay!=1)
    warning("Step size may lead to invalid predictions as it doesn't split outcry hours well!")
  
  #Reorder ind_var_names (sort based on cnames for easy prediction):
  ind_vars = lapply(ind_vars,function(x)x[order( match(x,cnames) )])
  col.inx = lapply(ind_vars, function(x)cnames %in% c(dep_var,x,"Time","MicroPrice","day","Outcry","Weight"))
  col.inx = lapply(col.inx, function(x)c(x, rep(F,ncol(d)-length(cnames))))
  #preds will hold the predictions from each individual model in a column and the final prediction in the last column
  preds = matrix(0,nrow=nrow(d),ncol=length(ind_vars)+1)
  colnames(preds) = c(paste0("Model",1:length(ind_vars)),"Final")
  
  #NOT CURRENTLY USED!  biglm model implementation is necessary when using alot of independent variables, but will be slower.  So, we're using glm.
  #Define function to read data. This is needed for the bigglm model. This function should return NULL when passed TRUE (and
  #reset the point we're reading from) and should return the next chunk of data when passed FALSE.
#  read.d = function(reset){
#    if(reset){
#      skip.rows<<-0 #Use <<- to assign to global environment
#      return(NULL)
#    }
#    row.inx = (skip.rows+1):min(skip.rows+chunk.rows,nrow(d))
#    #filter controls which rows are useable for the current model:
#    while( sum(filter[row.inx])==0 & skip.rows < nrow(d)-1 ){
#      skip.rows <<- skip.rows + chunk.rows
#      row.inx = (skip.rows+1):min(skip.rows+chunk.rows,nrow(d))
#    }
#    if(skip.rows>=nrow(d)-1) return(NULL)
#    out = data.frame( d[row.inx,col.inx] )
#    #Filter out NA rows:
#    out = out[apply(out,1,function(x){sum(is.na(x))})==0,]
#    colnames(out) = cnames[col.inx]
#    skip.rows <<- skip.rows + chunk.rows
#    #Stop processing once filter becomes F (i.e. reached end of training set)
#    if(any(!filter[row.inx])) skip.rows<<-nrow(d)
#    return(out[filter[row.inx],])
#  }
  
  #Create the model formula, initialize the time.loop:
  form = lapply(ind_vars,function(x){
    as.formula( paste0( dep_var, "~", paste(x,collapse="+") ) )
  } )
  time.loop = 24*60*60*2 + seq(0,24*60*60*3,by=step.size)
  #Remove times after end of dataset:
  time.loop = time.loop[time.loop<max(d[,which(cnames=="Time")])]
  
  #Build the model for each time chunk:
  for( i in time.loop ){
    
    #Only use rows which are more than 60 seconds before the first prediction (ensures you don't use data you're not supposed to have)
    filter = d[,which(cnames=="Time")]<=(i-60)
    #Define which rows you'll be predicting
    pred.filter = d[,which(cnames=="Time")]>i & d[,which(cnames=="Time")]<=i+step.size
    #If MicroPrice is NA on some row, don't predict for that row (this code generates a warning without this).  Only one row has NA, and it's on Friday
    pred.filter = pred.filter & !is.na(d[,which(cnames=="MicroPrice")])
    #Update the case weights:
    d[filter,which(cnames=="Weight")] =
      #Use the last observed MicroPrice and compare that to all MicroPrices (larger diff=>smaller weight)
      price.decay^(abs(d[filter,which(cnames=="MicroPrice")][sum(filter)]-d[filter,which(cnames=="MicroPrice")]))*
      #Use the first time and compare that to all times (larger diff=>smaller weight)
      time.decay^(abs(i-d[filter,which(cnames=="Time")])/60/60)*
      #Use the outcry for the first prediction obs and compare that to all outcries (if diff=>smaller weight)
      outcry.decay^(abs(d[sum(filter)+1,which(cnames=="Outcry")]-d[filter,which(cnames=="Outcry")]))*
      #Use the day for the first prediction obs and compare that to all days (larger diff=>smaller weight)
      day.decay^(abs(d[sum(filter)+1,which(cnames=="day")]-d[filter,which(cnames=="day")]))*
      #Use the hour for the first prediction obs and compare that to all hours (larger diff=>smaller weight)
      hour.decay^(pmin(
        (floor(d[sum(filter)+1,which(cnames=="Time")]%%(24*60*60)/3600) #Current Hour
        -floor(d[filter,which(cnames=="Time")]%%(24*60*60)/3600))%%24 #Observation Hour
       ,(floor(d[filter,which(cnames=="Time")]%%(24*60*60)/3600) #Observation Hour
        -floor(d[sum(filter)+1,which(cnames=="Time")]%%(24*60*60)/3600))%%24 #Current Hour
      ))
    d[filter,which(cnames=="Weight")] = d[filter,which(cnames=="Weight")]/max(d[filter,which(cnames=="Weight")])
    
    #Clear out all the other weights, just in case:
    d[!filter,which(cnames=="Weight")] = 0
    
    #Fit a GLM if that's what's desired, using bigglm
    #NOT USED!  We now use glm for speed and convenience (since we aren't using a ton of independent variables)
    #if(type=="GLM") fit = bigglm( form, read.d, weights=Weight~1 )
    
    #Loop through all of the different models specified (could just be one).  iVar is the iterator for the model number
    for( iVar in 1:length(ind_vars) ){
      #Set up model dataframe
      cols = which(cnames %in% c(dep_var,ind_vars[[iVar]]))
      cols = c(cols, which(cnames=="Weight"))
      data = data.frame(d[filter,cols])
      colnames(data) = cnames[cols]
      #Remove rows of data with small weights to speed up modeling
      data = data[data$Weight>min.wt*max(data$Weight),]
      
      if(type[[iVar]]=="GLM") fit = glm(form[[iVar]],data=data)
      
      #Fit a neural network if that's what's desired. Note: the necessary data matrix is brought into RAM in this case. Be careful!
      if(type[[iVar]]=="nnet"){
        bestfit = nnet( form[[iVar]], weights=Weight, data=data, linout=TRUE, maxit=100000, size=size[[iVar]], trace=F )
        j = 1
        #Run nnet with several different initial weights if repl>1:
        while(j < repl){
          fit = nnet( form[[iVar]], weights=Weight, data=data, linout=TRUE, maxit=100000, size=size[[iVar]], trace=F )
          if(fit$value<bestfit$value) bestfit=fit
          j = j+1
        }
        if(bestfit$convergence!=0) warning("Neural network failed to converge!")
        fit = bestfit
      }

      #Fit a GAM if that's what's desired:
      if(type[[iVar]]=="gam"){
        #Build the formula:
        form[[iVar]] = paste( dep_var, "~ ")
        for( j in ind_vars[[iVar]] ){
          #Use a linear term if you don't have enough unique values:
          if(length(unique(data[,j]))<=5)
            form[[iVar]] = paste0(form[[iVar]], j, "+")
          else
            form[[iVar]] = paste0(form[[iVar]], "s(", j, ") +")
        }
        #Remove the trailing "+", convert to formula:
        form[[iVar]] = as.formula( gsub("\\+$","",form[[iVar]]) )
        fit = gam( form[[iVar]], data=data, weights=Weight )
      }
      
      #Fit a penalized GLM if that's what's desired:
      if(type[[iVar]]=="GLMnet"){
        na.rows = is.na(apply(data, 1, sum))
        fit = cv.glmnet( x=as.matrix(data[!na.rows,ind_vars[[iVar]]]), y=as.matrix(data[!na.rows,dep_var[[iVar]]]), weights=data[!na.rows,"Weight"] )
      }
      
      #Make predictions:
      pred.d = data.frame(d[filter | pred.filter,col.inx[[iVar]]]) #covariates used for predictions
      colnames(pred.d) = cnames[col.inx[[iVar]]] #Give the covariates the correct colnames
      if(type[[iVar]] %in% c("GLM", "nnet", "gam")){
        preds[filter | pred.filter,iVar] = predict(fit, newdata=pred.d) #Update preds (in the correct rows) with the new predictions
      } else {
        #By default, this uses the optimal cv value for lambda
        preds[filter | pred.filter,iVar] = predict(fit, newx=as.matrix(pred.d[,ind_vars[[iVar]]]), s="lambda.min")
      }
    } #End Model Loop
        
    #Combine predictions from various models to make final prediction:
    #if only one prediction, use that:
    if(ncol(preds)==2){
      preds[pred.filter,2] = preds[pred.filter,1]
      print(paste0("Time ",i," completed out of total time ",max(time.loop)))
      next
    }
    
    #If a function is supplied, use that function to combine the predictions (usually mean)
    if(is.function(combine.method)){
      preds[pred.filter,ncol(preds)] = apply(preds[pred.filter,-ncol(preds), drop=F], 1, combine.method)
      print(paste0("Time ",i," completed out of total time ",max(time.loop)))
      next
    }

    #Combine the output using a glm
    if(combine.method=="glm"){
      combine.fit = glm( Y ~ ., data=data.frame(preds[filter,-ncol(preds)], Y=d[filter,which(cnames==dep_var)]))
      preds[pred.filter,ncol(preds)] = predict(combine.fit, newdata=data.frame(preds[pred.filter,]))
    }

    #Use a neural network to choose which model to use
    if(combine.method=="nnet"){
      combine.fit = nnet( x=preds[filter,-ncol(preds)], y=d[filter,which(cnames==dep_var)], size=10, linout=T, maxit=10000, trace=F)
      preds[pred.filter,ncol(preds)] = predict(combine.fit, newdata=data.frame(preds[pred.filter,]))
    }
    
    #Use a classification tree to choose the best model:
    if(combine.method=="classify"){
      err = abs(preds[filter,-ncol(preds)] - d[filter,which(cnames==dep_var)])
      best.model = apply(err, 1, which.min)
      rm(err)
      X = d[,which(cnames %in% c("MicroPrice","Time","Outcry"))]
      colnames(X) = cnames[which(cnames %in% c("MicroPrice","Time","Outcry"))]
      #Convert time to seconds past midnight rather than seconds since start of dataset:
      X[,"Time"] = X[,"Time"]-floor(X[,"Time"]/(24*60*60))
      fit = rpart(as.factor(Y) ~ ., data=data.frame(X[filter,], Y=best.model) )
      wts = predict(fit, newdata=data.frame(X[pred.filter,]))
      preds[pred.filter,ncol(preds)] = apply(preds[pred.filter,-ncol(preds)]*wts, 1, sum)
    }

    print(paste0("Time ",i," completed out of total time ",max(time.loop)))
  } #End Time Loop
  
  #Analyze model performance
  t = as.numeric(difftime(Sys.time(), Start, units="mins"))
  if( dep_var %in% c("PriceDiff1SecAhead", "PriceDiff60SecAhead") ){
    if(dep_var=="PriceDiff1SecAhead") perf=eval_preds(preds[,ncol(preds)], d, cnames, 1, full=T)
    if(dep_var=="PriceDiff60SecAhead") perf=eval_preds(preds[,ncol(preds)], d, cnames, 60, full=T)
  } else {
    return(preds[,ncol(preds)])
  }
  #Add the relevant results of this run to the results data.frame
  results = rbind(results, c(id=ID, ifelse(dep_var=="PriceDiff1SecAhead",1,60), type=do.call("paste",type)
     ,ind_vars = paste(ind_vars,collapse=","), step.size=step.size, size=paste(size,collapse=","), repl=repl
     ,params=paste0("price.decay=",price.decay,",day.decay=",day.decay,",time.decay=",time.decay,",outcry.decay=",outcry.decay,"hour.decay=",hour.decay,"repl=",repl,",min.wt=",min.wt,"combine.method=",as.character(combine.method))
     ,t=t, RMSE=perf[[1]], RMSE.W=perf[[2]][3,2], RMSE.R=perf[[2]][4,2], RMSE.F=perf[[2]][5,2] ) )
  #Write out the relevant results of this run to the results.csv file
  write.csv(results, "results.csv", row.names=F)
  #Write out the relevant results to another file (in case results.csv gets overwritten/corrupted)
  write.csv(results, file=paste0("results_",ID,".csv"), row.names=FALSE )
  #Create some plots of performance, if plot.fl=TRUE
  if(plot.fl){
    ggsave( paste0("ID=",ID,"_time.png"), ggplot(perf[[3]], aes(x=time, y=Model.RMSE/Base.RMSE) ) + geom_point() )
    ggsave( paste0("ID=",ID,"_price.png"), ggplot(perf[[4]], aes(x=price, y=Model.RMSE/Base.RMSE) ) + geom_point() )
    ggsave( paste0("ID=",ID,"_preds.png"), ggplot(perf[[5]], aes(x=preds/1000, y=Model.RMSE/Base.RMSE) ) + geom_point() + labs(x="Predicted Change") + geom_hline(xintercept=1,color="red",linetype=2) )
  }

  return(preds)
}

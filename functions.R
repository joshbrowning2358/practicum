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

#form: specify the model formula, i.e. Y ~ X1 + X2 + X3.  Note that "." notation is supported.
#data: a dataframe containing the data for which the model is desired.
#hidden: the number of hidden neurons in the network.  The package only supports one hidden layer.
#steps: how many iterations should be ran?  Note this may need adjustment based on convergence.
#print: how many times should the function print the current fit number and LMS?
#...: other parameters to be passed into newff or train in the AMORE package.
fit.nn = function( form, data, hidden, steps=1000, print=10, learning.rate.global=1e-2, momentum.global=.5, report=T
      ,error.criterium="LMS", Stao=NA, hidden.layer="tansig", output.layer="purelin", method="ADAPTgdwm" ){
  #Parse the form object to determine number of input neurons and relevant data
  if( !is.formula(form) ) stop("Formula incorrectly specified")
  form = as.character( form )
  indCol = (1:ncol(data))[colnames(data)==form[2]]
  depVars = strsplit(form[3],"+", fixed=T)
  #period matches everything:
  if( depVars=="." ){
    depCols = (1:ncol(data))[-indCol]
  } else {
    depVars = sapply( depVars, function(x)gsub(" ","",x) )
    depCols = (1:ncol(data))[colnames(data) %in% depVars]
  }
  
  #Fit the neural network
  mod = newff( n.neurons=c(length(depCols)+1,hidden,1), learning.rate.global=1e-2, momentum.global=0.5,
    error.criterium="LMS", Stao=NA, hidden.layer="tansig",
    output.layer="purelin", method="ADAPTgdwm")
  mod.fit = train( mod, T=as.matrix(data[,indCol]), P=cbind(1,data[,depCols]), n.shows=print, show.step=steps/print, report=report )

  #Pull off the output weights
  weights = lapply( mod.fit$net$neurons, function(x)x$weights )
  hidden.wts = do.call( "rbind", weights[1:hidden] )
  output.wts = weights[[hidden+1]]
  return(list(hidden.wts=hidden.wts, output.wts=output.wts, activation.function=hidden.layer, form=form))
}

fit.glmnet = function(form, data, lambda=1*(0.9)^(0:100), family=c("gaussian","binomial","poisson","multinomial","cox","mgaussian")
  ,alpha = 1, nlambda = 100, maxit=100000 ){
  #Parse the form object to determine number of input neurons and relevant data
  if( !is.formula(form) ) stop("Formula incorrectly specified")
  form = as.character( form )
  indCol = (1:ncol(data))[colnames(data)==form[2]]
  depVars = strsplit(form[3],"+", fixed=T)
  #period matches everything:
  if( depVars=="." ){
    depCols = (1:ncol(data))[-indCol]
  } else {
    depVars = sapply( depVars, function(x)gsub(" ","",x) )
    depCols = (1:ncol(data))[colnames(data) %in% depVars]
  }
  
  filter = apply( data[,c(depCols,indCol)], 1, function(x){all(!is.na(x))} )
  if(any(!filter)){
    warning("Missing values in data frame.  Removed for analysis")
    data = data[filter,]
  }
  
  glmnet(x=as.matrix(data[,depCols]), y=as.matrix(data[,indCol]), lambda=lambda, family=family, alpha=alpha, nlambda=nlambda, maxit=maxit)
}

#mod: A list as output by fit.nn.  It should contain hidden.wts, output.wts, activation.function and form.  Custom activation functions are not supported!
#newdata: the data for which a prediction is desired.
predict.nn = function( mod, newdata ){
  newdata = cbind( 1, newdata )
  depVars = strsplit(mod$form[3],"+", fixed=T)
  depVars = sapply( depVars, function(x)gsub(" ","",x) )  
  newdata = newdata[,colnames(newdata) %in% c("1",depVars)]
  if( mod$activation.function=="tansig" ){
    neurons = tanh( as.matrix(newdata) %*% t(mod$hidden.wts) )
    preds = neurons %*% mod$output.wts
  }
  if( mod$activation.function=="purelin" ){
    neurons = as.matrix(newdata) %*% t(mod$hidden.wts)
    preds = neurons %*% mod$output.wts
  }
  if( mod$activation.function=="sigmoid" ){
    neurons = 1/(1+exp(-as.matrix(newdata) %*% t(mod$hidden.wts)))
    preds = neurons %*% mod$output.wts
  }
  if( mod$activation.function=="hardlim" ){
    neurons = ifelse(as.matrix(newdata) %*% t(mod$hidden.wts)>0,1,0)
    preds = neurons %*% mod$output.wts
  }
  return(preds)
}

#d: Dataset of interest.  Should contain all training and test observations!
#cvGroup: Specify the cross-validation group number for the training data (1 to # of cv groups, typically 10).  -1=test data, 0=ignored.
#indCol: The column of d containing the independent variable.
#model: A string containing the model specification.  data arguments will be ignored, and the function used is required to have a formula argument.
#pred.cols: Some algorithms support multiple predictions (multiple averaged models, for example).  pred.cols controls how many models should be estimated, and estimations are choosen in a meaningful way.  Defaults to 1 (or 10 if gbm or fit.glmnet)
#Currently supported functions: fit.nn (defined above), neuralnet, gbm, randomForest, glm, lm, rpart, glmnet, pcr
cvModel = function(d, cvGroup, indCol, model="neuralnet(Y ~ X1 + X2 + X3 + X4 + X5, hidden=4, err.fct='sse')", pred.cols=1+9*grepl("(^fit.glmnet|^gbm)",model) ){
  ensem = data.frame( matrix(0, nrow=nrow(d), ncol=pred.cols ) )
  #Set up the rownames of ensem to match d.  This makes inserting in predicted values much easier later:
  rownames(ensem) = 1:nrow(d)
  rownames(d) = 1:nrow(d)
  
  model = gsub("data=[A-Za-z0-9_.]*", "data=train", model )
  if(!grepl( "data=", model )) model = sub( ")", ", data=train )", model )
  
  #Store the models, if desirable
  mods = list()
  
  #Set up the model formula and rename the columns of d appropriately:
  #Holdout each cv-group in turn:
  for( i in sort(unique(cvGroup[cvGroup>0])) ){
    train = d[!cvGroup %in% c(-1,i),]
    predict = d[cvGroup %in% c(-1,i),-indCol]

    #Pass the arguments to cvNnet into the neuralnet fit:
    fit = eval( parse( text=model) )
    
    #Predict based on the model used:
    if( grepl("^neuralnet", model) )
      preds = compute(fit, predict)$net.result
    if( grepl("^fit.nn", model) ){
      preds = predict.nn(fit, newdata=predict)
      mods[[length(mods)+1]] = fit
    }
    if( grepl("^gbm", model) ){
      if(pred.cols==1) warning("Only generating one prediction for a model that allows many!")
      #Exponentially space out the trees for prediction, but round to nearest integer and remove duplicates:
      tree.list = unique( round( exp(seq(0,log(fit$n.trees),length.out=pred.cols)) ) )
      preds = data.frame(predict(fit, newdata=predict, n.trees=tree.list))
      colnames(ensem) = paste0("gbm_trees",tree.list)
      #Remove extra columns in ensem, if applicable
      ensem = ensem[,1:ncol(preds)]
    }
    if( grepl("^randomForest", model) )
      preds = data.frame(predict(fit, newdata=predict))
    if( grepl("^([g]*lm|rpart)", model) ){
      preds = data.frame(predict(fit, newdata=predict))
      mods[[length(mods)+1]] = fit$coeff
    }
    if( grepl("^pcr", model) ){
      #Prediction returns a 3-dimensional array (observations x prediction_type (always 1 here) x components).  Extract and return all components
      preds = apply(predict(fit, newdata=predict, type="response"), 3, identity)
      if( ncol(preds)!=ncol(ensem) ){
        warning(paste0("Overwriting pred.cols to return all components: ",pred.cols,"->",ncol(preds)))
        if(ncol(ensem)<ncol(preds)) ensem = data.frame( matrix(0, nrow=nrow(d), ncol=ncol(preds)) )
        if(ncol(ensem)>ncol(preds)) ensem = ensem[,1:ncol(preds)]
      }
      colnames(ensem) = colnames(preds)
      preds = data.frame(preds)
    }
    if( grepl("^fit.glmnet", model) ){
      if(pred.cols==1) warning("Only generating one prediction for a model that allows many!")
      preds = data.frame(predict(fit, newx=as.matrix(predict)))
      col.index = round(seq(1,ncol(preds),length.out=pred.cols))
      preds = preds[,col.index]
      colnames(ensem) = paste0("glmnet_lambda",round(fit$lambda[col.index],4))
    }
    colnames(ensem) = gsub("\\.", "d", colnames(ensem))
    rownames(preds) = rownames(predict)
    
    #Insert the predicted values for the cv group into the ensem data.frame.
    pred.index = (1:nrow(ensem))[cvGroup==i]
    ensem[pred.index,] = preds[rownames(preds) %in% pred.index,]
    
    #Insert the predicted values for the test group into the ensem data.frame, but divide by the number of cv-folds (since each fold has a diff. prediction)
    test.index = (1:nrow(ensem))[cvGroup==-1]
    ensem[test.index,] = ensem[test.index,] + preds[rownames(preds) %in% test.index,]/(length(unique(cvGroup))-1)
    print(paste0("Model ",i,"/",length(unique(cvGroup[cvGroup>0]))," has finished"))
  }
  if(length(mods)==0) return(ensem)
  return(list(ensemble=ensem, models=mods))
}

make_raw = function(){
  options(digits.secs=6)
  
  raw = read.csv(file="C:/Users/jbrowning/Desktop/To Home/Personal/Mines Files/MATH 598- Statistics Practicum/Data/20131104.CLZ3.log")
  raw = rbind(raw, read.csv(file="C:/Users/jbrowning/Desktop/To Home/Personal/Mines Files/MATH 598- Statistics Practicum/Data/20131105.CLZ3.log") )
  raw$Time = as.POSIXct(strptime(x=raw$Time, format="%Y%m%d %H:%M:%OS"))

  #Data frame with prices:
  price = raw[raw$RestingSide=="",]
  price = price[,c(1:31,35:37)]
  #Round price values to bid/ask values to pennies:
  price[,1:10*3+1] = apply( price[,1:10*3+1], c(1,2), function(x)round(x,2) )
  
  #Data frame with transactions:
  orders = raw[raw$RestingSide!="",]
  orders = orders[,c(1,32:34)]
  
  rm(raw); gc()
  return(list(price=price, orders=orders))
}

#To bring in lagged times, use Rcpp (so you can loop efficiently):
#prices (the input, must be a matrix) should be two columns: time (numeric, in seconds) and variable to lag.
#Outputs shift_price, a matrix, which contains lagged values at 1s, 2s, 3s, 4s, 5s.
#6 indicators:
#i: Current row of matrix.  Take this microprice and impute it ahead 1s, 2s, 3s, 4s, 5s
#secj, j=1:5: Index of row corresponding to j seconds ahead of ith row.  Increments up as i increments
load_lag_times_cxx = cxxfunction(signature(prices="numeric"), plugin="RcppArmadillo", body="
  arma::mat price = Rcpp::as<arma::mat>(prices);
  int n = price.n_rows;
  arma::mat shift_price(n,20);
  int secArray [20] = {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1};
  for( int i=0; i<n; i++ ){
    for( int j=0; j<20; j++ ){
      while(price(i,0) - price(secArray[j],0) > j && secArray[j] < n-1) secArray[j]++;
      shift_price(i,j) = price(secArray[j],1);
    }
  }
  return Rcpp::wrap(shift_price);"
)

load_lag_times = function(prices){
  if(!is.matrix(prices)) stop("Input must be a matrix")
  if(ncol(prices)!=2) stop("Input has wrong number of columns.  Should be time, variable to lag.")
  lag = load_lag_times_cxx(prices)
  for(i in 1:20) lag[prices[,1]<i,i] = NA
  return(lag)
}

#Number of trades, # of Buy Trades, Highest and lowest price cleared
roll = cxxfunction(signature(prices="numeric", orders="numeric", lags="int"), plugin="RcppArmadillo", body="
  arma::mat price = Rcpp::as<arma::mat>(prices);
  arma::mat order = Rcpp::as<arma::mat>(orders);
  int lag = Rcpp::as<int>(lags);
  int n = price.n_rows;
  int m = order.n_rows;
  arma::mat roll_vals(n,20);
  return(Rcpp::wrap(lag));
  "
)

#Sample run of neural network code:
  #d = data.frame( matrix(rnorm(5000),ncol=5) ); colnames(d) = paste0("X",1:5); d$Y = as.matrix(d)%*%runif(5) + rnorm(100)
  #mod = fit.nn( Y ~ X1 + X2 + X3, data=d, hidden=8 )
  #predict.nn( mod, d[,1:3] )

#Use cvModel function:
  #outNn = cvModel( d, cvGroup=c(rep(-1,100), rep(1:10,each=90)), indCol=6, model="fit.nn(Y~X1+X2+X3+X4+X5, hidden=8, report=F)" )
  #outRf = cvModel( d, cvGroup=c(rep(-1,100), rep(1:10,each=90)), indCol=6, model="randomForest(Y~., ntree=10)" )
  #outGlm = cvModel( d, cvGroup=c(rep(-1,100), rep(1:10,each=90)), indCol=6, model="glm(Y~X1+X2+X3+X4+X5)" )
  #outGlmnet = cvModel( d, cvGroup=c(rep(-1,100), rep(1:10,each=90)), indCol=6, model="fit.glmnet(Y~X1+X2+X3+X4+X5)" )
  #outPcr = cvModel( d, cvGroup=c(rep(-1,100), rep(1:10,each=90)), indCol=6, model="pcr(Y ~ ., ncomp=2 )" )
  #outTree = cvModel( d, cvGroup=c(rep(-1,100), rep(1:10,each=90)), indCol=6, model="rpart(Y ~ . )" )
  #outGbm = cvModel( d, cvGroup=c(rep(-1,100), rep(1:10,each=90)), indCol=6, model="gbm(Y ~ ., n.trees=10, distribution='gaussian' )", pred.cols=4 )

#Measure Performance:
  #sum( (outGlm-d$Y)[cvGroup==-1,1]^2 )
  #sum( (outRf-d$Y)[cvGroup==-1,1]^2 )
  #sum( (outTree-d$Y)[cvGroup==-1,1]^2 )
  #sum( (outNn-d$Y)[cvGroup==-1,1]^2 )
  #for( i in 1:ncol(outGbm) ) print(sum( (outGbm[,i]-d$Y)[cvGroup==-1,1]^2 ))
  #for( i in 1:ncol(outGlmnet) ) print(sum( (outGlmnet[,i]-d$Y)[cvGroup==-1,1]^2 ))
  #for( i in 1:ncol(outPcr) ) print(sum( (outPcr[,i]-d$Y)[cvGroup==-1,1]^2 ))

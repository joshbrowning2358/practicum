#library(randomForest)
#library(rpart)
#library(gbm)
#library(AMORE)
#library(glmnet)
#library(pls)
library(car)
library(Rcpp)
library(inline)
library(RcppArmadillo)
library(reshape)
library(plyr)
library(ggplot2)
library(scales)
#library(neuralnet)
library(biglm)
library(bigmemory)
library(sqldf)
library(nnet)
library(mgcv)
library(glmnet)

#preds should be a vector that has a length=nrow(d).  It contains prediction values for the days/times of interest.
#d should be the big.matrix object.
#cnames is the colnames of d
#type should be 1 or 60 depending on the type of model: 1-second or 60-second forecasting
#full: Returns a list of performance by time and performance by price level if T.
eval_preds = function( preds, d, cnames, type, full=F ){
  if( any(is.na(preds)) ){
    warning(paste0("No NAs allowed in predictions!  ",sum(is.na(preds))," NA's replaced with MicroPrice at that time."))
    preds[is.na(preds)] = d[is.na(preds),which(cnames=="MicroPrice")]
  }
  eval.rows = d[,which(cnames=="Diff")]==1
  if(type==1) price_diff = d[,which(cnames=="PriceDiff1SecAhead")]
  if(type==60) price_diff = d[,which(cnames=="PriceDiff60SecAhead")]
  if(type!=1 & type!=60) stop("Invalid type supplied")
  time = d[,which(cnames=="Time")]
  price = d[,which(cnames=="MicroPrice")]
  day = d[,which(cnames=="day")]
  d.eval = data.frame( err=preds-price_diff, time, price, price_diff, day )
  d.eval = d.eval[eval.rows,]
  
  d.agg.tot = sqrt(mean(d.eval$err[d.eval$day %in% c(3,4)]^2, na.rm=T))
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
    
    #return performance aggregated by time and by current price
    return( list( d.agg.tot, d.agg.day, d.agg.t, d.agg.p ) )
  }
  
  return(list(d.agg.tot,d.agg.day))
}

eval_print = function( preds, price_diff=d[,5], price=d[,2], time=d[,1] ){
  if( any(is.na(preds)) ){
    warning(paste0("No NAs allowed in predictions!  ",sum(is.na(preds))," NA's replaced with MicroPrice at that time."))
    preds[is.na(preds)] = price[is.na(preds)]
  }
  eval.rows = c(0,diff( price ))!=0
  eval.rows[is.na(eval.rows)] = FALSE
  d.eval = data.frame( err=preds-price_diff, time, price, price_diff )
  d.eval = d.eval[eval.rows,]
  d.eval$day = ifelse( d.eval$time < 24*60*60, "Monday"
                       ,ifelse( d.eval$time < 48*60*60, "Tuesday"
                                ,ifelse( d.eval$time < 72*60*60, "Wednesday"
                                         ,ifelse( d.eval$time < 96*60*60, "Thursday"
                                                  ,ifelse( d.eval$time < 120*60*60, "Friday", "Error" ) ) ) ) )
  if( any(d.eval$day %in% c("Error")) ) stop("Bad times: Out of range")
  d.eval$day = factor(d.eval$day, levels=c("Monday","Tuesday","Wednesday","Thursday","Friday"))
  out = ddply( d.eval, "day", function(x){
    d = data.frame(MSE.Model=sum(x$err^2)/nrow(x), MSE.Pers=sum(x$price_diff^2)/nrow(x))
    d$Ratio = d[,1]/d[,2]
    return(d)
  } )
  d.eval$day = ifelse(d.eval$day %in% c("Wednesday","Thursday"), "Wed-Thu", "Mon-Tue")
  out = rbind( out, ddply( d.eval, "day", function(x){
    d = data.frame(MSE.Model=sum(x$err^2)/nrow(x), MSE.Pers=sum(x$price_diff^2)/nrow(x))
    d$Ratio = d[,1]/d[,2]
    return(d)
  } ) )
  out[,2:4] = round(out[,2:4],4)
  print(out)
}

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
  model = gsub(" ","",model)
  if(!grepl( "data=", model )) model = paste0(substr(model,1,nchar(model)-1),", data=train )")
  
  #Store the models, if desirable
  mods = list()
  
  #Set up the model formula and rename the columns of d appropriately:
  #Holdout each cv-group in turn:
  for( i in sort(unique(cvGroup[cvGroup>0])) ){
    train = d[!cvGroup %in% c(-1,i),]
    predict = d[cvGroup %in% c(-1,i),-indCol]
    
    #Evaluate the model
    fit = eval( parse( text=model) )
    
    #Predict based on the model used:
    if( grepl("^neuralnet", model) ){
      #neuralnet prediction requires a dataframe with only the used variables in it:
      depCols = gsub( ",.*", "", gsub(".*~", "", model ) )
      depCols = strsplit(depCols, "+", fixed=T)[[1]]
      depCols = sapply(depCols, function(x){gsub(" ","",x)} )
      predict.temp = predict[,colnames(predict) %in% depCols]
      preds = compute(fit, predict.temp)$net.result
    }
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
    if( grepl("(^randomForest|^nnet)", model) )
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

#This function fits cross-validated models using the bigglm package.
#ind_var_names: Names of variables to use in model
#d: big.matrix object containing the data
#cnames: column names for d
#chunk.rows: How many rows should be processed at a time?  25,000 seems like an optimal choice based on a few tests.
cvModel.bigglm = function(ind_var_names, d, cnames, chunk.rows=25000
                          ,dep_var="PriceDiff1SecAhead", filter=rep(T,nrow(d))){
  #Clean up ind_var_names
  if(any(!c("MicroPriceAdj","LogBookImb","LogBookImbInside","MicroPriceAdjExp") %in% ind_var_names)){
    print("Warning: Not including one/some of the base columns!")
  }
  #Reorder ind_var_names (sort based on cnames for easy prediction):
  ind_var_names = ind_var_names[order( match(ind_var_names,cnames) )]
  col.inx = cnames %in% c(dep_var,"cvGroup",ind_var_names)
  preds = rep(0,nrow(d))
  
  for( cvGroupNo in 1:10 ){
    #Define function to read data:
    read.d = function(reset){
      if(reset){
        skip.rows<<-0
        return(NULL)
      }
      row.inx = (skip.rows+1):min(skip.rows+chunk.rows,nrow(d))
      while( sum(filter[row.inx])==0 & skip.rows < nrow(d)-1 ){
        skip.rows <<- skip.rows + chunk.rows
        row.inx = (skip.rows+1):min(skip.rows+chunk.rows,nrow(d))
      }
      if(skip.rows>=nrow(d)-1) return(NULL)
      out = data.frame( d[row.inx,col.inx] )
      colnames(out) = cnames[col.inx]
      skip.rows <<- skip.rows + chunk.rows
      #Stop processing once you hit test data:
      if(any(out$cvGroup==-1)) skip.rows<<-nrow(d)
      return(out[!out$cvGroup %in% c(cvGroupNo,-1) & filter[row.inx],])
    }
    
    #Create the bigmatrix model:
    form = as.formula( paste0( dep_var, "~", paste(ind_var_names,collapse="+") ) )
    fit = bigglm( form, read.d )
    
    #Predict on the holdout group as well as the test group
    preds[d[,6]==cvGroupNo] = cbind(1,d[d[,6]==cvGroupNo, cnames %in% ind_var_names]) %*%t(t(summary(fit)$mat[,1]))
    preds[d[,6]==-1] = cbind(1,d[d[,6]==-1, cnames %in% ind_var_names]) %*%t(t(summary(fit)$mat[,1]))/10 +
      preds[d[,6]==-1]
  }
  preds[is.na(preds)] = 0
  
  return(preds)
}

create.read.f = function(filename,chunk.rows=100){
  f = function(reset=TRUE){
    if(reset){
      skip.rows<<-0
      return(NULL)
    }
    d = read.csv(filename, nrow=chunk.rows, skip=skip.rows)
    skip.rows <<- skip.rows + chunk.rows
    colnames(d) = paste0("X",1:10)
    if( nrow(d)==0 ) return(NULL)
    return(d)
  }
  return(f)
}

#d = matrix(rnorm(10000),nrow=1000)
#d = data.frame(d)
#write.csv(d, file="temp.csv", row.names=F)
#f = create.read.f("temp.csv", chunk.rows=50)
#f(T)
#for( i in 1:21 ) print( nrow(f(F)) )
#fit.big = bigglm( X1 ~ X2 + X3, data=read.d )
#fit = glm(X1 ~ X2 + X3, data=d)
#summary(fit.big)
#summary(fit)

#Don't run the Rcpp code if you're on Windows, unless you're on version 2.15.3
if( Sys.info()[1]!="Windows"){
  #To bring in lagged times, use Rcpp (so you can loop efficiently):
  #prices (the input, must be a matrix) should be two columns: time (numeric, in seconds) and variable to lag.
  #Outputs shift_price, a matrix, which contains lagged values at 1s, 2s, 3s, 4s, 5s.
  #6 indicators:
  #i: Current row of matrix.  Take this microprice and impute it ahead 1s, 2s, 3s, 4s, 5s
  #secj, j=1:5: Index of row corresponding to j seconds ahead of ith row.  Increments up as i increments
  load_lag_price_cxx = cxxfunction(signature(prices="numeric", lags="numeric"), plugin="RcppArmadillo", body="
                                   arma::mat price = Rcpp::as<arma::mat>(prices);
                                   arma::mat lag = Rcpp::as<arma::mat>(lags);
                                   int n = price.n_rows;
                                   int m = lag.n_rows;
                                   arma::mat shift_price(n,m);
                                   int secArray [m];
                                   for( int i=0; i<m; i++ ) secArray[i] = 1;
                                   for( int i=0; i<n; i++ ){
                                   for( int j=0; j<m; j++ ){
                                   while(price(i,0) - price(secArray[j],0) > lag[j] && secArray[j] < n-1) secArray[j]++;
                                   shift_price(i,j) = price(secArray[j],1);
                                   }
                                   }
                                   return Rcpp::wrap(shift_price);"
  )
  
  load_lag_price = function(prices, lags){
    if(is.data.frame(prices)) prices = as.matrix(prices)
    if(!is.matrix(prices)) stop("Input must be a matrix")
    if(ncol(prices)!=2) stop("Input has wrong number of columns.  Should be time, variable to lag.")
    if(is.numeric(lags)) lags = matrix(lags)
    if(!is.matrix(lags)) stop("Lags must be a list or a matrix!")
    if(ncol(lags)!=1) stop("Lags must have only one column!")
    out = load_lag_price_cxx(prices, lags)
    for(i in 1:length(lags)) out[prices[,1]<lags[i],i] = NA
    if( is.null(colnames(prices)[2]) ) colnames(prices)[2] = "Var"
    colnames(out) = paste0("Lag_",lags,"_",colnames(prices)[2])
    return(out)
  }
  
  #Check code works:
  #lags = load_lag_price( price[1:10000,c("Time","MicroPrice")], lags=1:10 )
  #lags = data.frame( price[1:10000,c("Time","MicroPrice")], lags)
  #toPlot = melt(lags, id.var="Time")
  #ggplot(toPlot[toPlot$Time<=120,], aes(x=Time, y=value, color=variable, group=variable) ) + geom_line()
  
  # prices has 1 column: time
  # orders has 4 columns: time, 0=BUY/1=SELL, price, units
  # output has 5 columns: # of trades in last Lag seconds, # of SELL trades, # of units traded, # of SELL units
  # Variable i keeps track of row of price
  # Variable iLag keeps track of Lag seconds back row of orders
  # Variable iCurr keeps track of first row of orders that has a time greater than price(i,0)
  load_lag_trades_cxx = cxxfunction(signature(prices="numeric", orders="numeric", lags="numeric"), plugin="RcppArmadillo", body="
                                    arma::mat price = Rcpp::as<arma::mat>(prices);
                                    arma::mat order = Rcpp::as<arma::mat>(orders);
                                    int n = price.n_rows;
                                    int m = order.n_rows;
                                    double lag = Rcpp::as<double>(lags);
                                    arma::mat output(n,4);
                                    int iLag(0), iCurr(0);
                                    for( int i=0; i<n; i++){
                                    while( price(i,0) > order(iLag,0) + lag && iLag < m-1) iLag++; //iterate iLag until it's within Lag seconds of price's time
                                    while( price(i,0) >= order(iCurr,0)  && iCurr < m-1 ) iCurr++;
                                    output(i,0) = iCurr-iLag;
                                    output(i,1) = 0;
                                    output(i,2) = 0;
                                    output(i,3) = 0;
                                    for( int j=iLag; j<iCurr; j++){
                                    output(i,1) = output(i,1) + order(j,1);
                                    output(i,2) = output(i,2) + order(j,3);
                                    output(i,3) = output(i,3) + order(j,3)*order(j,1);
                                    }
                                    }
                                    return(wrap(output));
                                    "
  )
  
  load_lag_trades = function( price, orders, lag=60 ){
    price_mat = as.matrix( price$Time )
    orders_agg = ddply( orders, c("Time", "RestingSide", "TradePrice"), function(df)sum(df$TradeQuantity) )
    colnames(orders_agg)[4] = "Units"
    orders_agg$RestingSide = as.numeric( orders_agg$RestingSide=="SELL")
    output = load_lag_trades_cxx(prices=matrix(price$Time), orders=as.matrix(orders_agg), lags=lag )
    output = data.frame(output)
    colnames(output) = paste0(c("Trades_Lag_", "SELLs_Lag_", "Units_Lag_", "Units_SELL_Lag_"), lag, "s" )
    return(output)
  }
}

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

#Run multiple rounds of simulations.  If one network is much slower or faster, tune it a bit for comparable speeds.  We want the best prediction with equal times.
sim_neural_net = function(type, n){
  if(type=="half-quadratic"){
    d = data.frame( x = runif(n*2, min=0, max=3) )
    d$y = d$x^2 + rnorm(n*2)
    d$train = rep(c(T,F), each=n)
  }
  if(type=="quadratic"){
    d = data.frame( x = runif(n*2, min=-3, max=3) )
    d$y = d$x^2 + rnorm(n*2)
    d$train = rep(c(T,F), each=n)
  }
  if(type=="sin"){
    d = data.frame( x = runif(n*2, min=-2*pi, max=2*pi) )
    d$y = sin(d$x) + rnorm(n*2)
    d$train = rep(c(T,F), each=n)
  }
  if(type=="linear"){
    d = data.frame( x = runif(n*2, min=-3, max=3) )
    d$y = d$x + rnorm(n*2)
    d$train = rep(c(T,F), each=n)
  }
  
  print("Starting nnet:")
  #nnet
  Start = Sys.time()
  fit = try( nnet( d$x[d$train], d$y[d$train], size=6, linout=T, maxit=100, trace=F ) )
  if( !is(fit)=="try-error" ){
    d$preds = predict( fit, newdata=d[,"x",drop=F] )
    out = data.frame(t = as.numeric(Sys.time() - Start))
    out$MSE.Test = 1/sum(!d$train)*t(as.numeric(!d$train)*(d$preds-d$y))%*%(d$preds-d$y)
    out$MSE.Train = 1/sum(!d$train)*t(as.numeric(d$train)*(d$preds-d$y))%*%(d$preds-d$y)
    out$mod = "nnet"
  } else {
    out = data.frame(t=NA, MSE.Test=NA, MSE.Train=NA, mod="nnet")
  }
  out$mod = as.character( out$mod )
  
  print("Starting neuralnet:")
  #neuralnet
  Start = Sys.time()
  fit = try( neuralnet( y ~ x, data=d[d$train,], hidden=6, threshold=.1 ) )
  if( !is(fit)=="try-error" ){
    preds = try( compute( fit, covariate=d[,"x",drop=F] )$net.result )
    if( !is(preds)=="try-error" ){
      d$preds = preds
      out.new = data.frame(t = as.numeric(Sys.time() - Start))
      out.new$MSE.Test = 1/sum(!d$train)*t(as.numeric(!d$train)*(d$preds-d$y))%*%(d$preds-d$y)
      out.new$MSE.Train = 1/sum(!d$train)*t(as.numeric(d$train)*(d$preds-d$y))%*%(d$preds-d$y)
      out.new$mod = "neuralnet"
      out = rbind(out, out.new)
    } else {
      out = rbind(out,c(NA,NA,NA,"neuralnet"))
    }
  } else {
    out = rbind(out,c(NA,NA,NA,"neuralnet"))
  }
  
  print("Starting mlp:")
  #RSNNS mlp
  Start = Sys.time()
  fit = try( mlp( d$x[d$train], d$y[d$train], size=c(6), linOut=T ) )
  if( !is(fit)=="try-error" ){
    d$preds = predict( fit, newdata=d[,"x",drop=F] )
    out.new = data.frame(t = as.numeric(Sys.time() - Start))
    out.new$MSE.Test = 1/sum(!d$train)*t(as.numeric(!d$train)*(d$preds-d$y))%*%(d$preds-d$y)
    out.new$MSE.Train = 1/sum(!d$train)*t(as.numeric(d$train)*(d$preds-d$y))%*%(d$preds-d$y)
    out.new$mod = "RSNNS: mlp"
    out = rbind(out, out.new)
  } else {
    out = rbind(out,c(NA,NA,NA,"RNNS: mlp"))
  }
  
  print("Starting rbf:")
  #RSNNS rbf
  Start = Sys.time()
  fit = try( rbf( d$x[d$train], d$y[d$train], size=6 ) )
  if( !is(fit)=="try-error" ){
    d$preds = predict( fit, newdata=d[,"x",drop=F] )
    out.new = data.frame(t = as.numeric(Sys.time() - Start))
    out.new$MSE.Test = 1/sum(!d$train)*t(as.numeric(!d$train)*(d$preds-d$y))%*%(d$preds-d$y)
    out.new$MSE.Train = 1/sum(!d$train)*t(as.numeric(d$train)*(d$preds-d$y))%*%(d$preds-d$y)
    out.new$mod = "RSNNS: rbf"
    out = rbind(out, out.new)
  } else {
    out = rbind(out,c(NA,NA,NA,"RNNS: rbf"))
  }
  
  print("Starting elman:")
  #RSNNS elman
  Start = Sys.time()
  fit = try( elman( d$x[d$train], d$y[d$train], size=6 ) )
  if( !is(fit)=="try-error" ){
    d$preds = predict( fit, newdata=d[,"x",drop=F])
    out.new = data.frame(t = as.numeric(Sys.time() - Start))
    out.new$MSE.Test = 1/sum(!d$train)*t(as.numeric(!d$train)*(d$preds-d$y))%*%(d$preds-d$y)
    out.new$MSE.Train = 1/sum(!d$train)*t(as.numeric(d$train)*(d$preds-d$y))%*%(d$preds-d$y)
    out.new$mod = "RSNNS: elman"
    out = rbind(out, out.new)
  } else {
    out = rbind(out,c(NA,NA,NA,"RNNS: elman"))
  }
  
  print("Starting jordan:")
  #RSNNS jordan
  Start = Sys.time()
  fit = try( jordan( d$x[d$train], d$y[d$train], size=6 ) )
  if( !is(fit)=="try-error" ){
    d$preds = predict( fit, newdata=d[,"x",drop=F])
    out.new = data.frame(t = as.numeric(Sys.time() - Start))
    out.new$MSE.Test = 1/sum(!d$train)*t(as.numeric(!d$train)*(d$preds-d$y))%*%(d$preds-d$y)
    out.new$MSE.Train = 1/sum(!d$train)*t(as.numeric(d$train)*(d$preds-d$y))%*%(d$preds-d$y)
    out.new$mod = "RSNNS: jordan"
    out = rbind(out, out.new)
  } else {
    out = rbind(out,c(NA,NA,NA,"RNNS: jordan"))
  }
  
  print("Starting AMORE:")
  #AMORE
  Start = Sys.time()
  net = newff( c(2,6,1), learning.rate.global=1e-2, momentum.global=.5
               ,error.criterium="LMS", Stao=NA, hidden.layer="tansig"
               ,output.layer="purelin", method="ADAPTgdwm" )
  fit = try( AMORE::train( net, as.matrix(cbind(1,d[d$train,"x",drop=F])), as.matrix(d[d$train,"y",drop=F])
                           ,error.criterium="LMS", show.step=10, n.shows=10, report=F ) )
  if( !is(fit)=="try-error" ){
    d$preds = predict.AMORE( fit, newdata=cbind(1,d[,"x",drop=F]) )
    out.new = data.frame(t = as.numeric(Sys.time() - Start))
    out.new$MSE.Test = 1/sum(!d$train)*t(as.numeric(!d$train)*(d$preds-d$y))%*%(d$preds-d$y)
    out.new$MSE.Train = 1/sum(!d$train)*t(as.numeric(d$train)*(d$preds-d$y))%*%(d$preds-d$y)
    out.new$mod = "AMORE"
    out = rbind(out, out.new)
  } else {
    out = rbind(out,c(NA,NA,NA,"AMORE"))
  }
  
  out$type = type
  out$n = n
  return(out)
}

sim_nnet = function(type, n, hidden, input){
  if(type=="half-quadratic"){
    d = data.frame(matrix(runif(n*2*input, min=0, max=3), ncol=input ))
    d$y = apply( d*d, 1, sum ) + rnorm(n*2)
    d$train = rep(c(T,F), each=n)
  }
  if(type=="quadratic"){
    d = data.frame(matrix(runif(n*2*input, min=-3, max=3), ncol=input ))
    d$y = apply( d*d, 1, sum ) + rnorm(n*2)
    d$train = rep(c(T,F), each=n)
  }
  if(type=="linear"){
    d = data.frame(matrix(runif(n*2*input, min=-3, max=3), ncol=input ))
    d$y = apply( d, 1, sum ) + rnorm(n*2)
    d$train = rep(c(T,F), each=n)
  }
  
  #nnet
  Start = Sys.time()
  fit = try( nnet( d[d$train,1:input], d$y[d$train], size=hidden, linout=T, maxit=100, trace=F ) )
  if( !is(fit)=="try-error" ){
    d$preds = predict( fit, newdata=d[,1:input,drop=F] )
    out = data.frame(t = as.numeric(Sys.time() - Start))
    out$MSE.Test = 1/sum(!d$train)*t(as.numeric(!d$train)*(d$preds-d$y))%*%(d$preds-d$y)
    out$MSE.Train = 1/sum(!d$train)*t(as.numeric(d$train)*(d$preds-d$y))%*%(d$preds-d$y)
    out$mod = "nnet"
  } else {
    out = data.frame(t=NA, MSE.Test=NA, MSE.Train=NA, mod="nnet")
  }
  
  out$hidden = hidden
  out$input = input
  out$type = type
  out$n = n
  return(out)
}

#Potential Improvements:
#- Change time delay to be based on time of day instead of decaying back indefinitely
#- Include option to run nnet with multiple starting weights and chose best one
#- Train nnet until it forecasts well on test set.  Probably not possible with nnet...

#NOTE: cnames should be in memory before running this function!!!
#d: big.matrix object
#ind_vars: names of variables to be used in the model
#dep_var: Either "PriceDiff1SecAhead" or "PriceDiff60SecAhead"
#price_decay: How the case weights should decrease as a function of price level?  If the current price is $94, then observations at $95 and $93 will have weight 1*price.decay.
#day.decay: How should the case weights decrease as a function of day?  For k days in the past, case weights are decayed by (day.decay)^k
#time.decay: How should the case weights decrease as a function of time?  For k seconds in the past, case weights are decayed by (time.decay)^k
#outcry.decay: How should the case weights decrease as a function of outcry period?  If the observation is the same as the current outcry, don't decay, otherwise multiply by this factor.
#hour.decay: How should the case weights decrease as a function of time of day?  
#step.size: How frequently should models be built (in seconds)?  Smaller values should be more accurate but take longer to fit.
#chunk.rows: Controls how many rows are read at one time for the bigglm algorithm.
#type: Should be either "GLM", "nnet", or "gam".  Either a linear regression, neural network, or GAM model is then used.
#size: How many hidden nodes to fit with nnet?  Ignored for type!="nnet"
#min.wt: What obs should be ignored?  Ignored if their weight is less than max(Weight)*min.wt
#repl: How many neural networks (with randomized weights) should be fit?  The network with best fit on training data is kept.  Ignored for type!="nnet"
#combine.method: How should the multiple outputs be combined?  If "glm" or "nnet" (with quotes!) then a linear regression or neural network is built on the output.  If a function, that function is applied to each row of the predictions.
weighted_model = function(d, ind_vars, dep_var="PriceDiff1SecAhead"
                          ,price.decay=1, day.decay=1, time.decay=1, outcry.decay=0.5, hour.decay=0.8, step.size=2.25*60*60
                          ,chunk.rows=25000, type="GLM", size=10, min.wt=0, repl=5, combine.method=mean
                          ,plot.fl=FALSE){
  #Check for results.csv, if it doesn't exist then create it:
  if( !any( list.files()=="results.csv") ){
    pred.1 = eval_preds(0,d,cnames,type=1)
    pred.60 = eval_preds(0,d,cnames,type=60)
    out = data.frame(id=1,forecast=1,type="pers",ind_vars=NA,step.size=NA,size=NA,repl=NA,params=NA,t=NA,RMSE=pred.1[[1]], RMSE.W=pred.1[[2]][3,2], RMSE.R=pred.1[[2]][4,2], RMSE.F=pred.1[[2]][5,2])
    out = rbind(out,data.frame(id=1,forecast=60,type="pers",ind_vars=NA,step.size=NA,size=NA,repl=NA,params=NA,t=NA,RMSE=pred.60[[1]], RMSE.W=pred.60[[2]][3,2], RMSE.R=pred.60[[2]][4,2], RMSE.F=pred.60[[2]][5,2]))
    write.csv(file="results.csv", out)
  }

  if(is.data.frame(d)) cnames = colnames(d)
  
  #Maintain compatability
  #If ind_vars is a vector, make it a list of length 1:
  if(is.character(ind_vars)){ ind_vars = list(ind_vars) }
  #If type is a vector, make it a list  
  if(length(type)==1){ type=as.list(rep(type,length(ind_vars)))}
  #No combine.method required if only running one model:
  if(length(ind_vars)==1) combine.method=""
  
  #Set up Start and results for output purposes:
  Start = Sys.time()
  results = read.csv(file="results.csv", stringsAsFactors=F)
  ID = max(results$id)+1
  
  #Put in a safety net to help keep R from crashing.
  if( !all(do.call("c",ind_vars) %in% cnames) ){
    stop("Not all variables in ind_vars are in cnames")
  }
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
  
  #NOT CURRENTLY USED!  GLM models are fit using glm and not bigglm.
  #Define function to read data. This is needed for the bigglm model. This function should return NULL when passed TRUE (and
  #reset the point we're reading from) and should return the next chunk of data when passed FALSE.
  read.d = function(reset){
    if(reset){
      skip.rows<<-0 #Use <<- to assign to global environment
      return(NULL)
    }
    row.inx = (skip.rows+1):min(skip.rows+chunk.rows,nrow(d))
    #filter controls which rows are useable for the current model:
    while( sum(filter[row.inx])==0 & skip.rows < nrow(d)-1 ){
      skip.rows <<- skip.rows + chunk.rows
      row.inx = (skip.rows+1):min(skip.rows+chunk.rows,nrow(d))
    }
    if(skip.rows>=nrow(d)-1) return(NULL)
    out = data.frame( d[row.inx,col.inx] )
    #Filter out NA rows:
    out = out[apply(out,1,function(x){sum(is.na(x))})==0,]
    colnames(out) = cnames[col.inx]
    skip.rows <<- skip.rows + chunk.rows
    #Stop processing once filter becomes F (i.e. reached end of training set)
    if(any(!filter[row.inx])) skip.rows<<-nrow(d)
    return(out[filter[row.inx],])
  }
  
  #Create the model formula, initialize the time.loop:
  form = lapply(ind_vars,function(x){
    as.formula( paste0( dep_var, "~", paste(x,collapse="+") ) )
  } )
  time.loop = 24*60*60*2 + seq(0,24*60*60*3,by=step.size)
  #Remove times after end of dataset:
  time.loop = time.loop[time.loop<max(d[,which(cnames=="Time")])]
  
  #Build the model for each time chunk:
  for( i in time.loop ){
    
    filter = d[,which(cnames=="Time")]<=(i-60) #Only use rows which are more than 60 seconds before the first prediction
    pred.filter = d[,which(cnames=="Time")]>i & d[,which(cnames=="Time")]<=i+step.size #Define which rows you'll be predicting
    #If MicroPrice is NA on some row, don't predict for that row (this code generates a warning without this)
    pred.filter = pred.filter & !is.na(d[,which(cnames=="MicroPrice")])
    #Update the case weights:
    d[filter,which(cnames=="Weight")] =
      #Use the last observed MicroPriceAdj and compare that to all MicroPrices (larger diff=>smaller weight)
      price.decay^(abs(d[filter,which(cnames=="MicroPriceAdj")][sum(filter)]-d[filter,which(cnames=="MicroPriceAdj")]))*
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
    
    #Fit a GLM if that's what's desired:
    #if(type=="GLM") fit = bigglm( form, read.d, weights=Weight~1 )
    
    #Create each of the different models
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
        bestfit = nnet( form[[iVar]], weights=Weight, data=data, linout=TRUE, maxit=100000, size=size, trace=F )
        j = 1
        #Run nnet with several different initial weights if repl>1:
        while(j < repl){
          fit = nnet( form[[iVar]], weights=Weight, data=data, linout=TRUE, maxit=100000, size=size, trace=F )
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
        fit = cv.glmnet( x=as.matrix(data[,ind_vars[[iVar]]]), y=as.matrix(data[,dep_var[[iVar]]]), weights=data[,"Weight"] )
      }
      
      #Make predictions:
      pred.d = data.frame(d[filter | pred.filter,col.inx[[iVar]]]) #covariates used for predictions
      colnames(pred.d) = cnames[col.inx[[iVar]]] #Give the covariates the correct colnames
      if(type[[iVar]] %in% c("GLM", "nnet", "gam")){
        preds[filter | pred.filter,iVar] = predict(fit, newdata=pred.d) #Update preds (in the correct rows) with the new predictions
      } else {
        #By default, this uses the optimal cv value for lambda
        preds[filter | pred.filter,iVar] = predict(fit, newx=as.matrix(pred.d[,ind_vars[[iVar]]]))
      }
    } #End Model Loop
        
    #Combine predictions from various models to make final prediction:
    #if only one prediction, use that:
    if(ncol(preds)==2){
      preds[pred.filter,2] = preds[pred.filter,1]
      next
    }
    
    if(is.function(combine.method)){
      preds[pred.filter,ncol(preds)] = apply(preds[pred.filter,-ncol(preds), drop=F], 1, combine.method)
      print(paste0("Time ",i," completed out of total time ",max(time.loop)))
      next
    }

    #Combine the output using a glm
    if(combine.method=="glm"){
      combine.fit = glm( Y ~ ., data=data.frame(preds[filter,-ncol(preds)], Y=d[filter,which(cnames==dep_var)]))
#      combine.fit = glm( Y ~ ., data=data.frame(preds[filter | pred.filter,-ncol(preds)], Y=d[filter | pred.filter,which(cnames==dep_var)]))
      preds[pred.filter,ncol(preds)] = predict(combine.fit, newdata=data.frame(preds[pred.filter,]))
    }

    #Use a neural network to choose which model to use
    if(combine.method=="nnet"){
      combine.fit = nnet( x=preds[filter,-ncol(preds)], y=d[filter,which(cnames==dep_var)], size=10, linout=T, maxit=10000, trace=F)
#      combine.fit = nnet( x=preds[filter | pred.filter,-ncol(preds)], y=d[filter | pred.filter,which(cnames==dep_var)])
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
  
  t = as.numeric(difftime(Sys.time(), Start, units="mins"))
  if( dep_var %in% c("PriceDiff1SecAhead", "PriceDiff60SecAhead") ){
    if(dep_var=="PriceDiff1SecAhead") perf=eval_preds(preds[,ncol(preds)], d, cnames, 1, full=T)
    if(dep_var=="PriceDiff60SecAhead") perf=eval_preds(preds[,ncol(preds)], d, cnames, 60, full=T)
  } else {
    return(preds[,ncol(preds)])
  }
  results = rbind(results, c(id=ID, ifelse(dep_var=="PriceDiff1SecAhead",1,60), type=do.call("paste",type)
     ,ind_vars = paste(ind_vars,collapse=","), step.size=step.size, size=size, repl=repl
     ,params=paste0("price.decay=",price.decay,",day.decay=",day.decay,",time.decay=",time.decay,",outcry.decay=",outcry.decay,"hour.decay=",hour.decay,"repl=",repl,",min.wt=",min.wt,"combine.method=",combine.method)
     ,t=t, RMSE=perf[[1]], RMSE.W=perf[[2]][3,2], RMSE.R=perf[[2]][4,2], RMSE.F=perf[[2]][5,2] ) )
  write.csv(results, "results.csv", row.names=F)
  write.csv(results, file=paste0("results_",ID,".csv"), row.names=FALSE )
  if(plot.fl){
    ggsave( paste0("ID=",ID,"_time.png"), ggplot(perf[[3]], aes(x=time, y=Model.RMSE/Base.RMSE) ) + geom_point() )
    ggsave( paste0("ID=",ID,"_price.png"), ggplot(perf[[4]], aes(x=price, y=Model.RMSE/Base.RMSE) ) + geom_point() )
  }

  return(preds)
}

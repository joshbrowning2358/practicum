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
library(bigmatrix)
library(sqldf)

eval_preds = function( preds, price_diff, price, time ){
  if( any(is.na(preds)) ) stop("No NAs allowed in predictions!  Replace with MicroPrice at that time.")
  eval.rows = c(0,diff( price ))!=0
  eval.rows[is.na(eval.rows)] = FALSE
  day = ifelse( time < 24*60*60, "Monday"
       ,ifelse( time < 48*60*60, "Tuesday"
       ,ifelse( time < 72*60*60, "Wednesday"
       ,ifelse( time < 96*60*60, "Thursday"
       ,ifelse( time < 120*60*60, "Friday", "Error" ) ) ) ) )
  if( any(day %in% c("Error","Wednesday")) ) stop("Bad times: Wednesday or out of range")
  df = data.frame( err=preds-price_diff, day, time )
  df = df[eval.rows,]
  df$day = factor(df$day, levels=c("Monday","Tuesday","Thursday","Friday"))
  ddply( df, "day", function(x){
    print(paste("Sum of Squares for day",x$day[1],"is",round(sum(x$err^2)/nrow(x),4)))
  } )
  SS = sum( (preds-price_diff)[]^2 )
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
cvModel.bigglm = function(ind_var_names=c("MicroPrice","MicroPriceAdj","LogBookImb","LogBookImbInside","MicroPriceAdjExp")
  ,d, cnames,chunk.rows=25000){
  #Clean up ind_var_names
  if(any(!c("MicroPriceAdj","LogBookImb","LogBookImbInside","MicroPriceAdjExp") %in% ind_var_names)){
    print("Warning: Not including one/some of the base columns!")
  }
  #Reorder ind_var_names (sort based on cnames for easy prediction):
  ind_var_names = ind_var_names[order( match(ind_var_names,cnames) )]
  col.inx = cnames %in% c("PriceDiff1SecAhead","cvGroup",ind_var_names)
  preds = rep(0,nrow(d))
  
  for( cvGroupNo in 1:10 ){
    #Define function to read data:
    read.d = function(reset){
      if(reset){
        skip.rows<<-0
        return(NULL)
      }
      if(skip.rows>=nrow(d)-1) return(NULL)
      row.inx = (skip.rows+1):min(skip.rows+chunk.rows,nrow(d))
      out = data.frame( d[row.inx,col.inx] )
      colnames(out) = cnames[col.inx]
      skip.rows <<- skip.rows + chunk.rows
      #Stop processing once you hit test data:
      if(any(out$cvGroup==-1)) skip.rows<<-nrow(d)
      return(out[!out$cvGroup %in% c(cvGroupNo,-1),])
    }
    
    #Create the bigmatrix model:
    form = as.formula( paste0( "PriceDiff1SecAhead ~", paste(ind_var_names,collapse="+") ) )
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

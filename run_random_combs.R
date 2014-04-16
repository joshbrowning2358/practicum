source_github("https://raw2.github.com/rockclimber112358/practicum/master/functions.R")
if(Sys.info()[1] == "Linux" & Sys.info()[4] == "jb" ) setwd("/media/storage/Professional Files/Mines/MATH 598- Statistics Practicum/Data/")
if(Sys.info()[1] == "Linux" & Sys.info()[4] == "jb" ) d = attach.big.matrix("model_matrix.desc")
if(Sys.info()[1] == "Linux" & Sys.info()[4] == "jb" ) cnames = as.character(read.csv("cnames.csv")[,1])

while(TRUE){
  ind_vars = c()
  #Set a probability for which variables will enter into the model:
  choose_prob = runif(1)
  potential_vars = cnames[!cnames %in% c("MicroPrice1SecAhead", "MicroPrice60SecAhead", "PriceDiff1SecAhead", "PriceDiff60SecAhead","Weight")]
  lag_potential_vars = unique(gsub("Lag_[0-9]{1,3}_","",potential_vars[grepl("Lag",potential_vars)]))
  potential_vars = potential_vars[!grepl("Lag",potential_vars)]
  potential_vars = potential_vars[!potential_vars %in% c("Time", "day", "Diff")]
  #Add potential_vars at random:
  for( i in potential_vars)
    if(runif(1)<choose_prob) ind_vars = c(ind_vars, i)
  for(i in lag_potential_vars){
    if(i %in% ind_vars & runif(1)<choose_prob) ind_vars = c(ind_vars,paste0("Lag_",1:round(rchisq(1,df=10)),"_",i))
    if(i %in% ind_vars & runif(1)<choose_prob) ind_vars = c(ind_vars,paste0("Lag_",1:round(rchisq(1,df=10))*60,"_",i))
  }
  step.size = sample(c(rep(24*5,10),rep(24,5),rep(2.25,2),.75),size=1)*60*60
  outcry.decay = runif(1)
  time.decay = runif(1)
  price.decay = runif(1,.5,1)
  hour.decay = runif(1)
  dep_var = sample(c("PriceDiff1SecAhead","PriceDiff60SecAhead"),size=1)
  #type = sample(c("GLM","gam","nnet"))
  type = "GLM"
  
  #Just in case something got in that shouldn't have:
  ind_vars = ind_vars[ind_vars %in% cnames]
  
  if(!is.null(ind_vars))
    #Use try as model may fail (perfect collinearity in columns)
    try( weighted_model(d, ind_vars=ind_vars, step.size=step.size, type=type, dep_var=dep_var, outcry.decay=outcry.decay, time.decay=time.decay, price.decay=price.decay, hour.decay=hour.decay, repl=10, size=length(ind_vars)*2) )
}

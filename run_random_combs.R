source_github("https://raw2.github.com/rockclimber112358/practicum/master/functions.R")
d = attach.big.matrix("mwd_matrix.desc")
cnames = as.character(read.csv("cnames_mwd.csv")[,1])

while(TRUE){
  ind_vars = c()
  #Set a probability for which variables will enter into the model:
  choose_prob = runif(1)
  potential_vars = cnames[!cnames %in% c("MicroPrice1SecAhead", "MicroPrice60SecAhead", "PriceDiff1SecAhead", "PriceDiff60SecAhead","Weight")]
  lag_potential_vars = unique(gsub("Lag_[1-5]_","",potential_vars[grepl("Lag",potential_vars)]))
  potential_vars = potential_vars[!grepl("Lag",potential_vars)]
  #Add potential_vars at random:
  for( i in potential_vars)
    if(runif(1)<choose_prob) ind_vars = c(ind_vars, i)
  for(i in lag_potential_vars)
    if(i %in% ind_vars & runif(1)<choose_prob) ind_vars = c(ind_vars,paste0("Lag_",1:round(runif(1,0.5,5.5)),"_",i))
  step.size = sample(c(rep(24*5,10),rep(24,5),rep(2.25,2),.75,.25),size=1)*60*60
  outcry.decay = runif(1)
  time.decay = 1-10^sample(c(-3,-4,-5,-100),size=1)
  price.decay = runif(1,.5,1)
  dep_var = sample(c("PriceDiff1SecAhead","PriceDiff60SecAhead"),size=1)
  type = sample(c("GLM","gam","nnet"))
  if(!is.null(ind_vars))
    #Use try as model may fail (perfect collinearity in columns)
    try( weighted_model(d, ind_vars=ind_vars, step.size=step.size, type=type, dep_var=dep_var, outcry.decay=outcry.decay, time.decay=time.decay, price.decay=price.decay, repl=10, size=length(ind_vars)*2) )
}

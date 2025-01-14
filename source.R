## function to generate arrival times.

ArrivalTimeFun = function(T0,lambda){
  i = 1
  AT = c() #vector of arrival times
  while(sum(AT)<=T0){
    AT[i] = rexp(1,rate=lambda)
    i = i+1
  }
  AT  = cumsum(AT)[-(i-1)]
  return(AT)
}

## Function to find best fit marginal distribution from "exponential","gamma","chi-squared","log-normal","weibull".
find.marginal = function(x,criteria){
  library(MASS)
  n=length(x)
  suppressWarnings({
    if(n>1 & class(x)=="numeric"){
      dist = c("exponential","chi-squared","log-normal","weibull")
      aic_vec = c()
      bic_vec = c()
      for(i in 1:length(dist)){
        if(dist[i]=="chi-squared"){
          strlist = list(df=mean(x))
          k = length(strlist)
          fit = fitdistr(x,densfun = dist[i],start = strlist)
          aic_vec[i] = 2*(k-fit$loglik)
          bic_vec[i] = k*log(n)-2*fit$loglik
        }else{
          fit = fitdistr(x,densfun = dist[i])
          k = length(fit$estimate)
          aic_vec[i] = 2*(k-fit$loglik)
          bic_vec[i] = k*log(n)-2*fit$loglik
        }
      }
      if(class(criteria)=="character"){
        if(criteria=="AIC"){
          if(dist[which.min(aic_vec)]=="chi-squared"){
            strlist = list(df=mean(x))  
            fit = fitdistr(x,densfun = dist[which.min(aic_vec)],start = strlist)
            return(list(dist[which.min(aic_vec)],fit$estimate))
          }else{
            fit = fitdistr(x,densfun = dist[which.min(aic_vec)])
            return(list(dist[which.min(aic_vec)],fit$estimate))
          }
        }else{
          if(dist[which.min(bic_vec)]=="chi-squared"){
            strlist = list(df=mean(x))  
            fit = fitdistr(x,densfun = dist[which.min(bic_vec)],start = strlist)
            return(list(dist[which.min(bic_vec)],fit$estimate))
          }else{
            fit = fitdistr(x,densfun = dist[which.min(bic_vec)])
            return(list(dist[which.min(bic_vec)],fit$estimate))
          }
        }
      }else(paste("Expected criteria is not character object."))  
    }else(paste("vector length must > 1 OR class is not numeric."))
  })
} ##Removed gamma
## Function to calculate the number of transaction after which ATM needs to refill.

timeFun = function(x,limit){
  if(class(x)=="integer"){
    num_tran = c(0,0)
    a=1
    sum = count=0
    tresh = limit #capacity of ATM
    for(i in 1:length(x)){
      sum  = sum+x[i]
      count = count+1
      if(sum>=tresh){
        num_tran = rbind(num_tran,cbind(sum,count))
        a = a+1
        sum=count=0
      }
    }
  }else{paste("Expected a numeric vector.")}
  return(num_tran[-1,])
}

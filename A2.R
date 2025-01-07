## function to generate arrival times.

ArTFun = function(T0,lambda){
  i = 1
  AT = c()
  while(sum(AT)<=T0){
    AT[i] = rexp(1,rate=lambda)
    i = i+1
  }
  AT  = cumsum(AT)[-(i-1)]
  return(AT)
}

nD = 30 #for three days
T1 = 12*60 #trabsaction on minute level. 
lambda1 = 20/60 #arrival rate at day
lambda2 = 7/60 #arrival rate at night

MeanVec = c(rep(log(2000),10),rep(log(1500),10),rep(log(700),10))
SDVec = c(rep(0.8,10),rep(0.7,10),rep(0.6,10))

TransMat = c(0,0,0)

for(i in 1:nD){
  T2 = ArTFun(T1,lambda = lambda1)
  nT = length(T2) #total transaction in a day
  AmtVec = rlnorm(nT,MeanVec[i],SDVec[i])
  TransMat = rbind(TransMat,cbind(i,9+round(T2)/60,AmtVec)) #transaction between 9AMto9PM
  if(i<nD){
    T2 = ArTFun(T1,lambda = lambda2)
    nT = length(T2)
    AmtVec = rlnorm(nT,MeanVec[i],SDVec[i])
    T2Mod=21+round(T2)/60 #for 9PMto9AM
    w = which(T2Mod>=24)
    if(length(w)>0) T2Mod[w] = T2Mod[w]-24 #Make sure that every thing is in 24 hrs
    TransMat = rbind(TransMat,cbind(i,T2Mod,AmtVec))
  }
}
TransMat = TransMat[-1,]
View(TransMat)

colnames(TransMat) = c("Day","Time","Amount_withdrawal")
View(TransMat) #Transaction level data
tail(TransMat) #Not exceeded 21 

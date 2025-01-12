## Function to calculate the number of transaction after which ATM needs to refill.

timeFun = function(x){
  if(class(x)=="integer"){
    num_tran = c(0,0)
    a=1
    sum = count=0
    tresh = 100000 #capacity of ATM
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

data = read.csv("ATM_transactions_simulated.csv")

amt = data$Amount_withdrawal

stamps = timeFun(amt)

View(stamps)

summary(stamps)
head(stamps)
tail(stamps)

t = stamps[,2]
sum(data$Amount_withdrawal[1:t[1]])
sum(data$Amount_withdrawal[38:71])

t = which(data$Day==1)

newdata = as.data.frame(data)
## To find is time in minutes to refill the ATM.

a = seq(from=24,to=720,by=24)
for(i in 2:30){
  w = which(data$Day == i)
  newdata$Time[w] = newdata$Time[w]+a[i-1]
}
View(newdata)

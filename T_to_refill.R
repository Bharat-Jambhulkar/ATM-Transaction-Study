## Function to calculate the number of transaction after which ATM needs to refill.

timeFun = function(x){
  if(class(x)=="integer"){
    num_tran = c(0,0)
    a=1
    sum = count=0
    tresh = 300000 #capacity of ATM
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
newdata = newdata[order(newdata$Time), ]

## write.csv(newdata,file = "newdata.csv") only for testing.
stamps = timeFun(newdata$Amount_withdrawal)

View(stamps)

num_hours = c()
nst = cumsum(stamps[,2])
for(i in 1:length(nst)){
  w1 = nst[i];w2 = nst[i+1]
  num_hours[i] = newdata$Time[w2]-newdata$Time[w1]
}
num_hours = num_hours[-length(num_hours)]

length(num_hours)
length(nst)
nst

## For first 10 days
newdata$Day[3093]
w = which(nst==3093)
w

srthrs = sort(num_hours[1:28])
quantile(srthrs,probs = 0.05)
## fill every 5 hrs.

## for 11-20 days
srthrs=sort(num_hours[28:48])
quantile(srthrs,probs = 0.05)

## fill every 6:30 hrs.


srthrs=sort(num_hours[49:57])
plot(density(srthrs))
quantile(srthrs,probs = 0.05)
## For last 10 days of the months fill every 22.46 hrs. 24hrs can also be work. 

## Now we will assume that we don't know the simulation steps. We will perform
## EDA to understand and data and will make guesses
## Also we will deal with problem of how often we should refill the machine
## with cash 

data  = read.csv("ATM_transactions_simulated.csv")
dim(data) #total transactions 9610 

sum(is.na(data)) #no null values

summary(data$Day) #30 days ATM data

head(data) #first 6
tail(data) #last 6

str(data) #describe data

sum(duplicated(data)) #no duplicate rows

#How much transactions took place everyday
num_trs = c()
for(i in 1:30){
  num_trs[i] = nrow(data[which(data$Day==i),])
} 

names = seq(1:30)

barplot(height=num_trs,names.arg = names,xlab = "Day",ylab="Number of Transactions",main = "Number of Transactions Per Day",col=rgb(0.8,0.1,0.1,0.6),ylim=c(0,400))

w = which.max(num_trs)
names[w] #Day 19 heights number of transactions 
num_trs[w] #369 highest number 

m = which.min(num_trs)

names[m] #Day 30 minimum number of transactions
num_trs[m] #251 transactions 

#There is not a lot of variation in the number of transactions each day.

#Analyze amount of transaction 

summary(data$Amount_withdrawal)
#amount is a continuos variable 
plot(density(data$Amount_withdrawal),lwd=2,col=4,main="Amount Withdrawal",xlab = "Amount")

"clearly a positively skewed distribution. Also amount 
is a positive number. Candidate distributions are: Exponentia,
Gamma, Chisquare, lognormal"
c2=rgb(0.8,0.1,0.1,0.6)
boxplot(log(data$Amount_withdrawal)~data$Day,xlab = "Day",ylab="log(Amount)",main="Each Day log(Amount) Boxplot",col=rep(c("#69b3a2",c2,7),each=10))
legend("topright",legend = c("Day 1-10","Day 11-20","Day 21-30"),fill=c("#69b3a2",c2,7),cex=0.8)

"
Based on the median nlack line we can oberve a change point in the median value
particulary in day 21-30 

log transformation is used to reduce the scale and to the see the difference
"

# To check whether there is a change in the median through another way:


f1 = which(data$Day<=10)
f2 = which(data$Day>=11 & data$Day<=20) 
f3 = which(data$Day>=21 & data$Day<=30)

plot(density(data$Amount_withdrawal[f1]),main = "Density Plot",xlab="Amount",lwd=2,col="#1E90FF",ylim=c(0,0.0011))
lines(density(data$Amount_withdrawal[f2]),lwd=2,col="#D2691E")
lines(density(data$Amount_withdrawal[f3]),lwd=2,col="#7B68EE")
legend("topright",legend = c("Day 1-10","Day 11-20","Day 21-30"),fill = c("#1E90FF","#D2691E","#7B68EE"))

"
clearly there is shift in mean of amount.
From EDA we are abel to differ between distribution of amount over 30 days. 

"

t1 = which(data$Time>8 & data$Time<=12) #morning
t2 = which(data$Time>12 & data$Time<=16) #Afternoon
t3 = which(data$Time>16 & data$Time<=20) #Evening
t4 = which(data$Time>20 | data$Time<=8) #Night 

length(t1)
length(t2)
length(t3)
length(t4)

t = which(data$Time>=9 & data$Time<=21)
length(t)

"
if we divide data into 4 sections then we can say customer arrive at 
same rate but during night how this much of customers are coming?
"

## To estimate find the distribution of transaction amount 

"
Ealier we note that first 10 days have diff dist, 11-20 days have diff and 
last 10 days have diff dist.
"

"
Since amount of transaction can be treated as continuos variable we can
use the following function
"

d1 = which(data$Day<=10)
d11 = which(data$Day>=11 & data$Day<=20)
d21 = which(data$Day>=21)

dist1 = getmarginal(data$Amount_withdrawal[d1])
dist11 = getmarginal(data$Amount_withdrawal[d11])
dist21 = getmarginal(data$Amount_withdrawal[d21])


"
Currently the function is using fitdistplus. Target is to use fitdistr only.
"

## Function to find best fit marginal distribution from "exponential","gamma","chi-squared","log-normal","weibull".
find.marginal = function(x,criteria){
  n=length(x)
  suppressWarnings({
  if(n>1 & class(x)=="numeric"){
    dist = c("exponential","gamma","chi-squared","log-normal","weibull")
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
}

y = rexp(1000,4)


find.marginal(y,"BIC")

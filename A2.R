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

num_days = 30 #Number of days
T1 = 12*60 #on minute level. 
lambda1 = 20/60 #arrival rate at day per minute 
lambda2 = 7/60 #arrival rate at night per minute

mean_vec = c(rep(log(2000),10),rep(log(1500),10),rep(log(700),10)) #Mean for lognormal
sd_vec = c(rep(0.8,10),rep(0.7,10),rep(0.6,10)) #SD for lognormal 

transaction_matrix = c(0,0,0)

for(i in 1:num_days){
  T2 = ArrivalTimeFun(T1,lambda = lambda1)
  nT = length(T2) #total transaction in a day
  amount_vec = rlnorm(nT,mean_vec[i],sd_vec[i])
  transaction_matrix = rbind(transaction_matrix,cbind(i,9+round(T2)/60,amount_vec)) #transaction between 9AMto9PM
  if(i<num_days){
    T2 = ArrivalTimeFun(T1,lambda = lambda2)
    nT = length(T2)
    amount_vec = rlnorm(nT,mean_vec[i],sd_vec[i])
    T2_modified=21+round(T2)/60 #for 9PMto9AM
    w = which(T2_modified>=24)
    if(length(w)>0) T2_modified[w] = T2_modified[w]-24 #Make sure that every thing is in 24 hrs
    transaction_matrix = rbind(transaction_matrix,cbind(i,T2_modified,amount_vec))
  }
}
transaction_matrix = transaction_matrix[-1,]
View(transaction_matrix)

colnames(transaction_matrix) = c("Day","Time","Amount_withdrawal")
View(transaction_matrix) #Transactions level data
tail(transaction_matrix) #Not exceeded 21 


## Now to make sure that transaction amount is in the multiples of 100

extra_amount = transaction_matrix[,3]%%100
modified_transaction_amount = transaction_matrix[,3] - extra_amount
View(cbind(transaction_matrix,modified_transaction_amount)) #not proper rounding

w = which(extra_amount>=50)
modified_transaction_amount[w] = modified_transaction_amount[w]+100  
View(cbind(transaction_matrix,modified_transaction_amount))

transaction_matrix[,3] = modified_transaction_amount
transaction_matrix[,2] = round(transaction_matrix[,2],2)

write.csv(transaction_matrix,file = "ATM_transactions_simulated.csv")

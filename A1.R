data <- read.csv("D:/Bharat/College/SPPU Material/SPPU Syllabus/CISM/ATM Transaction Study/atm_transactions.csv")

library(dplyr)

data <- data$totalBalance 
  

filtered_data <- data %>% select(atmId, totalBalance, numberIncomeTransaction, numberOutcomeTransaction, 
                                totalIncome, totalOutcome, totalNumberTransaction, day, transactionTime)

filtered_data_OM <- filtered_data %>% filter(atmId == 'atm350000')

View(filtered_data_OM)

##################
### Generate Data

# Generate a sequence of daily date and time for 30 days

start_time <- as.POSIXct("2025-01-01 00:00:00")
end_time <- as.POSIXct("2025-01-31 23:00:00")
timestamp_sequence <- seq.POSIXt(start_time, end_time, by = "hour")
print(timestamp_sequence)

length(timestamp_sequence)

time = (format(as.POSIXct(timestamp_sequence), format = "%H:%M:%S")) 

head(time)

date = as.Date(timestamp_sequence)
length(unique(date))
head(date)

date[1:6] = as.Date("2025-01-01")
head(date)

data <- data.frame(
  Date = date, 
  Time = time
)
View(data)

NineAM_to_9PM = time[11:22]

NinePM_to_9AM = time[23:10]

num_of_people = matrix(nrow=nrow(data),ncol=1)
first_10_days = seq.Date(from = as.Date("2025-01-01"),to=as.Date("2025-01-10"),by="day")

first_11_20_days = seq.Date(from = as.Date("2025-01-11"),to=as.Date("2025-01-20"),by="day")

last_10_days = seq.Date(from = as.Date("2025-01-21"),to=as.Date("2025-01-31"),by="day")

transaction = c()

set.seed(1)
for(i in 1:length(data$Date)){
  #print(unique(data$Date)[i])
  if(data$Date[i] %in% first_10_days){
    transaction[i] = rexp(1,1/2000)
    if(data$Time[i] %in% NineAM_to_9PM){
      num_of_people[i, ] = rpois(1,lambda = 7)
    }else{
      num_of_people[i, ] = rpois(1,lambda = 2)
    }
  }
  if(data$Date[i] %in% first_11_20_days){
    transaction[i] = rexp(1,1/1500)
    if(data$Time[i] %in% NineAM_to_9PM){
      num_of_people[i, ] = rpois(1,lambda = 5)
    }else{
      num_of_people[i, ] = rpois(1,lambda = 1)
    }
  }
  if(data$Date[i] %in% last_10_days){
    transaction[i] = rexp(1,1/1000)
    if(data$Time[i] %in% NineAM_to_9PM){
      num_of_people[i, ] = rpois(1,lambda = 6)
    }else{
      num_of_people[i, ] = rpois(1,lambda = 1.5)
    }
  }
}

num_of_customer = c(num_of_people)

data$num_of_customer = num_of_customer
View(data)

data$transaction = round(transaction)
View(data)


write.csv(data,file = "atm_transactions_simulated.csv")

nrow(data[data$num_of_customer == 0,]) ## This is the problem where num_of_customer is equal zero we should have 0 transaction.


##problem solved in excel file.

d = read.csv("atm_transactions_simulated.csv")
View(d)



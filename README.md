# ATM-Transaction-Study
The generated data is aggregated to represent ATM customer withdrawal arrivals per hour. An attempt is being made to generate transaction-level data, for which a Poisson process would be useful. 

Update: The A2.R file contains code to generate transactions using a Poisson process for customer arrivals, with transaction amounts generated from a lognormal distribution. 

The previously written code generated the same time point for multiple transactions due to the time rounding. This issue has been resolved by modifying the code in A2.R to prevent duplicates in the simulated data. 

Currently working on defining a function to determine the best marginal fit along with its parameters. Attempting to achieve this using only the MASS library.

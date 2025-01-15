# ATM-Transaction-Study
The generated data is aggregated to represent ATM customer withdrawal arrivals per hour. An attempt is being made to generate transaction-level data, for which a Poisson process would be useful. 

Update: The A2.R file contains code to generate transactions using a Poisson process for customer arrivals, with transaction amounts generated from a lognormal distribution. 

The previously written code generated the same time point for multiple transactions due to the time rounding. This issue has been resolved by modifying the code in A2.R to prevent duplicates in the simulated data. 

Currently working on defining a function to determine the best marginal fit along with its parameters. Attempting to achieve this using only the MASS library.

Update: The "find.marginal" function was completed to get the best fit marginal from the constrained space.

Added T_to_refill.R to get the number of transactions till the next refill is needed. Next, find the hours between the refills. 

Updated code to find the hours required to refill the machine.

An update was added to identify peak and non-peak transaction hours.  

Next, I have two targets: 1) Create a good report of this study. 2) Write clean code where all functions will be in one place and then the analysis part. 

Report link: https://www.overleaf.com/project/6784c099ff49f92479b7fd36

# This is the utility function
get_rate_distribution <- function(n_events, nobs, sequence, print = F) {
  results = rep(NA, length(sequence))
  names(results) = sequence                                               
  
  stop = 0 # Stops iterating when probability falls below this number (must be very small, e.g. 0.0000001). Set to 0 to iterate all
  
  for (i in 1:length(sequence)) {
    
    if (print) {
      if ((i %% 1000) == 0) print(i)
    }
    prob = sequence[i]
    
    if (prob == 0) { # R does not handle 0 well
      if (n_events == 0) {
        results[i] = 1
      } else {
        results[i] = 0
      }
    } else {
      results[i] = dbinom(n_events, nobs, prob)
    }
    
    if (prob > n_events / nobs) {
      if (results[i] < stop) break
    }
    
  }
  
  results = results[1:i]
  
  distribution = results / sum(results)
  
  return(distribution)
  
}



# Main code starts here

total_num = 100
sampled_num = 30
num_events = 2

by = 0.00001 
test_sequence = seq(from = 0, to = 1, by = by)

# Step 1. Estimate the probability of seeing x events in total num
total_events = list()

for (i in 0:total_num) {
  
  print(i)
  
  if (i < num_events) {
    total_events[[paste0(i)]] = list(p = 0)
  } else if (i > (total_num - (sampled_num - num_events))) {
    total_events[[paste0(i)]] = list(p = 0)
  } else {
    
    probability = exp(lchoose(i,num_events) + lchoose(total_num - i, sampled_num - num_events) - lchoose(total_num, sampled_num))
    total_events[[paste0(i)]] = list(p = probability) # This is the probability of sampling the outcome if we had total_num observations with i events
    
    # Generate the results using the function 
    dist = get_rate_distribution(i,total_num, test_sequence)
    total_events[[paste0(i)]]$conditional_density = dist
  }
}

# Normalize the probabilities
total_prob = sum(sapply(total_events, FUN = function(x) {x[['p']]}))
rate_distribution = 0
for (i in 0:total_num) {
  total_events[[paste0(i)]]$prob_of_events = total_events[[paste0(i)]]$p / total_prob
  
  if (!is.null(total_events[[paste0(i)]]$conditional_density)) {
    rate_distribution = rate_distribution + total_events[[paste0(i)]]$prob_of_events * total_events[[paste0(i)]]$conditional_density
  }
}

sum(rate_distribution) #This should be 1

# We can now use this distribution for the rate to calculate confidence intervals and significance of tolerance levels
rate_cdf = cumsum(rate_distribution)
rate_cdf_fun = approxfun(x = as.numeric(names(rate_cdf)), y = rate_cdf)
rate_inv_cdf_fun = approxfun(y = as.numeric(names(rate_cdf)), x = rate_cdf)

# Probability that rate is higher than tolerance
tolerance = 0.05
1 - rate_cdf_fun(tolerance)

# Percentile value
percentile = 0.975
rate_inv_cdf_fun(percentile)




##########
# Compare against the alternative method where we assume that the sampled observation is what we have
##########


require(binom)
binom.confint(num_events, sampled_num, conf.level = 0.95)

# Manual
distribution = get_rate_distribution(num_events,sampled_num, test_sequence)

tolerance = 0.05
which_above_tolerance = as.numeric(names(distribution)) > tolerance
sum(distribution[which_above_tolerance])

first_above_95 = min(which(cumsum(distribution) > 1-(1-0.95)/2))
perc_95 = as.numeric(names(distribution)[first_above_95])
perc_95

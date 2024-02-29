# This code compares the accuracy of the Confidence intervals for bootstrapping
require(boot)
mydata <- read.csv("fake_data.csv")

type = "type_variable"

all_biased = mydata[mydata[,type] == "Biased",1]
all_balanced = mydata[mydata[,type] == "Balanced",1]

ratio = length(all_balanced)/length(all_biased)

hist(all_biased)
hist(all_balanced)

# Generate a reasonable distribution for the population
require(MASS)
biased_t = fitdistr(all_biased, "t", start = list(m=mean(all_biased),s=sd(all_biased), df=3), lower=c(-2, 0.001,2))
biased_pop = rt(runif(1000000), biased_t$estimate[3])*biased_t$estimate[2] + biased_t$estimate[1]
biased_pop = biased_pop[abs(biased_pop) < max(abs(all_biased))]

balanced_t = fitdistr(all_balanced, "t", start = list(m=mean(all_balanced),s=sd(all_balanced), df=5), lower=c(-2, 0.001,2))
balanced_pop = rt(runif(floor(ratio*1000000)), balanced_t$estimate[3])*balanced_t$estimate[2] + balanced_t$estimate[1]
balanced_pop = balanced_pop[abs(balanced_pop) < max(abs(all_balanced))]

hist(biased_pop)
hist(balanced_pop)

diff.medians <- function(df, ind){
  computation.df <- df[ind,]
  median.A <- median(computation.df[computation.df[,"Type"] == "Biased",1])
  median.B <- median(computation.df[computation.df[,"Type"] == "Balanced",1])
  return(median.B - median.A)
}


trials = 10000
inner_trials = 1000
sample_size = nrow(mydata)

basic_total = as.data.frame(matrix(NA, nrow = trials*5, ncol = 6))
colnames(basic_total) = c("alpha", "A","B","L","U","Trial")

perc_total = basic_total
norm_total = basic_total
bca_total = basic_total

norm_total = as.data.frame(matrix(NA, nrow = trials*5, ncol = 4))
colnames(norm_total) = c("alpha", "L","U","Trial")

set.seed(100)
for (tt in 1:trials) {
  
  if (tt%%10 == 0) print(tt)
  
  sampled_idx = sample(1:(length(biased_pop) + length(balanced_pop)), sample_size, replace = F)  
  biased_sample = biased_pop[sampled_idx[sampled_idx <= length(biased_pop)]]
  balanced_sample = balanced_pop[sampled_idx[sampled_idx > length(biased_pop)]-length(biased_pop)]
  
  newdata = data.frame("Value" = c(biased_sample, balanced_sample), 
                       "Type" = c(rep("Biased", length(biased_sample)),rep("Balanced", length(balanced_sample))))
  
  bootstrap.samples <- boot(newdata, diff.medians, R=inner_trials)
  
  
  CI = boot.ci(boot.out = bootstrap.samples, type = c("basic", "perc", "norm", "bca"), conf = c(0.95, 0.9, 0.8, 0.7, 0.6))
  
  basic = as.data.frame(CI$basic)
  basic$trial = tt
  
  perc = as.data.frame(CI$perc)
  perc$trial = tt
  
  norm = as.data.frame(CI$norm)
  norm$trial = tt
  
  bca = as.data.frame(CI$bca)
  bca$trial = tt
  
  basic_total[(5*(tt-1)+1):(5*tt),] = basic
  perc_total[(5*(tt-1)+1):(5*tt),] = perc
  norm_total[(5*(tt-1)+1):(5*tt),] = norm
  bca_total[(5*(tt-1)+1):(5*tt),] = bca
  
}

median_diff = median(balanced_pop) - median(biased_pop)

basic_total$value = (basic_total$L < median_diff) & (basic_total$U > median_diff)
aggregate(basic_total$value, by = list(basic_total$alpha), FUN = mean)

perc_total$value = (perc_total$L < median_diff) & (perc_total$U > median_diff)
aggregate(perc_total$value, by = list(perc_total$alpha), FUN = mean)

norm_total$value = (norm_total$L < median_diff) & (norm_total$U > median_diff)
aggregate(norm_total$value, by = list(norm_total$alpha), FUN = mean)

bca_total$value = (bca_total$L < median_diff) & (bca_total$U > median_diff)
aggregate(bca_total$value, by = list(bca_total$alpha), FUN = mean)



require(boot)
require(data.table)

mydata <- read.csv("fake_data.csv")

# Bootstrap quantile regressions without size
quantile_nosize <- function(df, ind){
  computation.df <- df[ind,]
  rq1 = rq(y ~ x, data=computation.df, tau = 0.5000001) # For some reason exactly 0.5 causes an issue
  coef_value = -unname(rq1$coefficients[2])
  return(coef_value)
}

set.seed(100000)
bootstrap.samples <- boot(mydata, quantile_nosize, R=1000)
CI = boot.ci(boot.out = bootstrap.samples, type = c("basic", "perc", "norm", "bca"), conf = c(0.95, 0.9, 0.8, 0.7, 0.6))
CI
# Mean coefficient
mean(bootstrap.samples$t)


# Bootstrap quantile regressions with size, generating CI for x
quantile_size <- function(df, ind){
  computation.df <- df[ind,]
  rq1 = rq(y ~ x + ln_fs, data=computation.df, tau = 0.5000001) # For some reason exactly 0.5 causes an issue
  coef_value = -unname(rq1$coefficients[2])
  return(coef_value)
}
set.seed(100000)
bootstrap.samples_size <- boot(mydata, quantile_size, R=1000)
CI_size = boot.ci(boot.out = bootstrap.samples_size, type = c("basic", "perc", "norm", "bca"), conf = c(0.95, 0.9, 0.8, 0.7, 0.6))
CI_size
mean(bootstrap.samples_size$t)



# Bootstrap quantile regressions with size, generating CI for x2
quantile_size <- function(df, ind){
  computation.df <- df[ind,]
  rq1 = rq(y ~ x + x2, data=computation.df, tau = 0.5000001) # For some reason exactly 0.5 causes an issue
  coef_value = unname(rq1$coefficients[3])
  return(coef_value)
}
set.seed(100000)
bootstrap.samples_size <- boot(mydata, quantile_size, R=1000)
CI_size = boot.ci(boot.out = bootstrap.samples_size, type = c("basic", "perc", "norm", "bca"), conf = c(0.95, 0.9, 0.8, 0.7, 0.6))
CI_size
mean(bootstrap.samples_size$t)
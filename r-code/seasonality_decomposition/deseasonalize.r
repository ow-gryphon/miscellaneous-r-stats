# Fake some data
insample = AirPassengers[1:100]
outofsample = AirPassengers[101:144]

# Create timeseries
freq = 12 # 4 for Quarterly, 12 for monthly

insample = ts(insample, start = 1990.00, frequency = freq)
outofsample = ts(outofsample, start = 1998+4/12, frequency = freq)


#### Additive seasonality ####
stl_result = stl(insample, "periodic")
plot(stl_result)

all(rowSums(stl_result$time.series) == insample)

# Deseasonalize forecast
seasonality = stl_result$time.series[1:freq,1]
times = round(1+round(freq*(time(stl_result$time.series)[1:freq]),1) %% freq)
names(seasonality) = times

# Extract times from outofsample
outofsample_deseas = outofsample - seasonality[match(1+round(freq*time(outofsample),1) %% freq,names(seasonality))] 
plot(outofsample_deseas, ylim = c(min(outofsample,na.rm=T), max(outofsample,na.rm=T)), type = "l")
lines(outofsample, col = 'red')

#### Multiplicative seasonality ####
stl_result = stl(log(insample), "periodic")
plot(stl_result)

all(rowSums(stl_result$time.series) == log(insample))

# Deseasonalize forecast
seasonality = stl_result$time.series[1:freq,1]
times = round(1+round(freq*(time(stl_result$time.series)[1:freq]),1) %% freq)
names(seasonality) = times

# Extract times from outofsample
outofsample_deseas = exp(log(outofsample) - seasonality[match(1+round(freq*time(outofsample),1) %% freq,names(seasonality))])
plot(outofsample_deseas, ylim = c(min(outofsample,na.rm=T), max(outofsample,na.rm=T)), type = "l")
lines(outofsample, col = 'red')

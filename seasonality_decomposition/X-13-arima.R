require(seasonal)
require(seasonalview)
# See more here https://github.com/christophsax/seasonal/wiki/Examples-of-X-13ARIMA-SEATS-in-R

# Create a fake exogenous variable
N = length(AirPassengers) + 120
vars = list()
for (i in 1:100) {
  vars[[i]] = cumsum(rnorm(N))
}

results = unlist(lapply(vars, FUN = function(x) {cor(x[1:length(AirPassengers)], AirPassengers)}))
max_id = which.max(abs(results))
second_max_id = which.max(abs(results[-max_id]))

x1 = vars[[max_id]]
if (second_max_id >= max_id) {
  x2 = vars[[second_max_id+1]]
} else {
  x2 = vars[[second_max_id]]
}

x1 = ts(x1, start = 1949, freq=12)
x2 = ts(x2, start = 1949, freq=12)
# Perform de-seasonalization

# Basic 
deseasoned_1 = seas(AirPassengers)
summary(deseasoned_1)
view(deseasoned_1)

# Deseasonalize with constant seasonality 
deseasoned_2 = seas(AirPassengers, 
                    regression.aictest = NULL,
                    regression.variables = c("const", "seasonal"))
summary(deseasoned_2)
view(deseasoned_2)

# Conditional expectation with Regression
deseasoned_3 = seas(AirPassengers, 
                    xreg = cbind(x1,x2),
                    regression.aictest = NULL,
                    regression.variables = c("const", "seasonal"))
summary(deseasoned_3)
view(deseasoned_3)

   # Forecast
deseasoned_4 = seas(AirPassengers, 
                    xreg = cbind(x1,x2),
                    regression.aictest = NULL,
                    forecast.maxlead = 60,
                    regression.variables = c("const", "seasonal"), x11.appendfcst = "yes")

view(deseasoned_4)

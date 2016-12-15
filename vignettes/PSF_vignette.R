## ------------------------------------------------------------------------
library(PSF)

## ------------------------------------------------------------------------
a <- psf(data = nottem, n.ahead = 12)
a

## ------------------------------------------------------------------------
b <- psf(data = sunspots, n.ahead = 48)
b

## ---- fig.width = 7, fig.height = 4--------------------------------------
psf_plot(data = nottem, predictions = a$predictions)

## ---- fig.width = 7, fig.height = 4--------------------------------------
psf_plot(data = sunspots, predictions = b$predictions)

## ------------------------------------------------------------------------
library(PSF)
library(forecast)
options(warn=-1)
  
## Consider data `sunspots` with removal of last years's readings
# Training Data
x <- sunspots[1:2772]

# Test Data
y <- sunspots[2773:2820]

PSF <- NULL
ARIMA <- NULL
ETS <- NULL

for(i in 1:5)
{
  set.seed(i)
  
  # for PSF
  a <- psf(data = x, n.ahead = 48)$predictions
  
  # for ARIMA
  b <- forecast(auto.arima(x), 48)$mean
  
  # for ets
  c <- as.numeric(forecast(ets(x), 48)$mean)
  
  ## For Error Calculations
  # Error for PSF
  PSF[i] <- sqrt(mean((y - a)^2))
  # Error for ARIMA
  ARIMA[i] <- sqrt(mean((y - b)^2))
  # Error for ETS
  ETS[i] <- sqrt(mean((y - c)^2))

}

## Error values for PSF
  PSF
  mean(PSF)
  
## Error values for ARIMA
  ARIMA
  mean(ARIMA)
  
## Error values for ETS
  ETS
  mean(ETS)


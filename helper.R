#install.packages('tidyquant')
#install.packages('bsts')
library(tidyquant)
library(zoo)
library(dplyr)
library(bsts)
library(lubridate)
library(ranger)
library(tidytext)
library(ROCR)
library(tidyverse)

# helper functions relates to tweets data----------------------------------------------------------------------------------------
# bag of words related to cryptos
bag_of_words <- c('blockchain', 'coin', 'coinbase', 'cryptocurrency', 'cryptocurrencies',
                  'decentralization', 'defi',
                  'altcoin', 'altcoins', 'bitcoin', 'bitcoins', 'ethereum', 'ethereums', 'dogecoin', 'dogecoins', 'cardano',
                  'exchange', 'nft', 'nfts',
                  'btc', '#btc', '$btc',
                  'xbt', '#xbt', '$xbt',
                  'eth', '#eth', '$eth',
                  'doge', '#doge', '$doge',
                  'bnb', '#bnb', '$bnb',
                  'ada', '#ada', '$ada',
                  'xpr', '#xpr', '$xpr',
                  'dash', '#dash', '$dash',
                  'satoshi', 'nakamoto',
                  'binance', 'U+1F415')

# a csv file contains the tweets that elon musk post for last year 
tweet_elon <- read_csv('../tweets/data_tweets/tweet_elon.csv')


# using tidytext to unnest all the tweets
tweet_elon <- 
  tweet_elon %>% 
  dplyr::select(-...1)

tweet_elon_token <- 
  tweet_elon %>% 
  unnest_tokens(words, text)

# only keep those words that relates to cryptos using bag of words
tweet_elon_crypto <- 
  tweet_elon_token %>% 
  filter(words %in% bag_of_words)

# write.csv(tweet_elon_crypto, '../tweets/data_tweets/tweet_elon_crypto.csv')

# count how many tweets related to cryptocurrencies that Elon tweeted
tweet <- 
  tweet_elon_crypto %>% 
  mutate(date = as_date(format(created_at, format = "%Y-%m-%d"))) %>% 
  group_by(date) %>% 
  mutate(num_tweets = n()) %>% 
  distinct(date, num_tweets)


# helper functions relates to more complex time series model (ARIMA, random forest, Bayesian structural time series)------------------------------------------------------

# some famous tickers for cryptocurrencys and large companies
tickers = c("ETH-USD", "BTC-USD", "DOGE-USD", "USDT-USD", "SOL-USD", "BCH-USD",
            "LTC-USD", # other cryptos
            "UST", "SPY", "UBT", # S%P500 and bonds
            "EURUSD=X", "JPY=X", "GBPUSD=X", # forex
            "GC=F", "CL=F", "NG=F", # gold, oil, natural gas futures
            "^DJUSS", "^MID", # small and mid cap stock ETF's
            "TSM", # Largest chip producer
            "GOOGL", "TSLA", "AMZN", # big tech stocks 
            "NVDA", # Make GPU's used for mining
            "JPM", "BLK", "CBRE", # Financial and real estate
            "DIS" # disney, entertainment sector
)

# last value carry forward
lvcf <- function(x){
  # if it is the first day, impute zero since it nothing to calculate
  if(is.na(x[1])){
    x[1] = 0
  }
  # if that day is no return(NA), impute the previous day's value
  for(i in 2:length(x)){
    if(is.na(x[i])){
      x[i] = x[i-1]
    }
  }
  return(x)
}

# make a column calculate the percentage change from the other day since return is calculated by 
# [(New Adjusted Closing Price - Old Adjusted Price)/Old Price] *100
pc_col <- function(x){
  return(((x - lag(x, 1))/lag(x, 1)) * 100)
}

#  big chunk of code to create the dataset of cryptocurrency 
create_data <- function(start_date, end_date, ticks = tickers, ts = T, lags = 3, difference_cryptos = T){
  # using tq_get() to obtain the crypto data from selected tickers,
  # date and their adjusted closing value
  prices <- tq_get(ticks,
                   from = start_date,
                   to = end_date,
                   get = "stock.prices")
  prices <- prices %>% dplyr::select(date, symbol, adjusted)
  priceH <- data.frame()
  
  # merge all the table which create previously in a single dataframe. One column for date, and the rest columns
  # are all the tickers we select at the beginning with their adjusted prices                  
  for ( i in ticks){
    df <- prices %>% filter(symbol == i) %>% dplyr::select(date, adjusted)
    colnames(df) <- c("date",i)
    if(nrow(priceH) == 0){
      priceH <- df
    }
    else{
      priceH <- merge(priceH, df, by = "date", all =T)
    }
  }
  # applying lvcf function to handle weird situation like missing value 
  for(i in ticks){
    priceH[,i] <- lvcf(priceH[,i])
  }
  
  # finally, we convert all of prices to return (percentage change) by calling pc_col() function which build previously
  returns <- priceH
  for(i in ticks){
    returns[,i] <- pc_col(returns[,i])
  }
  # if we found the value makes non sense like infinite, impute NA
  returns[returns == Inf] <- NA
  returns[is.na(returns)] <- NA
  returns <- na.omit(returns)
  if(difference_cryptos == T){
    returns <- returns %>%
      mutate(doge_delta = `DOGE-USD` - returns$`BTC-USD`,
             sol_delta = `SOL-USD` - returns$`BTC-USD`,
             bch_delta = `BCH-USD` - returns$`BTC-USD`,
             ltc_delta = `LTC-USD` - returns$`BTC-USD`) %>%
      dplyr::select(-`DOGE-USD`, -`SOL-USD`, -`BCH-USD`, -`LTC-USD`)
  }
  if(ts == F){
    returns$weekday <- weekdays(returns$date)
    lag_cols <- colnames(returns)
    lag_cols <- lag_cols[! lag_cols %in% c("date", "weekday")]
    for(i in lag_cols){
      for(l in 1:lags){
        name <- paste0(i,'_lag',l)
        returns[,name] <- dplyr::lag(returns[,i],l)
        returns[,name][is.na(returns[,name])] <- 0
      }
    }
  }
  return(returns)
}

# a function to helps us manipulate the formula when we fit model
get_formula <- function(target, covs){
  formula <- paste0("`",target, "`", ' ~ ')
  for(i in covs){
    formula <- paste0(formula, "`", i, "`", " + ")
  }
  formula <- substr(formula,1,nchar(formula)-3)
  return(formula)
}

# This function is doing the training, testing and predict for Arima model
back_test_arima <- function(series, data, covs, train_pers, first_pred, last_pred, order_v = c(3,0,3)){
  preds <- c()
  trues <- c()
  # a sequence contains all the date that we want to make prediction on 
  pred_dates <- seq(as.Date(first_pred), as.Date(last_pred), by="day")
  
  # Then, for each day, we train the model on a period leading up to the day we want to predict, and make prediction on
  # that day. ex: if we want to predict 2022-01-26, we need to use say like 90 days leading up to that day (2021-10-27 to 2021-01-25)
  # as training set
  for(i in pred_dates){
    train <- data %>% 
      filter(date >= ymd(as.Date(i)) - days(train_pers + 1),
             date <= ymd(as.Date(i)) - days(1))
    test <- data %>% filter(date == as.Date(i))
    model <- arima(train[,series], order=order_v, xreg = train[,covs])
    pred <- predict(model, newxreg = subset(test, select = covs))
    pred <- as.vector(pred$pred[1])
    
    # After modeling and predicting, we compare the prediction with the true value to see how well the fit is
    true <- test[,series]
    preds <- c(preds, pred)
    trues <- c(trues, true)
  }
  # merging the true values and predicted value then compute their residuals for analysis
  backtest <- data.frame(date = pred_dates, true = trues, pred = preds) %>%
    mutate(residuals = trues - preds,
           sq_residuals = residuals^2)
  return(backtest)
}

# function for training, testing and predicting for Bayesian structural time series model. The general structure still 
# follows by the previous function 
back_test_bsts <- function(series, data, covs, train_pers, first_pred, last_pred, n_lags = 3){
  formula <- get_formula(series, covs)
  preds <- c()
  trues <- c()
  pred_dates <- seq(as.Date(first_pred), as.Date(last_pred), by="day")
  for(i in pred_dates){
    train <- data %>% 
      filter(date >= ymd(as.Date(i)) - days(train_pers + 1),
             date <= ymd(as.Date(i)) - days(1))
    test <- data %>% filter(date == as.Date(i))
    ss <- AddLocalLevel(list(), y = train[,series])
    ss <- AddAr(ss,
                train[,series],
                lags = n_lags)
    model <- bsts(formula, state.specification = ss, niter = 1000, 
                  ping=0, seed=2016, data = train, expected.model.size = 3)
    pred <- predict(model, newdata = subset(test, select = covs))
    pred <- pred$mean
    true <- test[,series]
    preds <- c(preds, pred)
    trues <- c(trues, true)
  }
  
  backtest <- data.frame(date = pred_dates, true = trues, pred = preds) %>%
    mutate(residuals = trues - preds,
           sq_residuals = residuals^2)
  return(backtest)
}

# function for training, testing, and predicting for random forest model, similar structure as above
back_test_rf <- function(series, data, covs, train_pers, first_pred, last_pred){
  preds <- c()
  trues <- c()
  pred_dates <- seq(as.Date(first_pred), as.Date(last_pred), by="day")
  for(i in pred_dates){
    train <- data %>% 
      filter(date >= ymd(as.Date(i)) - days(train_pers + 1),
             date <= ymd(as.Date(i)) - days(1))
    test <- data %>% filter(date == as.Date(i))
    model <- ranger(data = train, x = train[,covs], y = train[,series])
    pred <- predict(model, test)$predictions
    true <- test[,series]
    preds <- c(preds, pred)
    trues <- c(trues, true)
  }
  backtest <- data.frame(date = pred_dates, true = trues, pred = preds) %>%
    mutate(residuals = trues - preds,
           sq_residuals = residuals^2)
  return(backtest)
}


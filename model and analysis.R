# source code of all helper functions. please make sure run all of them
source('helper.R')
library(ggplot2)
options("getSymbols.warning4.0"=FALSE)
options("getSymbols.yahoo.warning"=FALSE)


# Part I: Using tweets relates to cryptocurrency and crypto return to build model and make prediction(easy model)-------------------------------------------------------------

# crypto data from 2021-04-28 to 2022-04-26
start_date <- "2021-04-28"
end_date <- "2022-04-26"

# we want to use those four companies' cryptocurrency data "ETH-USD", "BTC-USD", "DOGE-USD", "SOL-USD", "BCH-USD"

# For each company, use tq_get() to obtain their stock price from 2021-04-28 to 2022-04-26
# then compute the difference between their open value and close value 
# we set the outcome be 1 if the daily the absolute return is greater than or equal to 5%, 0 otherwise. do the same thing for
# the table below (doge, btc, sol, bch)
eth <- 
  tq_get("ETH-USD",
         from = start_date,
         to = end_date,
         get = "stock.prices") %>% 
  mutate(eth_returns = pc_col(adjusted)) %>% 
  mutate(eth = ifelse(abs(eth_returns) < 5, 0, 1)) %>% 
  dplyr::select(date, eth_returns, eth)

doge <- 
  tq_get("DOGE-USD",
         from = start_date,
         to = end_date,
         get = "stock.prices") %>% 
  mutate(doge = pc_col(adjusted)) %>% 
  mutate(doge = ifelse(abs(doge) < 5, 0, 1)) %>% 
  dplyr:: select(date, doge)

btc <- 
  tq_get("BTC-USD",
         from = start_date,
         to = end_date,
         get = "stock.prices") %>% 
  mutate(btc = pc_col(adjusted)) %>% 
  mutate(btc = ifelse(abs(btc) < 5, 0, 1)) %>% 
  dplyr::select(date, btc)

sol <- 
  tq_get("SOL-USD",
         from = start_date,
         to = end_date,
         get = "stock.prices") %>% 
  mutate(sol = pc_col(adjusted)) %>% 
  mutate(sol = ifelse(abs(sol) < 5, 0, 1)) %>% 
  dplyr::select(date, sol)

bch <- 
  tq_get("BCH-USD",
         from = start_date,
         to = end_date,
         get = "stock.prices") %>% 
  mutate(bch = pc_col(adjusted)) %>% 
  mutate(bch = ifelse(abs(bch) < 5, 0, 1)) %>% 
  dplyr::select(date, bch)

# finally, we join them together with the number of tweets that elon musk post on each day (those only relates to cryptocurrency)
eth <- 
  eth %>% 
  left_join(doge) %>% 
  left_join(btc) %>% 
  left_join(tweet) %>% 
  left_join(sol) %>% 
  left_join(bch) %>% 
  mutate(num_tweets = replace_na(num_tweets, 0))
eth <- eth[-1, ]

## Simple logistic regression---------------------------------------------------------------
# we first fit the model with a simple logistic regression and calculate its AUC score

# fit a logistic regression 
logistic <- glm(eth ~ doge + btc + sol + bch + num_tweets, data = eth, family = 'binomial')
summary(logistic)

# make prediction for simple logistic regression
logistic_prob <- predict(logistic, eth)

# AUC
logistic_pred <- prediction(logistic_prob, eth$eth)
logisitc_performace <- performance(logistic_pred, 'auc')
cat('The AUC score of the simple logisitc is ', logisitc_performace@y.values[[1]], "\n")

# plot tweets and returns to see if there is a pattern 
eth_plot <- 
  eth %>% 
  mutate(eth = ifelse(abs(eth_returns) < 5, 0, abs(eth_returns))/3)

p <- 
  ggplot(data = eth_plot, aes(x = date)) +
  geom_line(aes(y = eth, colour = "eth_abs_returns/3")) +
  geom_line(aes(y = num_tweets, colour = "num_tweets")) +
  scale_colour_manual("", 
                      breaks = c("eth_abs_returns/3", "num_tweets"),
                      values = c("blue", "red"))
p

# ggsave(plot = p, file = '../figures/tweets_and_returns.png', height = 5, width = 10)

# Part II: Introducing time series in order to do a better fit (complex models)-----------------------------------------------------

## Creating the dataset and specify predictors-----
# This dataset is the stock price data from 2021-10-25 to 2022-04-26 with time series play roles 
returns_ts <- create_data("2021-10-25", "2022-04-26") # add day for returns

# Same dataset, except we don't want to include time series, so set ts=F
returns <- create_data("2021-10-25", "2022-04-26", ts = F)

# predictors for Arima and bsts model
covars <- colnames(returns_ts)
covars <- covars[! covars %in% c("ETH-USD", "date")]

# predictors for random forest models 
covars_rf <- colnames(returns)
covars_rf <- covars_rf[! covars_rf %in% c("ETH-USD", "date")]

# alternative dataset (might need to use to do comparison)
covars2 <- c("BTC-USD","doge_delta" , "USDT-USD", "GC=F", "BLK", "JPM", "CBRE", 
             "ltc_delta", "SPY", "^DJUSS", "^MID", "sol_delta", "GOOGL")



## Arima model------
# This is the arima model where our target is "ETH-USD'(Ethereum), using return_ts as dataset and covars as
# predictors. make prediction on each day from 2022-01-26 to 2022-04-26 
arima1 <- back_test_arima("ETH-USD", returns_ts, covars, 90, "2022-01-26", "2022-04-26")

# plot the true value vs predicted value to see how good the fit visually
#, where true value is in read, predicted value is in blue
p1 <- ggplot(data = arima1, aes(x = date)) +
  geom_line(aes(y = true, colour = "true")) +
  geom_line(aes(y = pred, colour = "pred")) +
  labs(x = 'Date',
       y = 'Percentage Change(return)')+
  scale_colour_manual("", 
                      breaks = c("pred", "true"),
                      values = c("blue", "red"))+
  ggtitle("Arima model: True vs Predict")
p1
# ggsave(plot = p1, file = '../figures/arima.png', height = 4, width = 7)
# calculate the average square of residual to see how well the fit mathematically
cat('The square of residual for Arima model is', mean(arima1$sq_residuals))


## Bayesian structural time series model--------
# This bsts model still use ETH-USD (Ethereum) as the target, and return_ts as dataset, and covars as 
# predictors. Then, making predictions on each day from 2022-01-26 to 2022-04-26
bsts1 <- back_test_bsts("ETH-USD", returns_ts, covars, 90, "2022-01-26", "2022-04-26")

# plot the true value vs predicted value to see how good the fit visually
#, where true value is in read, predicted value is in blue
p2 <- ggplot(data = bsts1, aes(x = date)) +
  geom_line(aes(y = true, colour = "true")) +
  geom_line(aes(y = pred, colour = "pred")) +
  labs(x = 'Date',
       y = 'Percentage Change(return)')+
  scale_colour_manual("", 
                      breaks = c("pred", "true"),
                      values = c("blue", "red"))+
  ggtitle("BSTS model: True vs Predict")
p2
#ggsave(plot = p2, file = '../figures/bsts_true_and_predict.png', height = 4, width = 7)

# calculate the average square of residual to see how well the fit mathematically
cat('The square of residual for BSTS is', mean(bsts1$sq_residuals))

## Random Forest model-------
# This random forest model still use ETH-USD(Ethereum) as the target, but we use returns as dataset since we don't actually 
# apply time series in this model, even through it looks like a time series model
rf1 <- back_test_rf("ETH-USD", returns, covars_rf, 90, "2022-01-26", "2022-04-26")

# plot the true value vs predicted value to see how good the fit visually
#, where true value is in read, predicted value is in blue
p3 <- ggplot(data = rf1, aes(x = date)) +
  geom_line(aes(y = true, colour = "true")) +
  geom_line(aes(y = pred, colour = "pred")) +
  labs(x = 'Date',
       y = 'Percentage Change(return)')+
  scale_colour_manual("", 
                      breaks = c("pred", "true"),
                      values = c("blue", "red"))+
  ggtitle("Random Forest model: True vs Predict")
p3
# ggsave(plot = p3, file = '../figures/rf1_true_and_predict.png', height = 4, width = 7)

# calculate the average square of residual to see how well the fit mathematically
cat('The square of residual for random forest', mean(rf1$sq_residuals))


# Edit
After sharing my results with other option traders, I made some adjustments to the parameters of the model which I had constructed.
The new parameters are 0.02-2% risk (prev. 1-5%) and 10-45% roi (prev. 30-50%).
These changes seem to make the strategy a bit more effective, as long as proper risk management is made use of.
However, I have not implemented any new features such as trade management, but i look to add that in the future

# Vertical Credit Spreads
As an options trader I constantly think about wats to implement effective strategies aimed to capture realized volatility relatively to 
volatility implied by the options market. One way to do this is a vertical credit spread. This allows us to skew the probability of success
to be greater than 50/50 with the tradeoff of a capped profit potential. basically, The higher the POS, the lower the ROI and vice versa.
A vertical call spread is made up of two legs. Selling a call is one leg and simultaneously buying call is the second leg.  
The call being sold is the strike aimed to expire worthless, when the strike expires out-the-money(otm) by expiration, the full credit is collected as a profit. 
But to hedge against the risk of the strike moving in-the-money(itm) by expiration – in addition buy a call to define our risk. By doing this, 
we know exactly how much we are risking on the trade before execution. That should be enough of a synopsis, lets begin: 
  
  
  # Process
  To simulate the process of performing this trade many times, I first began with creating a basic function.
This starter function simulates one instance of the Vertical Credit Spread.
Here I construct the basic portions of the necessary mechanisms such as starting capital, risk per trade, and return on investment from a trade (roi).
```{r}
simulate_options = function(n, initial_capital = 10000, roi) {
  # first, do one round of put credit spread
  risk_per_trade = sample(1:5, 1) * 0.01 # Randomly risk 1-5% of capital
  capital_per_trade = initial_capital * risk_per_trade
  profit_per_trade = capital_per_trade * roi
  print(profit_per_trade)
}

simulate_options(n = 1, roi = 0.3)
```

# Version 2
In the second version of the simulation, I begin to use a for loop to simulate 'n' rounds of trading.
I include a separate function which determines the profit or loss depending if the trade is profitable or not.
```{r}
trade_direction = function(x, max_loss, roi) {
  if (x == 1) { # Stock goes up
    max_loss = max_loss * roi
  } else if (x == 2) { # Stock goes sideways
    max_loss = max_loss * roi
  } else { # Stock goes down
    max_loss = -1 * max_loss
  }
  return(max_loss)
} # Determine result of trade whether stock goes up or sideways

simulate_options2 = function(n, initial_capital = 10000, risk_per_trade = 0.01, roi = 0.3) {
  profit_loss_vector = rep(0,n) # Initialize profit/loss vector
  profit_loss_vector[1] = initial_capital # Include starting capital
  
  for (i in 1:n) {
    max_loss = profit_loss_vector[i] * risk_per_trade # Capital at risk per round
    x = sample(1:3, 1) # Simulate stock going up, down, or sideways
    max_loss = trade_direction(x = x, max_loss = max_loss, roi = roi) # Determine result of movement
    profit_loss_vector[i+1] = profit_loss_vector[i] + max_loss
  }
  return(profit_loss_vector)
}

plot(simulate_options2(n = 100), xlab = "Number of Trades", ylab = "Amount of Money", main = "Capital Over Time")
```

# Version 3
Here in the third version I change the separate function so that it allows for different % win rates to be tested.
This is done by comparing the results of runif (random uniform distribution) to a certain standard, e.g., 0.33.
```{r}
trade_direction2 = function(x, max_loss, roi) {
  if (x > 0.33) {
    max_loss = max_loss * roi
  } else {
    max_loss = -1 * max_loss
  }
  return(max_loss)
}

simulate_options3 = function(n, initial_capital = 10000, risk_per_trade = 0.01, roi = 0.3) {
  profit_loss_vector = rep(0,n) # Initialize profit/loss vector
  profit_loss_vector[1] = initial_capital # Include starting capital
  
  for (i in 1:n) {
    max_loss = profit_loss_vector[i] * risk_per_trade # Capital at risk per round
    x = runif(1) # Randomize trade direction
    max_loss = trade_direction2(x = x, max_loss = max_loss, roi = roi) # Determine result of movement
    profit_loss_vector[i+1] = profit_loss_vector[i] + max_loss
  }
  return(profit_loss_vector)
}

plot(simulate_options3(n = 1000), xlab = "Number of Trades", ylab = "Amount of Money", main = "Capital Over Time")
```

# Version 4
In the final version, I begin to randomize other parameters such as the risk per trade, which is 1-5% of the capital.
Another parameter that I randomize is the return on investment (roi) per trade, which is changed to 30-50% (admittedly slightly too large).
However, after several rounds of testing, it seems that the only possible parameter that can be changed which would allow for a profitable series of trades is the win rate.
After making it such that the trades are successful 80% of the time (up from 66%), 
the pattern is such that the winning trades will outperform the losing trades (indicated by an uptrending capital amount).
```{r}
trade_direction3 = function(x, max_loss, roi) {
  if (x > 0.2) {
    max_loss = max_loss * roi
  } else {
    max_loss = -1 * max_loss
  }
  return(max_loss)
}

simulate_options4 = function(n, initial_capital = 10000, risk_per_trade, roi) {
  profit_loss_vector = rep(0,n) # Initialize profit/loss vector
  profit_loss_vector[1] = initial_capital # Include starting capital
  
  for (i in 1:n) {
    risk_per_trade = sample(1:5,1) * 0.01 # Randomly risk 1-5%
    roi = runif(n = 1, min = 3, max = 5) * 0.1 # Randomize an ROI from 30-50%
    max_loss = profit_loss_vector[i] * risk_per_trade # Capital at risk per round
    x = runif(1) # Randomize trade direction
    max_loss = trade_direction3(x = x, max_loss = max_loss, roi = roi) # Determine result of movement
    profit_loss_vector[i+1] = profit_loss_vector[i] + max_loss
  }
  return(profit_loss_vector)
}

plot(simulate_options4(n = 1000), xlab = "Number of Trades", ylab = "Amount of Money", main = "Capital Over Time")
```


# (1) I decided to change the risk per trade from 1-5% to 0.02-2% after asking for clarity from the person that taught me options.
(2) Also, I changed the return on investment per trade to some random % from 10-45%.
I was told that low DTE trades can have smaller r.o.i.'s by another trader.
After making these changes I saw rather large differences in the results.
(3) I was able to change the win rate back to 66% and still discover an uptrending capital over time.
I apologize for any confusion that I may have caused.
The model that I created is still rough, since it lacks qualities such as trade management.

# Version 5
```{r}
trade_direction3 = function(x, max_loss, roi) {
if (x > 0.33) { # 66% win rate
max_loss = max_loss * roi
} else {
max_loss = -1 * max_loss
}
return(max_loss)
}

simulate_options5 = function(n, initial_capital = 10000, risk_per_trade, roi) {
profit_loss_vector = rep(0,n) # Initialize profit/loss vector
profit_loss_vector[1] = initial_capital # Include starting capital

for (i in 1:n) {
risk_per_trade = sample(2:20,1)*0.001 # Randomly risk 0.02-2%
roi = runif(n = 1, min = 1, max = 4.5) # Randomize an ROI from 10-45%
max_loss = profit_loss_vector[i] * risk_per_trade # Capital at risk per round
x = runif(1) # Randomize trade direction
max_loss = trade_direction3(x = x, max_loss = max_loss, roi = roi) # Determine result of movement
profit_loss_vector[i+1] = profit_loss_vector[i] + max_loss
}
return(profit_loss_vector)
}

plot(simulate_options5(n = 300), xlab = "Number of Trades", ylab = "Amount of Money", main = "Capital Over Time")

```
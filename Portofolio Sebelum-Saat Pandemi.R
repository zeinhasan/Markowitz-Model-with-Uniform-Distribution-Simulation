library(plotly) # To create interactive charts
library(timetk) # To manipulate the data series
library(tibble)

# Read data
price_data <- read.delim("clipboard")
colnames(price_data)

# Log return function
BBCA_Ret <- diff(log(price_data$BBCA), lag=1)
BBRI_Ret <- diff(log(price_data$BBRI), lag=1)
BMRI_Ret <- diff(log(price_data$BMRI), lag=1)
ASII_Ret <- diff(log(price_data$ASII), lag=1)
HMSP_Ret <- diff(log(price_data$HMSP), lag=1)
UNVR_Ret <- diff(log(price_data$UNVR), lag=1)

# Make dataframe
log_ret = data.frame(BBCA_Ret,BBRI_Ret,BMRI_Ret,ASII_Ret,HMSP_Ret,UNVR_Ret)
log_ret
# Mean Return
mean_ret <- colMeans(log_ret)
print(round(mean_ret, 5))

# Covar return
cov_mat <- cov(log_ret)
print(round(cov_mat,5))

# Random Unif
wts <- runif(n = 6)
print(wts)
print(sum(wts))

wts <- wts/sum(wts)
print(wts)
print(sum(wts))

# Port Return
port_returns <- (sum(wts*mean_ret))
port_returns

# Port Risk
port_risk <- sqrt(t(wts) %*% (cov_mat %*% wts))
print(port_risk)

# Sharpe Ratio
sharpe_ratio <- port_returns/port_risk
print(sharpe_ratio)


# Simulate 
num_port <- 1000000

# Creating a matrix to store the weights

all_wts <- matrix(nrow = num_port,
                  ncol = 6)

# Creating an empty vector to store
# Portfolio returns

port_returns <- vector('numeric', length = num_port)

# Creating an empty vector to store
# Portfolio Standard deviation

port_risk <- vector('numeric', length = num_port)

# Creating an empty vector to store
# Portfolio Sharpe Ratio

sharpe_ratio <- vector('numeric', length = num_port)

# Iterate
for (i in seq_along(port_returns)) {
  
  wts <- runif(n=6)
  wts <- wts/sum(wts)
  
  # Storing weight in the matrix
  all_wts[i,] <- wts
  
  # Portfolio returns
  port_ret <- sum(wts * mean_ret)
  port_ret <- ((port_ret + 1)^52) - 1
  # Storing Portfolio Returns values
  port_returns[i] <- port_ret
  
  
  # Creating and storing portfolio risk
  port_sd <- sqrt(t(wts) %*% (cov_mat  %*% wts))
  port_risk[i] <- port_sd
  
  # Creating and storing Portfolio Sharpe Ratios
  # Assuming 0% Risk free rate
  
  sr <- port_ret/port_sd
  sharpe_ratio[i] <- sr
  
}

# Storing the values in the table
portfolio_values <- tibble(Return = port_returns,
                           Risk = port_risk,
                           SharpeRatio = sharpe_ratio)

# Converting matrix to a tibble and changing column names
all_wts <- tk_tbl(all_wts)

colnames(all_wts) <- colnames(log_ret)

# Combing all the values together
portfolio_values <- tk_tbl(cbind(all_wts, portfolio_values))

portfolio_values = portfolio_values[order(portfolio_values$Return, decreasing = T),]

min_var <- portfolio_values[which.min(portfolio_values$Risk),]
max_sr <- portfolio_values[which.max(portfolio_values$SharpeRatio),]


#####
win.graph()
portfolio_values %>%
  ggplot(aes(x = Risk, y = Return, color = SharpeRatio)) +
  geom_point() +
  theme_classic() +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(labels = scales::percent) +
  labs(x = 'Risk',
       y = 'Returns',
       title = "Portfolio Optimization & Efficient Frontier Sebelum - Saat Pandemi") +
  geom_point(aes(x = Risk,
                 y = Return), data = min_var, color = 'green') +
  geom_point(aes(x = Risk,
                 y = Return), data = max_sr, color = 'red')

min_var
max_sr
head(portfolio_values)
write.csv(portfolio_values, file = "C:\\Users\\Hasan Zein\\Downloads\\portfolio_values Sebelum -  Saat.csv")

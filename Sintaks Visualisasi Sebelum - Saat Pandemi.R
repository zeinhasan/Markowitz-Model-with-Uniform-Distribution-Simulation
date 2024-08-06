### Data sebelum
# Read data
price_data2 <- read.delim("clipboard")
colnames(price_data2)

# Make Time Series Data
library(lubridate)
ts = ts(price_data2[-1], freq=365.25/7, start=decimal_date(ymd("2019-01-01")))

# Plot TS
require(graphics)
win.graph()
ts.plot(ts,gpars=list(xlab="year", ylab="Price", lty=1,
                      col = c("red","blue","green",
                              "black","yellow","orange")),main="Plot Harga Saham Sebelum - Saat Pandemi")
legend("topright",legend =  c("BBCA","BBRI","BMRI","ASII","HMSP","UNVR"), 
       lty = 1, col = c("red","blue","green","black","yellow","orange"), 
       merge = TRUE, cex=.9)

# Log return function
BBCA_Ret2 <- diff(log(price_data2$BBCA), lag=1)
BBRI_Ret2 <- diff(log(price_data2$BBRI), lag=1)
BMRI_Ret2 <- diff(log(price_data2$BMRI), lag=1)
ASII_Ret2 <- diff(log(price_data2$ASII), lag=1)
HMSP_Ret2 <- diff(log(price_data2$HMSP), lag=1)
UNVR_Ret2 <- diff(log(price_data2$UNVR), lag=1)

# Make dataframe
log_ret2 = data.frame(BBCA_Ret2,BBRI_Ret2,BMRI_Ret2,ASII_Ret2,HMSP_Ret2,UNVR_Ret2)
log_ret2
# Mean Return
mean_ret2 <- colMeans(log_ret2)
print(round(mean_ret2, 5))

# Covar return
cov_mat2 <- cov(log_ret2)
print(round(cov_mat2,5))

var(log_ret2)

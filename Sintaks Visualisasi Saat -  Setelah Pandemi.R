### Data sebelum
# Read data
price_data3 <- read.delim("clipboard")
colnames(price_data3)

# Make Time Series Data
library(lubridate)
ts2 = ts(price_data3[-1], freq=365.25/7, start=decimal_date(ymd("2021-01-01")))

# Plot TS
require(graphics)
win.graph()

ts.plot(ts2,gpars=list(xlab="year", ylab="Price", lty=1,
                      col = c("red","blue","green",
                              "black","yellow","orange")),main="Plot Harga Saham Saat - Sesudah Pandemi")
legend("topright",legend =  c("BBCA","BBRI","BMRI","ASII","HMSP","UNVR"), 
       lty = 1, col = c("red","blue","green","black","yellow","orange"), 
       merge = TRUE, cex=.9)

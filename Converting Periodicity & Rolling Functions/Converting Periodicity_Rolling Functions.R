library(xts) 
library(zoo)

df = read.csv("F:/df.csv",header = TRUE, stringsAsFactors = FALSE) 
date <- seq(as.Date("2021-03-01"), length = 396, by = "days") 
data = df$Value 
# transform the data frame into time series object
df = xts(x=data,order.by=date)

df_week = to.period(df,period = "weeks")  
head(df_week)

#without OHLC
df_week_2 = to.period(df,period = "weeks",OHLC = FALSE) 
head(df_week_2)

#--------------------------
#Applying Rolling functions
#--------------------------


#a monthly cumulative sum of a time series
df_monthly = split(df,f="months") 
df_cumsum = lapply(df_monthly,FUN = cumsum) > head(df_cumsum)

#standard deviation of the same time series data after every 3 units of period
df_sd <- rollapply(df, width=3, FUN = sd) # width = the number of periods after the sd is calculated. 
head(df_sd)

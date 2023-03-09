library(zoo) 
library(xts) 
Sys.setenv(TZ="Asia/Dhaka") 

# Hourly time series 
timeseries <- seq(from=as.POSIXct("2010-01-13 00:00:00 BST"), to=as.POSIXct("2010-03-13 23:59:59 BST"), by="hour") 
value <- rnorm(n=length(timeseries)) 
irreg <- xts(value, order.by = timeseries)
head(irreg)

# Converting as Matrix
data_matrix = as.matrix(coredata(irreg))
# Again converting as Dataframe
as.data.frame(data_matrix)

value <- rnorm(n=row(df))
df$newColumn = value

irreg <- xts(x = df, order.by = timeseries)

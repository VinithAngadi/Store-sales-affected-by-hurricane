library(expsmooth)
library(forecast)


data_csv <- read.csv('sales_data.csv')

store_data_csv <- data_csv['Store.data']
county_data_csv <- data_csv['County.data']

# Convert data frame to time series format 
store_data_ts <- ts(store_data_csv[1:52,], frequency = 12, start = c(2012, 9))
store_data_ts

county_data_ts_plot <- ts(county_data_csv[1:52,], frequency = 12, start = c(2012, 9))
county_data_ts_plot

county_data_ts <- ts(county_data_csv[1:48,], frequency = 12, start = c(2012, 9))
county_data_ts

# Forecast store sales using exponential smoothing

ets_fit_store <- ets(store_data_ts)

summary(ets_fit_store)

pred_store_sales_without_hurricane <- forecast(ets_fit_store, h = 4)

pred_store_sales_without_hurricane

plot(store_data_ts, col="blue", xlab="Year", ylab="Sales (in $100,000)", main="Store Sales Forecast without hurricane", type='l')
lines(pred_store_sales_without_hurricane$mean, col = 'red', lwd = 2)


# Forecast county sales without hurricane

ets_fit_county <- ets(county_data_ts)
ets_fit_county

pred_county_without_hurricane <- forecast(ets_fit_county, h = 4)
pred_county_without_hurricane

plot(county_data_ts_plot, col="blue", xlab="Year", ylab="Sales", main="County Sales Forecast", type='l')
lines(pred_county_without_hurricane$mean, col = 'red', lwd = 2)


# Predict store sales with hurricane

county_sales_without_hurricane <- pred_county_without_hurricane$mean
county_sales_without_hurricane

store_sales_without_hurricane <- pred_store_sales_without_hurricane$mean
store_sales_without_hurricane

county_sales_with_hurricane <- ts(county_data_csv[49:52,], frequency = 12, start = c(2016, 9))
county_sales_with_hurricane


county_sales_ratio <- county_sales_with_hurricane/county_sales_without_hurricane
county_sales_ratio

store_sales_with_hurricane <- store_sales_without_hurricane * county_sales_ratio

store_sales_with_hurricane



#store sales forecast if there was no hurricane
store_sales_without_hurricane

#store sales forecast if shop made sales after the hurricane
store_sales_with_hurricane

plot(store_sales_without_hurricane, col="blue", xlab="Year", ylab="Sales", main="County Sales Forecast", type='l')
lines(store_sales_with_hurricane, col = 'red', lwd = 1)
legend(2016.67, 4.3, legend=c("Store sales with hurricane", "Store sales without hurricane"),
       col=c("red", "blue"), lty=1:2, cex=0.8)


# The shop would have made increased sales if it was functional after the hurricane as observed in the above values.
# Hence, Carlson stores is entitled to compensation for excess sales due to increased business activity after the storm.





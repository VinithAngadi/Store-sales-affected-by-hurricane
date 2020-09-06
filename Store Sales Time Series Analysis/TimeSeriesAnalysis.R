library(expsmooth)
library(forecast)

sales_data = read.csv('hw3.csv')
head(sales_data)
store_sales_df <- sales_data['Store.data']
county_sales_df <- sales_data['County.data']

store_data_ts = ts(store_sales_df[1:48,], frequency = 12, start = c(2012, 9))
store_data_ts


ets_fit_store = ets(store_data_ts)
ets_fit_store

store_sales_pred_without_hurricane = forecast(ets_fit_store, h = 4)
store_sales_pred_without_hurricane

plot(store_sales_pred_without_hurricane, main = 'Store sales forecast')

county_data_ts = ts(county_sales_df[1:48,], frequency = 12, start = c(2012, 9))
county_data_ts

ets_fit_county = ets(county_data_ts)
ets_fit_county

county_sales_pred_without_hurricane = forecast(ets_fit_county, h = 4)
county_sales_pred_without_hurricane

plot(county_sales_pred_without_hurricane, main = 'County sales forecast')


county_sales_without_hurricane = county_sales_pred_without_hurricane$mean
county_sales_with_hurricane = ts(county_sales_df[49:52,], frequency = 12, start = c(2016,9))


store_sales_without_hurricane = store_sales_pred_without_hurricane$mean

county_sales_ratio = county_sales_with_hurricane/county_sales_without_hurricane
county_sales_ratio

store_sales_with_hurricane = store_sales_without_hurricane * county_sales_ratio

store_sales_with_hurricane

store_sales_without_hurricane

pred = ts.union(store_sales_without_hurricane, store_sales_with_hurricane)
pred
plot(pred)

plot(store_sales_with_hurricane)
lines(store_sales_without_hurricane)

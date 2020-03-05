# Time-Series-Forecasting-Gold-Return
The aim of this project is to predict the monthly return of gold in 12 months of 2018 base on the LBMA's monthly gold prices from January 2002 to December 2018 by choosing an adequate ARIMA model.

## Table of contents
* [General info](#general-info)
* [Results](#results)

## General info
The data used in this report relates to the prices of gold. The price of gold is decided through trading in gold and also its derivatives markets. The London Gold Fix which establishes in September 1919 listed a daily benchmark price for gold. The London Bullion Market Association (LBMA) Gold Price which sprang from March 2015 to replace the London Gold Fix (London, 2019). The price is set twice daily, morning and evening, in US dollars. The monthly evening close price from January 2002 to December 2018 (204 observed prices) is used for the analysis. The data is obtained from www.quandl.com. 
Training Sample: It consists of 192 time observations corresponding to 192 monthly data points from January 2002 to December 2017.
Testing Sample: It consists of 12 time observations corresponding to 12 monthly data points from January 2018 to December 2018.

## Results
Monthly price per one troy ounce of Gold in USD from January 2002 to December 2018.
![1](https://user-images.githubusercontent.com/56982400/76016937-baa62e80-5eeb-11ea-9330-3ad89cf00edb.jpeg)

In 2008, the financial crisis had large impact on the world's economy, however, it set stage for the gold to rise for the next few years. The outliers are not considered extreme; they can be replaced by potential replacements values which are suggested by a special function that detect outliers in R.
![2](https://user-images.githubusercontent.com/56982400/76017344-71a2aa00-5eec-11ea-9b80-6f470154b3b5.jpeg)

Monthly return of gold from January 2002 to December 2017 (Training Sample).
![3](https://user-images.githubusercontent.com/56982400/76017471-a878c000-5eec-11ea-8965-d7c7a4a1d6c1.jpeg)

Decomposition of the monthly return of gold time serie.
![4](https://user-images.githubusercontent.com/56982400/76017596-dd851280-5eec-11ea-9ef8-af76b6027780.jpeg)

Density histogram of the residuals (red) and the density curve of the normal distribution with the same mean and variance (blue line).
![8](https://user-images.githubusercontent.com/56982400/76017661-f55c9680-5eec-11ea-82f3-5df7aec78435.jpeg)

Quantile-Quantile plot: Residuals from ARIMA (0,0,2) Model with a 45-degree reference line.
![6](https://user-images.githubusercontent.com/56982400/76017709-0a392a00-5eed-11ea-9464-1775a048dc7f.jpeg)

Forecasts from ARIMA(0,0,2) with non-zero mean (blue line), the testing sample of monthly gold return (dash line), the 80% forecast limits (blue), and the 90% forecast limits (gray).
![7](https://user-images.githubusercontent.com/56982400/76017788-2ccb4300-5eed-11ea-8477-7d231d4bf376.jpeg)

